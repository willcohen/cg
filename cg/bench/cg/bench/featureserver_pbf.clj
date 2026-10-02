;; Copyright (c) 2019, 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.bench.featureserver-pbf
  "Measure the f=pbf fetch path against GDAL's ESRIJSON driver on a live
   server.

   The end-to-end ratio is not a property of the code. This path waits on the
   server for about 93% of its time, so the ratio moves with server load. Any
   figure quoted from this benchmark needs its date, and needs a new run before
   it is quoted again.

   Two modes:

     e2e     (default) time a whole load through each loader, order alternated
     split   time fetch and decode apart, to attribute the difference

   Both loaders must read the same rows at the same page size, or the comparison
   means nothing. The GDAL loader runs :fetch-format :json and the PBF loader runs
   :pbf-strict, so a silent fallback throws instead of being timed as PBF."
  (:require [cg.featureserver-pbf :as pbf]
            [cg.io :as io]
            [net.willcohen.gdal.network :as gdal-network]))

(set! *warn-on-reflection* true)

;; The bench has no default layer, because each server and layer needs its
;; own --where bound on OBJECTID to read a fixed number of rows: ask the
;; server for the minimum OBJECTID through outStatistics, then add the row
;; count you want.

(def default-page-size 2000)

;; The split mode fetches pages itself, so it must know how many to ask for.
;; The e2e mode does not: the loop reads the transfer-limit flag.
(def default-pages 10)

;; The split reuses cg.io's own URL builder and page fetch, both private, so
;; it measures the shipped code and not a copy that could drift from it.
(def ^:private build-url #'cg.io/build-geoservices-query-url)
(def ^:private fetch-page #'cg.io/fetch-geoservices-page)

(defn- ms-since ^long [^long t0]
  (Math/round (/ (double (- (System/nanoTime) t0)) 1e6)))

(def ^:private wire (atom {:requests 0 :bytes 0}))

(defn- install-transport-counter!
  "Count every request and every response byte of both loaders.

   GDAL's driver and the PBF loop both fetch through this one var, so a wrapper
   here gives the payload totals without a second measurement. The root value
   changes rather than a binding, because GDAL calls back on its own thread."
  []
  (let [base gdal-network/*transport*]
    (alter-var-root
     #'gdal-network/*transport*
     (fn [_]
       (fn counting-transport [request]
         (let [resp (base request)
               n    (if-let [^bytes b (:body-bytes resp)] (alength b) 0)]
           (swap! wire (fn [w] (-> w (update :requests inc) (update :bytes + n))))
           resp))))))

(defn- run-lane [lane {:keys [layer where page-size]}]
  (reset! wire {:requests 0 :bytes 0})
  (let [t0     (System/nanoTime)
        result (io/load-gdal-source layer
                                    :where where
                                    :result-record-count page-size
                                    :fetch-format (case lane :gdal :json :pbf :pbf-strict))
        ms     (ms-since t0)
        w      @wire]
    {:lane lane :ms ms :rows (count (:rows result)) :crs (:source-crs result)
     :requests (:requests w) :bytes (:bytes w)}))

(defn- median [xs]
  (let [s (sort xs)] (nth s (quot (count s) 2))))

(defn- report-e2e [results]
  (println)
  (println "=== summary ===")
  (doseq [lane [:gdal :pbf]]
    (let [rs (filter #(= lane (:lane %)) results)
          ms (sort (map :ms rs))]
      (println (format "%-5s  n=%d  min=%d  median=%d  max=%d  bytes=%d"
                       (name lane) (count ms) (first ms) (median ms) (last ms)
                       (long (:bytes (first rs)))))))
  (println (format "median speedup: %.2fx"
                   (double (/ (median (map :ms (filter #(= :gdal (:lane %)) results)))
                              (median (map :ms (filter #(= :pbf (:lane %)) results)))))))
  (println "row counts:" (distinct (map :rows results)))
  (println "crs agreement:" (= 1 (count (distinct (map :crs results))))))

(defn run-e2e
  "Time a whole load through each loader, alternating which loader runs first.

   The loader that runs second gains from the server having just produced those
   rows, so a fixed order would credit that gain to one loader."
  [{:keys [runs] :as cfg}]
  (install-transport-counter!)
  (let [results (doall
                 (for [i (range (* 2 (long runs)))
                       :let [order (if (even? i) [:gdal :pbf] [:pbf :gdal])]
                       lane order]
                   (let [r (run-lane lane cfg)]
                     (println (format "pair %d  %-4s  first=%-4s  %6d ms  %6d rows  %2d req  %9d bytes"
                                      (inc i) (name lane) (name (first order))
                                      (:ms r) (:rows r) (:requests r) (:bytes r)))
                     (flush)
                     r)))]
    (report-e2e results)))

(defn- fetch-all
  "Fetch `pages` pages of one wire format. Returns [elapsed-ms bytes bodies]."
  [wire-format {:keys [layer where page-size pages]}]
  (let [url (fn page-url [offset]
              (build-url layer {:where where :result-record-count page-size
                                :wire-format wire-format :result-offset offset}))
        t0 (System/nanoTime)
        bodies (doall
                (for [p (range (long pages))
                      :let [offset (* (long p) (long page-size))]]
                  (if (= wire-format "pbf")
                    (fetch-page (url offset) {})
                    (:body-bytes (gdal-network/*transport* {:url (url offset)})))))]
    [(ms-since t0) (reduce + (map alength bodies)) bodies]))

(defn- decode-all [bodies {:keys [page-size]} opts]
  (let [t0 (System/nanoTime)
        n  (reduce + (for [[i b] (map-indexed vector bodies)]
                       (count (:rows (pbf/decode-page
                                      b (assoc opts :fid-offset (* (long i) (long page-size))))))))]
    [(ms-since t0) n]))

(defn run-split
  "Time fetch and decode apart.

   GDAL owns fetch and parse together, so read its parse cost as the whole
   driver load from e2e mode minus the JSON fetch reported here."
  [{:keys [runs] :as cfg}]
  ;; One untimed pass first, or the first trial reports scanner JIT warm-up.
  (let [[_ _ warm] (fetch-all "pbf" cfg)]
    (decode-all (take 1 warm) cfg {}))
  (fetch-all "json" cfg)
  (dotimes [trial (long runs)]
    (let [[pbf-ms pbf-bytes bodies] (fetch-all "pbf" cfg)
          [dec-ms rows]             (decode-all bodies cfg {})
          [raw-ms _]                (decode-all bodies cfg {:raw-dates? true :preserve-nulls? true})
          [json-ms json-bytes _]    (fetch-all "json" cfg)]
      (println (format "trial %d" (inc trial)))
      (println (format "  pbf  fetch  %5d ms   %9d bytes" pbf-ms pbf-bytes))
      (println (format "  pbf  decode %5d ms   %6d rows   (defaults)" dec-ms rows))
      (println (format "  pbf  decode %5d ms                  (:raw-dates? :preserve-nulls?)" raw-ms))
      (println (format "  json fetch  %5d ms   %9d bytes" json-ms json-bytes))
      (println (format "  payload %.2fx smaller   fetch %.2fx faster"
                       (double (/ json-bytes pbf-bytes))
                       (double (/ json-ms (max 1 pbf-ms)))))
      (flush))))

(defn- parse-args [args]
  (reduce (fn [m [k v]]
            (case k
              "--layer"     (assoc m :layer v)
              "--where"     (assoc m :where v)
              "--page-size" (assoc m :page-size (Long/parseLong v))
              "--pages"     (assoc m :pages (Long/parseLong v))
              "--runs"      (assoc m :runs (Long/parseLong v))
              m))
          {:page-size default-page-size :pages default-pages :runs 3}
          (partition 2 args)))

(defn -main [& args]
  (let [mode (if (#{"split" "e2e"} (first args)) (first args) "e2e")
        cfg  (parse-args (if (#{"split" "e2e"} (first args)) (rest args) args))]
    (when-not (and (:layer cfg) (:where cfg))
      (println "usage: bb bench:pbf [e2e|split] --layer URL --where CLAUSE [--page-size N] [--pages N] [--runs N]")
      (System/exit 2))
    (println "mode:" mode)
    (println "layer:" (:layer cfg))
    (println "where:" (:where cfg) " page-size:" (:page-size cfg) " runs:" (:runs cfg))
    (println)
    (if (= mode "split") (run-split cfg) (run-e2e cfg))
    (shutdown-agents)
    (System/exit 0)))
