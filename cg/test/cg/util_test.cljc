;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.util-test
  "The cg.util fns that a .cg model calls: deep-merge, log, warn and error."
  #?(:clj (:require [clojure.string :as str]
                    [clojure.test :refer [deftest is]]
                    [cg.util :as util])
     :cljs (:require [clojure.string :as str]
                     [cljs.test :refer [deftest is]]
                     ["../../src/cg/util.mjs" :as util]
                     ["./test_runner.mjs" :refer [run_tests_and_exit_BANG_]])))

#?(:clj (set! *warn-on-reflection* true))

;; A bonus table: the rules of each district, with an override of one rule.
(deftest deep-merge-keeps-the-keys-that-an-override-does-not-name
  (is (= {"RD-2" {:front 15.0 :side 3.0 :coverage {:steps [0.25]}}
          "RD-3" {:front 12.0 :side 5.0}}
         (util/deep-merge {"RD-2" {:front 15.0 :side 5.0 :coverage {:steps [0.25]}}
                           "RD-3" {:front 12.0 :side 5.0}}
                          {"RD-2" {:side 3.0}}))))

(defn- streams
  "What `f` writes to the output and to the error stream."
  [f]
  #?(:clj (let [err (java.io.StringWriter.)
                out (with-out-str (binding [*err* err] (f)))]
            [out (str err)])
     :cljs (let [out (atom [])
                 err (atom [])
                 saved [(.-log js/console) (.-warn js/console) (.-error js/console)]
                 to (fn [lines] (fn [& args] (swap! lines conj (str/join " " args))))]
             (set! (.-log js/console) (to out))
             (set! (.-warn js/console) (to err))
             (set! (.-error js/console) (to err))
             (try (f)
                  (finally
                    (set! (.-log js/console) (nth saved 0))
                    (set! (.-warn js/console) (nth saved 1))
                    (set! (.-error js/console) (nth saved 2))))
             [(str/join "\n" @out) (str/join "\n" @err)])))

(deftest log-writes-to-the-output-and-warn-and-error-to-the-error-stream
  (let [[out err] (streams #(util/log "parcels" 3))]
    (is (str/includes? out "parcels 3"))
    (is (= "" err)))
  (doseq [f [util/warn util/error]]
    (let [[out err] (streams #(f "no frontage" "p-7"))]
      (is (= "" out))
      (is (str/includes? err "no frontage p-7")))))

#?(:cljs (run_tests_and_exit_BANG_ "cg.util-test"))
