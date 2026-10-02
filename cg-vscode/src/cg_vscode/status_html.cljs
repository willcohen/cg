;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-vscode.status-html
  "The page of the CG Run Status tab, from the shared run status. It has no
   script: the host sets it again on each change."
  (:require ["../../../cg-app/shared/squint/src/cg_gui/run_status.mjs" :as rs]))

(defn- esc [s]
  (-> (str (if (some? s) s ""))
      (.replaceAll "&" "&amp;")
      (.replaceAll "<" "&lt;")
      (.replaceAll ">" "&gt;")
      (.replaceAll "\"" "&quot;")))

(defn- join-some [parts sep]
  (.join (.filter parts (fn [p] (and (some? p) (not= "" p)))) sep))

(defn- now-block [^js v]
  (str "<section class=\"now\" data-kind=\"" (esc (aget v "kind")) "\">"
       "<div class=\"head\"><span class=\"dot\"></span><span class=\"title\">"
       (esc (aget v "title")) "</span>"
       (when (aget v "clock")
         (str "<span class=\"clock\">Elapsed " (esc (aget v "clock")) "</span>"))
       "</div>"
       (when (some? (aget v "total"))
         (str "<progress max=\"" (aget v "total") "\" value=\"" (or (aget v "n") 0) "\"></progress>"))
       (when (aget v "detail")
         (str "<p class=\"detail\">" (esc (aget v "detail")) "</p>"))
       "</section>"))

(defn- step-li [^js r]
  (let [st (aget r "state")]
    (str "<li data-state=\"" (esc st) "\"><span class=\"dot\"></span>"
         "<span class=\"label\">" (esc (aget r "label")) "</span>"
         "<span class=\"state\">" (esc (if (= "error" st) "failed" st)) "</span>"
         "<span class=\"time\">" (esc (aget r "time")) "</span>"
         "<span class=\"more\">"
         (esc (join-some #js [(aget r "count") (aget r "rows") (aget r "queued")] " · "))
         "</span></li>")))

(defn- worker-li [^js w]
  (let [busy? (= "busy" (aget w "state"))
        jobs (aget w "jobs")]
    (str "<li data-state=\"" (esc (aget w "state")) "\"><span class=\"dot\"></span>"
         "<span class=\"label\">" (esc (aget w "name"))
         (when (and (aget w "handler") (not= "main" (aget w "handler")))
           (str " <span class=\"tag\">" (esc (aget w "handler")) "</span>"))
         "</span>"
         "<span class=\"state\">" (esc (aget w "state")) "</span>"
         "<span class=\"more\">"
         (esc (if busy?
                (join-some #js [(aget w "label")
                                (when (aget w "step") (str "for step " (aget w "step")))]
                           " · ")
                (str jobs (if (= 1 jobs) " job done" " jobs done"))))
         "</span></li>")))

(defn- flow-li [^js f]
  (let [st (aget f "state")]
    (str "<li data-state=\"" (esc st) "\"><span class=\"dot\"></span>"
         "<span class=\"label\">" (esc (aget f "label")) "</span>"
         "<span class=\"state\">" (if (= "error" st) "failed" "done") "</span>"
         "<span class=\"time\">" (esc (aget f "time")) "</span>"
         "<span class=\"more\">" (esc (aget f "detail")) "</span></li>")))

(def ^:private style
  (str "body{font-family:var(--vscode-font-family);font-size:13px;color:var(--vscode-foreground);"
       "background:var(--vscode-editor-background);padding:8px 16px;}"
       "h3{font-size:11px;text-transform:uppercase;letter-spacing:.04em;margin:16px 0 6px;"
       "color:var(--vscode-descriptionForeground);font-weight:600;}"
       ".of{text-transform:none;letter-spacing:0;font-weight:400;}"
       ".head{display:flex;align-items:center;gap:8px;}.title{font-weight:600;}"
       ".clock{margin-left:auto;color:var(--vscode-descriptionForeground);font-variant-numeric:tabular-nums;}"
       "progress{width:100%;margin-top:6px;}.detail{margin:6px 0 0;color:var(--vscode-descriptionForeground);}"
       "ol,ul{list-style:none;margin:0;padding:0;}"
       "li{display:grid;grid-template-columns:12px minmax(0,1fr) auto auto;gap:2px 8px;align-items:baseline;padding:3px 0;}"
       "li .more{grid-column:2/5;color:var(--vscode-descriptionForeground);font-size:12px;}"
       "li .more:empty{display:none;}.state,.time{color:var(--vscode-descriptionForeground);font-size:12px;}"
       ".dot{display:inline-block;width:8px;height:8px;border-radius:50%;background:var(--vscode-descriptionForeground);opacity:.5;}"
       "[data-state=running] .dot,[data-state=busy] .dot,[data-kind=running] .dot{background:var(--vscode-charts-blue);opacity:1;}"
       "[data-state=complete] .dot,[data-state=done] .dot,[data-kind=ok] .dot{background:var(--vscode-charts-green);opacity:1;}"
       "[data-state=error] .dot,[data-kind=error] .dot,[data-kind=refused] .dot{background:var(--vscode-errorForeground);opacity:1;}"
       ".tag{font-size:11px;padding:0 4px;border-radius:3px;background:var(--vscode-badge-background);"
       "color:var(--vscode-badge-foreground);}"
       ".empty{color:var(--vscode-descriptionForeground);}"))

(defn body-html
  "The body of the page for the run status `run` at `now` (ms)."
  [run now]
  (let [v (rs/run-view run nil now)
        running? (and v (= "running" (aget v "kind")))
        steps (rs/step-rows run now)
        workers (rs/worker-cells run)
        flows (rs/flow-rows run)
        label (rs/current-flow-label run)]
    (str (if v
           (now-block v)
           "<p class=\"empty\">Nothing runs. Click Run on the canvas, and this tab shows what the run does: its stage, the steps of each flow, and the workers.</p>")
         (when (or running? (pos? (.-length steps)))
           (str "<section><h3>Steps" (when label (str " <span class=\"of\">of " (esc label) "</span>")) "</h3>"
                (if (pos? (.-length steps))
                  (str "<ol>" (.join (.map steps step-li) "") "</ol>")
                  "<p class=\"empty\">No flow runs at this moment.</p>")
                "</section>"))
         (when (pos? (.-length workers))
           (str "<section><h3>Workers</h3><ul>" (.join (.map workers worker-li) "") "</ul></section>"))
         (when (pos? (.-length flows))
           (str "<section><h3>Flows that ended</h3><ol>" (.join (.map flows flow-li) "") "</ol></section>")))))

(defn build-html
  "The page. `nonce` allows the inline style sheet."
  [run now nonce]
  (str "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"UTF-8\">"
       "<meta http-equiv=\"Content-Security-Policy\" content=\"default-src 'none'; style-src 'nonce-" nonce "';\">"
       "<style nonce=\"" nonce "\">" style "</style></head><body>"
       (body-html run now)
       "</body></html>"))
