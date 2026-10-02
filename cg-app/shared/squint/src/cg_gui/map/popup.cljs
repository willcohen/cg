;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.map.popup
  "The card of a clicked map feature, from the :popup of a map-layer spec.
   The spec names the columns; this namespace knows no column of a module.")

;; The popup spec, all keys optional, each column a property name:
;;   title      a column, or an array of columns joined with a space
;;   status     a column whose value is a tag under the title
;;   highlight  the status value of a rule that the feature does not meet
;;   rules      an array of #js {label actual min|max status unit}: a value,
;;              its limit, and the status column of the rule
;;   facts      an array of a column or #js {label value unit format}
;; With no popup spec, the card shows the first fields of the feature.

(def missing "—")

(def ^:private default-fact-count 8)

(defn- plain
  "A property name with no leading colon. A JVM keyword column has one."
  [s]
  (if (string? s) (.replace s (js/RegExp. "^:") "") s))

(defn prop
  "The value of the column `k` in `props`, with or without the colon."
  [props k]
  (when (and props (some? k))
    (let [bare (plain (str k))]
      (if (some? (aget props bare))
        (aget props bare)
        (aget props (str ":" bare))))))

(defn humanize
  "A column name or a value as display text: no colon, no final question
   mark, spaces for dashes and underscores, an upper-case first letter."
  [s]
  (let [t (-> (plain (str s))
              (.replace (js/RegExp. "\\?$") "")
              (.replace (js/RegExp. "[-_]+" "g") " ")
              (.trim))]
    (str (.toUpperCase (.charAt t 0)) (.slice t 1))))

(defn- capitalize-run [w]
  (str (.charAt w 0) (.toLowerCase (.slice w 1))))

(defn display-text
  "Text in upper case, as many source tables store a name, in title case.
   Other text, and a short code such as \"R1\", comes back with no change."
  [s]
  (let [t (str s)]
    (if (and (not (.test (js/RegExp. "[a-z]") t))
             (.test (js/RegExp. "[A-Z]{4}|[A-Z]\\S*\\s+\\S*[A-Z]") t))
      (.replace t (js/RegExp. "[A-Z]+" "g") capitalize-run)
      t)))

(defn format-number
  "A number as display text: no decimals for an integer or from 100, one from 1,
   two below 1. `grouped?` adds thousands separators, as does a value from 10,000."
  [v grouped?]
  (let [a       (js/Math.abs v)
        digits  (cond (js/Number.isInteger v) 0
                      (>= a 100) 0
                      (>= a 1) 1
                      :else 2)
        rounded (js/Number (.toFixed v digits))
        text    (.toLocaleString (js/Math.abs rounded) "en-US"
                                 #js {:maximumFractionDigits digits
                                      :useGrouping (or (true? grouped?)
                                                       (>= (js/Math.abs rounded) 10000))})]
    (str (when (neg? rounded) "−") text)))

(defn- with-unit [text unit]
  (if (nil? unit) text (str text " " unit)))

(defn- percent-text [v]
  (str (format-number (js/Math.round v) false) "%"))

(defn format-value
  "A property value as display text, or nil for no value. `fmt` \"percent\"
   shows a ratio as a percent; the unit \"%\" marks a value that is one."
  [v unit fmt]
  (cond
    (or (nil? v) (= "" v)) nil
    (and (number? v) (js/Number.isNaN v)) nil
    (and (number? v) (= "percent" (plain fmt))) (percent-text (* 100 v))
    (and (number? v) (= "%" unit)) (percent-text v)
    (number? v) (with-unit (format-number v (some? unit)) unit)
    (boolean? v) (if v "Yes" "No")
    :else (with-unit (display-text v) unit)))

(defn- difference-text
  "The signed difference of `actual` from `limit`, or nil."
  [limit actual unit]
  (when (and (number? limit) (number? actual))
    (let [d    (- actual limit)
          text (format-number (js/Math.abs d) (some? unit))]
      (with-unit (cond (= "0" text) text
                       (neg? d) (str "−" text)
                       :else (str "+" text))
                 unit))))

(defn- failed?
  "True when the rule is not met: its status is the highlight value, or,
   with no status column, the value is past its limit."
  [dir limit actual status highlight]
  (if (some? status)
    (= (plain (str status)) (plain (str highlight)))
    (and (number? limit) (number? actual)
         (if (= "min" dir) (< actual limit) (> actual limit)))))

(defn- value-color
  "The layer color of the value `v`, or nil."
  [colors v]
  (when (and colors (some? v))
    (aget colors (plain (str v)))))

(defn- rule-row [props rule popup colors]
  (let [dir        (if (some? (aget rule "min")) "min" "max")
        limit      (prop props (aget rule dir))
        actual     (prop props (aget rule "actual"))
        status     (prop props (aget rule "status"))
        unit       (aget rule "unit")
        failed     (failed? dir limit actual status (aget popup "highlight"))
        limit-text (format-value limit unit nil)]
    #js {:label      (or (aget rule "label") (humanize (aget rule "actual")))
         :required   (if limit-text (str dir " " limit-text) missing)
         :actual     (or (format-value actual unit nil) missing)
         :difference (or (difference-text limit actual unit) missing)
         :failed     failed
         :color      (value-color colors (if (some? status)
                                           status
                                           (when failed (aget popup "highlight"))))}))

(defn rules-summary
  "\"2 of 8 rules not met\", \"All 8 rules met\", or nil when a rule has no
   difference and none failed."
  [rows]
  (let [n      (.-length rows)
        failed (.-length (.filter rows (fn [r] (aget r "failed"))))
        blank  (.-length (.filter rows (fn [r] (= missing (aget r "difference")))))]
    (cond
      (pos? failed) (str failed " of " n " rules not met")
      (and (pos? n) (zero? blank)) (str "All " n " rules met")
      :else nil)))

(defn- fact-row [props fact]
  (let [spec (if (string? fact) #js {:value fact} fact)
        col  (aget spec "value")
        text (format-value (prop props col) (aget spec "unit") (aget spec "format"))]
    (when text
      #js {:label (or (aget spec "label") (humanize col)) :value text})))

(defn- title-text
  "The values of the title columns, joined, or nil when none has a value."
  [props title]
  (let [cols  (if (js/Array.isArray title) title #js [title])
        parts (.filter (.map cols (fn [c] (prop props c)))
                       (fn [v] (not (or (nil? v) (= "" v) (= 0 v)))))]
    (when (pos? (.-length parts))
      (display-text (.join parts " ")))))

(defn- field-rows [props]
  (.map (js/Object.keys props)
        (fn [k] #js {:label (plain k)
                     :value (or (format-value (aget props k) nil nil) missing)})))

(defn- default-facts [props]
  (.slice (.filter (.map (field-rows props) (fn [r] (fact-row props (aget r "label"))))
                   some?)
          0 default-fact-count))

(defn card-model
  "The display data of the card of one feature: #js {layer title status color
   summary rules facts fields}. `popup` and `colors` can be nil."
  [var-name props popup colors]
  (let [props  (or props #js {})
        p      (or popup #js {})
        status (prop props (aget p "status"))
        rules  (.map (or (aget p "rules") #js []) (fn [r] (rule-row props r p colors)))]
    #js {:layer   var-name
         :title   (or (when (aget p "title") (title-text props (aget p "title")))
                      var-name)
         :status  (when (some? status) (humanize status))
         :color   (value-color colors status)
         :summary (rules-summary rules)
         :rules   rules
         :facts   (cond
                    (aget p "facts") (.filter (.map (aget p "facts") (fn [f] (fact-row props f)))
                                              some?)
                    popup #js []
                    :else (default-facts props))
         :fields  (field-rows props)}))

(defn- el [tag class-name text]
  (let [e (js/document.createElement tag)]
    (when class-name (set! (.-className e) class-name))
    (when (some? text) (set! (.-textContent e) text))
    e))

(defn- append! [parent children]
  (.forEach children (fn [c] (when c (.appendChild parent c))))
  parent)

(defn- tone!
  "Gives `e` the color of a value, for the CSS of the card."
  [e color]
  (when color (.setProperty (.-style e) "--cg-card-tone" color))
  e)

(defn- head-element [model]
  (let [tag (when (aget model "status")
              (tone! (el "span" "cg-card-tag" (aget model "status")) (aget model "color")))]
    (append! (el "header" "cg-card-head" nil)
             #js [(when (not= (aget model "title") (aget model "layer"))
                    (el "div" "cg-card-layer" (aget model "layer")))
                  (el "div" "cg-card-title" (aget model "title"))
                  (when (or tag (aget model "summary"))
                    (append! (el "div" "cg-card-status" nil)
                             #js [tag
                                  (when (aget model "summary")
                                    (el "span" "cg-card-summary" (aget model "summary")))]))])))

(defn- rule-element [row]
  (doto (append! (el "tr" nil nil)
                 #js [(el "th" nil (aget row "label"))
                      (el "td" nil (aget row "required"))
                      (el "td" nil (aget row "actual"))
                      (el "td" "cg-card-diff" (aget row "difference"))])
    (tone! (aget row "color"))
    (.setAttribute "data-failed" (str (true? (aget row "failed"))))))

(defn- rules-element [rows]
  (when (pos? (.-length rows))
    (let [head (append! (el "tr" nil nil)
                        (.map #js ["Rule" "Required" "Actual" "Difference"]
                              (fn [h] (el "th" nil h))))]
      (append! (el "table" "cg-card-rules" nil)
               #js [(append! (el "thead" nil nil) #js [head])
                    (append! (el "tbody" nil nil) (.map rows rule-element))]))))

(defn- pairs-element [class-name rows]
  (when (pos? (.-length rows))
    (append! (el "dl" class-name nil)
             (.flatMap rows (fn [r] #js [(el "dt" nil (aget r "label"))
                                         (el "dd" nil (aget r "value"))])))))

(defn- fields-element [rows]
  (when (pos? (.-length rows))
    (append! (el "details" "cg-card-all" nil)
             #js [(el "summary" nil (str "All fields (" (.-length rows) ")"))
                  (pairs-element "cg-card-fields" rows)])))

(defn card-element
  "The DOM of a card-model. It uses createElement and textContent, not
   innerHTML, because the CSP of the cg-vscode webview forbids it."
  [model]
  (doto (append! (el "article" "cg-card" nil)
                 #js [(head-element model)
                      (rules-element (aget model "rules"))
                      (pairs-element "cg-card-facts" (aget model "facts"))
                      (fields-element (aget model "fields"))])
    (.setAttribute "data-layer" (aget model "layer"))))

;; The cg-app shell defines --cg-* and the cg-vscode webview --vscode-*; the
;; last fallback is a light theme.
(def ^:private card-css
  ".cg-card{font-size:12px;line-height:1.35}
.cg-card+.cg-card{margin-top:10px;padding-top:10px;border-top:1px solid var(--cg-line-soft,var(--vscode-panel-border,#cbd2d9))}
.cg-card-layer{font-size:11px;color:var(--cg-ink-muted,var(--vscode-descriptionForeground,#52606d))}
.cg-card-title{font-size:14px;font-weight:600}
.cg-card-status{display:flex;align-items:center;gap:8px;margin-top:4px}
.cg-card-tag{padding:1px 8px;border-radius:10px;font-weight:600;color:#fff;background:var(--cg-card-tone,#525252)}
.cg-card-summary{color:var(--cg-ink-muted,var(--vscode-descriptionForeground,#52606d))}
.cg-card-rules{width:100%;margin-top:8px;border-collapse:collapse;font-variant-numeric:tabular-nums}
.cg-card-rules th,.cg-card-rules td{padding:2px 4px;text-align:right;white-space:nowrap}
.cg-card-rules th:first-child{text-align:left;font-weight:400}
.cg-card-rules thead th{font-size:11px;font-weight:600;color:var(--cg-ink-muted,var(--vscode-descriptionForeground,#52606d))}
.cg-card-rules tr[data-failed=true]{box-shadow:inset 3px 0 0 var(--cg-card-tone,#eb6200);background:color-mix(in srgb,var(--cg-card-tone,#eb6200) 14%,transparent)}
.cg-card-rules tr[data-failed=true] .cg-card-diff{font-weight:700}
.cg-card-facts,.cg-card-fields{display:grid;grid-template-columns:auto 1fr;gap:2px 10px;margin:8px 0 0}
.cg-card-facts dt,.cg-card-fields dt{color:var(--cg-ink-muted,var(--vscode-descriptionForeground,#52606d))}
.cg-card-facts dd,.cg-card-fields dd{margin:0;overflow-wrap:anywhere}
.cg-card-all{margin-top:8px}
.cg-card-all summary{cursor:pointer;color:var(--cg-ink-muted,var(--vscode-descriptionForeground,#52606d))}")

(defn install-css!
  "Adds the style of the card to the document, one time."
  []
  (when-not (js/document.getElementById "cg-card-css")
    (let [s (el "style" nil card-css)]
      (set! (.-id s) "cg-card-css")
      (.appendChild js/document.head s))))
