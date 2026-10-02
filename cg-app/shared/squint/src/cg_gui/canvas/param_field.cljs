;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg-gui.canvas.param-field
  "The boxes of a parameter chip, one for each scalar of the literal. Enter or
   blur posts SetLiteral. Escape, or text of the wrong kind, keeps the old value."
  (:require ["react" :refer [useState useEffect useRef]]
            [cg-gui.canvas.host :as host]
            [cg-gui.doc.literal :as literal]))

(defn- key-label
  "The text of a map key next to its box: a keyword without its colon."
  [k]
  (let [s (str k)]
    (if (.startsWith s ":") (.slice s 1) s)))

(defn- post-literal!
  [param-name ^js field src]
  (host/post! #js {:type "cg/edit"
                   :editType "SetLiteral"
                   :model nil
                   :payload #js {:name param-name
                                 :location (aget field "location")
                                 :was (aget field "raw")
                                 :text src}}))

(defn- stop [^js e] (.stopPropagation e))

(defn- refusal
  [kind]
  (if (= kind "nil")
    "Type a number, a keyword, a string in quotes, true, false or nil"
    (str "Not a " kind ": the old value stays")))

(defn- BooleanBox [^js props]
  (let [field (.-field props)]
    #jsx [:select {:className "cg-field-input nodrag nopan"
                   :data-testid (.-testid props)
                   :value (str (aget field "raw"))
                   :onPointerDown stop
                   :onChange (fn [^js e]
                               ((.-onCommit props) field (.. e -target -value)))}
          [:option {:value "true"} "true"]
          [:option {:value "false"} "false"]]))

(defn- TextBox [^js props]
  (let [field (.-field props)
        kind  (str (aget field "kind"))
        raw   (str (aget field "raw"))
        shown (literal/field-text kind raw)
        [typed set-typed] (useState shown)
        ;; Escape blurs the box, and that blur must not commit.
        cancel (useRef false)
        src   (literal/field-source kind typed)
        bad?  (nil? src)
        settle! (fn []
                  (cond
                    (.-current cancel) (do (set! (.-current cancel) false)
                                           (set-typed shown))
                    (or bad? (= src raw)) (set-typed shown)
                    :else ((.-onCommit props) field src)))]
    (useEffect
     (fn []
       (set-typed shown)
       js/undefined)
     #js [field])
    #jsx [:input {:className (str "cg-field-input nodrag nopan kind-" kind)
                  :data-testid (.-testid props)
                  :value typed
                  :spellCheck false
                  :autoComplete "off"
                  :aria-invalid (if bad? "true" "false")
                  :title (if bad? (refusal kind) (str "Edit the " kind " in the file"))
                  :onPointerDown stop
                  :onClick stop
                  :onDoubleClick stop
                  :onChange (fn [^js e] (set-typed (.. e -target -value)))
                  :onKeyDown (fn [^js e]
                               (.stopPropagation e)
                               (cond
                                 (= "Enter" (.-key e)) (.blur (.-target e))
                                 (= "Escape" (.-key e)) (do (set! (.-current cancel) true)
                                                            (.blur (.-target e)))))
                  :onBlur (fn [_] (settle!))}]))

(defn ParamFields
  "props: :name (the def), :fields (the parser fields of its literal)."
  [^js props]
  (let [nm     (.-name props)
        fields (or (.-fields props) #js [])
        commit (fn [field src] (post-literal! nm field src))]
    #jsx [:div {:className "cg-fields" :data-testid (str "param-fields-" nm)}
          (.map fields
                (fn [^js f i]
                  (let [k      (aget f "key")
                        testid (str "param-field-" nm "-" (if k (key-label k) i))
                        Box    (if (= "boolean" (str (aget f "kind"))) BooleanBox TextBox)]
                    #jsx [:label {:className "cg-field" :key (str i)}
                          (when k
                            #jsx [:span {:className "cg-field-key"} (key-label k)])
                          [Box {:field f :testid testid :onCommit commit}]])))]))
