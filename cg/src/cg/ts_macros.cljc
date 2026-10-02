;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

;; Macro layer for cg.geo. It reads the wasmts registry at expand time
;; (through ts_registry.cljc, which `bb ts:sync` keeps in step) and mints one
;; defn for each [class, method, shape-kind] group of in-scope entries.
;;
;; Call shapes:
;;   - A group of one entry with 0 or 1 args gives a positional defn:
;;     (geometry-get-area feat), (geometry-buffer feat a0).
;;   - Any other group gives an opts-map defn. The keys are the kebab
;;     :param-names of the registry, and the provided keys select the overload:
;;     (geometry-union geom {:other other-geom}).
;;   - A constructor is <kebab-class>-create. A group with a no-arg variant
;;     also gets a 0-arity.
;;
;; Each minted defn registers under "geo.<package>.<kebab-class>/<op-name>" in
;; the bare-op-fns map of cg.feature. It does not go in bare-op-namespaces:
;; that map is global and the last write wins, and minted names such as
;; "filter" or "buffer" would replace the cg.dataset registrations.
(ns cg.ts-macros
  (:require [cg.ts-registry :as tsr]
            [clojure.string :as str]))

#?(:clj (set! *warn-on-reflection* true))

(defn kebab
  "Convert camelCase or PascalCase to kebab-case. An acronym run stays one
   word: getSRID gives get-srid, XMLHttpRequest gives xml-http-request."
  [s]
  (-> s
      (str/replace #"([a-z\d])([A-Z])" "$1-$2")
      (str/replace #"([A-Z]+)([A-Z][a-z])" "$1-$2")
      (str/lower-case)))

(defn simple-name [class-fqn]
  (last (str/split class-fqn #"\.")))

(defn kebab-class
  "Convert the simple name of a class to kebab-case: BufferParameters gives
   buffer-parameters."
  [class-fqn]
  (kebab (simple-name class-fqn)))

(defn package-leaf
  "Return the package path under org.locationtech.jts: `geom` for Geometry,
   `geom.prep` for PreparedGeometry."
  [class-fqn]
  (->> (-> class-fqn
           (str/replace "org.locationtech.jts." "")
           (str/split #"\."))
       butlast
       (str/join ".")))

(defn method-op-name
  "Return the kebab op name of a registry method. A constructor (\"<init>\")
   is \"create\"."
  [method]
  (if (= method "<init>") "create" (kebab method)))

(defn defn-name
  "Return the defn symbol for a [class, method] group:
   <kebab-class>-<kebab-method-or-create>."
  [class method]
  (symbol (str (kebab-class class) "-" (method-op-name method))))

(def ^:private geometry-subtypes-set
  #{"org.locationtech.jts.geom.Geometry"
    "org.locationtech.jts.geom.Point"
    "org.locationtech.jts.geom.LineString"
    "org.locationtech.jts.geom.LinearRing"
    "org.locationtech.jts.geom.Polygon"
    "org.locationtech.jts.geom.MultiPoint"
    "org.locationtech.jts.geom.MultiLineString"
    "org.locationtech.jts.geom.MultiPolygon"
    "org.locationtech.jts.geom.GeometryCollection"})

(def geometry-subtypes
  "JTS class names that the wasmts codegen merges into one Geometry surface; their JS path
   drops the class name (wasmts.geom.buffer). cg.geo/geometry-typed? reads it at run time."
  geometry-subtypes-set)

(defn js-path
  "Return the wasmts JS path of an entry, or `hint` when there is one. A Geometry subtype
   drops the class name, another class keeps its simple name, a constructor is createN (arity N)."
  [{:keys [class method params]} hint]
  (or hint
      (let [pkg (package-leaf class)
            m   (if (= method "<init>") (str "create" (count params)) method)]
        (if (geometry-subtypes-set class)
          (str "wasmts." pkg "." m)
          (str "wasmts." pkg "." (simple-name class) "." m)))))

;; The data is in a cljc file because the squint macro evaluator runs under SCI, which has no slurp.
(defn load-registry
  "Return the registry data that `bb ts:sync` writes into src/cg/ts_registry.cljc."
  []
  tsr/registry)

(defn receiver-call? [[_ v]] (= :receiver-call (:kind (:shape v))))
(defn static-call?   [[_ v]] (= :static-call   (:kind (:shape v))))
(defn ctor?          [[_ v]] (= :ctor          (:kind (:shape v))))

(defn in-scope?
  "Return true for an entry with a shape that this macro can dispatch.
   :bespoke entries and bare-keyword shape tags are out of scope."
  [entry]
  (or (receiver-call? entry) (static-call? entry) (ctor? entry)))

(defn in-scope-entries
  "Return the registry entries with a known shape kind, sorted to keep the
   codegen stable."
  [registry]
  (->> registry
       (filter in-scope?)
       (sort-by (fn [[k _]] [(:class k) (:method k) (count (:params k))]))))

(defn param-keyword
  "Return the kebab-case keyword of a registry :param-names string:
   quadrantSegments gives :quadrant-segments."
  [pname]
  (keyword (kebab pname)))

(defn ^:private fallback-param-names
  "Return the names a0, a1, ... for an entry whose :param-names is absent or
   has the wrong length."
  [n]
  (mapv (fn [i] (str "a" i)) (range n)))

(defn effective-param-names
  "Return the param names of the entry, or the fallback names when they are
   absent or do not match the arity."
  [k v]
  (let [arity (count (:params k))
        names (:param-names v)]
    (if (= arity (count names))
      (vec names)
      (fallback-param-names arity))))

(defn entry-meta-literal
  "Return the meta map of an entry, which the dispatcher reads at run time."
  [k v]
  {:class       (:class k)
   :method      (:method k)
   :params      (:params k)
   :param-names (effective-param-names k v)
   :static?     (:static? v)
   :returns     (:returns v)
   :js-path     (js-path k (:js-path v))
   :shape       (:shape v)})

(defn entry-keys
  "Return the kebab keywords of the positional params of an entry, in :params
   order. The set of the keywords selects an overload at the call site."
  [k v]
  (mapv param-keyword (effective-param-names k v)))

(defn map-dispatch?
  "Return true when a group uses opts-map dispatch: a group of 2 or more
   entries, or one entry with 2 or more args."
  [entries]
  (or (> (count entries) 1)
      (>= (count (:params (ffirst entries))) 2)))

#?(:clj
   (do
     (def ^:private jvm-primitive-casts
       {"boolean" 'clojure.core/boolean "byte" 'clojure.core/byte
        "char" 'clojure.core/char "short" 'clojure.core/short
        "int" 'clojure.core/int "long" 'clojure.core/long
        "float" 'clojure.core/float "double" 'clojure.core/double})

     (def ^:private jvm-primitive-array-tags
       {"boolean[]" 'booleans "byte[]" 'bytes "char[]" 'chars "short[]" 'shorts
        "int[]" 'ints "long[]" 'longs "float[]" 'floats "double[]" 'doubles})

     ;; Fully qualified: the expanding namespace may not import the class.
     (defn- jvm-tag
       "The :tag for a registry type name: a fully qualified class name, or the JVM descriptor
        of an object array (\"[Lorg...Coordinate;\")."
       [t]
       (cond
         (jvm-primitive-array-tags t) (jvm-primitive-array-tags t)
         (str/ends-with? t "[]")
         (let [elem (str/replace t "[]" "")
               dims (quot (- (count t) (count elem)) 2)]
           (when (jvm-primitive-casts elem)
             (throw (ex-info "No :tag for a primitive array of 2 or more dimensions"
                             {:type t})))
           (str (apply str (repeat dims "[")) "L" elem ";"))
         :else (symbol t)))

     ;; A static type on each argument lets the compiler select one overload.
     (defn- jvm-arg
       "Argument `i` of `args`, cast to its primitive type `t` or tagged with its class."
       [args i t]
       (let [a (list 'clojure.core/nth args i)]
         (if-let [cast (jvm-primitive-casts t)]
           (list cast a)
           (with-meta a {:tag (jvm-tag t)}))))

     ;; cg code passes a prepared geometry as the receiver of a Geometry op; on JS,
     ;; invoke-instance-js finds the prepared function by name.
     (def ^:private jvm-prepared-methods
       "The [method params] of each PreparedGeometry receiver call in the registry."
       (into #{}
             (keep (fn [[k v]]
                     (when (and (= "org.locationtech.jts.geom.prep.PreparedGeometry" (:class k))
                                (= :receiver-call (-> v :shape :kind)))
                       [(:method k) (:params k)])))
             (load-registry)))

     ;; A Geometry receiver call that PreparedGeometry also has gets both calls, and the receiver class picks one.
     (defn- jvm-invoker
       "A fn form that calls the method of registry key `k` directly. A receiver call takes [receiver args],
        a static call or a constructor takes [args], in :params order."
       [{:keys [class method params]} kind]
       (let [args      (gensym "args")
             r         (gensym "r")
             call-args (map-indexed #(jvm-arg args %1 %2) params)
             klass     (symbol class)
             call      (fn [receiver] `(. ~receiver (~(symbol method) ~@call-args)))]
         (case kind
           :receiver-call
           (if (and (geometry-subtypes-set class) (jvm-prepared-methods [method params]))
             `(fn [~r ~args]
                (if (instance? org.locationtech.jts.geom.prep.PreparedGeometry ~r)
                  ~(call (with-meta r {:tag 'org.locationtech.jts.geom.prep.PreparedGeometry}))
                  ~(call (with-meta r {:tag klass}))))
             `(fn [~(with-meta r {:tag klass}) ~args] ~(call r)))
           :static-call   `(fn [~args] (. ~klass (~(symbol method) ~@call-args)))
           :ctor          `(fn [~args] (new ~klass ~@call-args)))))))

(defn- invoker-args
  "The invoker argument of a dispatcher call for entry [k v]: a direct-call fn
   when `jvm?`, else nothing."
  [jvm? [k v]]
  (if jvm?
    #?(:clj [(jvm-invoker k (-> v :shape :kind))]
       :cljs (throw (ex-info "No JVM invoker under squint"
                             {:class (:class k) :kind (-> v :shape :kind)})))
    []))

(defn- dispatch-call
  "The call of `dispatcher` for entry [k v]: the entry's meta literal, its
   invoker when `jvm?`, then `args`."
  [jvm? dispatcher [k v :as entry] & args]
  `(~dispatcher '~(entry-meta-literal k v) ~@(invoker-args jvm? entry) ~@args))

(defn emit-positional-body
  "Return the [argv body] of a positional group (1 entry, 0 or 1 args)."
  [jvm? [k _ :as entry] kind dispatcher]
  (let [arg-syms (vec (for [i (range (count (:params k)))]
                        (symbol (str "a" i))))
        feat-sym (gensym "feat")]
    (case kind
      :receiver-call `([~feat-sym ~@arg-syms]
                       ~(dispatch-call jvm? dispatcher entry feat-sym arg-syms))
      :static-call   `([~@arg-syms]
                       ~(dispatch-call jvm? dispatcher entry arg-syms))
      :ctor          `([~@arg-syms]
                       ~(dispatch-call jvm? dispatcher entry arg-syms)))))

(defn ^:private dedup-by-key-set
  "Keep the first of the entries that share a kebab key-set, as Area.ofRing(Coordinate[]) and
   Area.ofRing(CoordinateSequence) do. Both runtimes call only the kept overload."
  [entries]
  (->> entries
       (reduce (fn [{:keys [seen kept]} [k v :as entry]]
                 (let [ks (set (entry-keys k v))]
                   (if (contains? seen ks)
                     {:seen seen :kept kept}
                     {:seen (conj seen ks) :kept (conj kept entry)})))
               {:seen #{} :kept []})
       :kept))

(defn ^:private emit-map-clause
  "Return a cond clause for one entry: the test (= provided #{keys}) and the
   dispatcher call with the args in registry order."
  [jvm? opts-sym provided-sym feat-sym [k v :as entry] kind dispatcher]
  (let [keys-vec  (entry-keys k v)
        key-set   (set keys-vec)
        arg-exprs (mapv (fn [kw] `(get ~opts-sym ~kw)) keys-vec)]
    (case kind
      :receiver-call [`(= ~provided-sym ~key-set)
                      (dispatch-call jvm? dispatcher entry feat-sym arg-exprs)]
      :static-call   [`(= ~provided-sym ~key-set)
                      (dispatch-call jvm? dispatcher entry arg-exprs)]
      :ctor          [`(= ~provided-sym ~key-set)
                      (dispatch-call jvm? dispatcher entry arg-exprs)])))

(defn emit-map-bodies
  "Return 1 or 2 [argv body] forms for a map-dispatch group: an opts arity that selects the entry
   by (set (keys opts)), and a 0-arity when the group has a no-arg entry."
  [jvm? fname entries kind dispatcher]
  (let [unique-entries (dedup-by-key-set entries)
        has-zero?      (some (fn [[k _]] (zero? (count (:params k))))
                             unique-entries)
        opts-sym       (gensym "opts")
        feat-sym       (gensym "feat")
        provided-sym   (gensym "provided")
        clauses        (mapcat #(emit-map-clause jvm? opts-sym provided-sym
                                                 feat-sym % kind dispatcher)
                               unique-entries)
        available-key-sets (mapv (fn [[k v]] (set (entry-keys k v)))
                                 unique-entries)
        cond-body
        `(let [~provided-sym (set (keys ~opts-sym))]
           (cond
             ~@clauses
             :else (throw (ex-info ~(str fname ": no overload matches provided keys")
                                   {:fn '~fname
                                    :provided ~provided-sym
                                    :available ~available-key-sets}))))]
    (case kind
      :receiver-call
      (if has-zero?
        [`([~feat-sym] (~fname ~feat-sym {}))
         `([~feat-sym ~opts-sym] ~cond-body)]
        [`([~feat-sym ~opts-sym] ~cond-body)])

      :static-call
      (if has-zero?
        [`([] (~fname {}))
         `([~opts-sym] ~cond-body)]
        [`([~opts-sym] ~cond-body)])

      :ctor
      (if has-zero?
        [`([] (~fname {}))
         `([~opts-sym] ~cond-body)]
        [`([~opts-sym] ~cond-body)]))))

(defn emit-group
  "Emit the defn and the register-bare-op-fn! call for one [class, method,
   kind] group. See map-dispatch? for the call shape."
  [jvm? [[class method _kind] entries] dispatchers]
  (let [fname      (defn-name class method)
        ns-key     (str "geo." (package-leaf class) "." (kebab-class class))
        op-name    (method-op-name method)
        kind       (-> entries first second :shape :kind)
        dispatcher (get dispatchers kind)
        bodies     (if (map-dispatch? entries)
                     (emit-map-bodies jvm? fname entries kind dispatcher)
                     [(emit-positional-body jvm? (first entries) kind dispatcher)])]
    [`(defn ~fname ~@bodies)
     `(cg.feature/register-bare-op-fn! ~ns-key ~op-name ~fname)]))

(defn group-key
  "Return [class, method, shape-kind] for an entry. The shape-kind is in the
   key because one defn must not mix receiver calls and static calls."
  [[k v]]
  [(:class k) (:method k) (:kind (:shape v))])

;; Each receiver-call op on a Geometry subtype also gets a bare promoted name
;; (see short-op-name), lifted over GeoInput and dispatched positionally.
;; These promoted names are the main cg.geo surface. The raw names stay.

;; A massage keyword names a cg.geo fn that wraps the dispatch call at emit
;; time, to give the result a better form. :coord-pairs (Coordinate[] to
;; [[x y] ...]) serves `coordinates`, `nearest-points` and `closest-points`.

(def ^:private massage-fns
  "Massage keyword -> qualified cg.geo fn symbol. The symbol is qualified
   because syntax-quote would resolve a bare one into cg.ts-macros."
  {:coord-pairs 'cg.geo/coords->pairs})

(def ^:private receiver-massages
  "Promoted receiver-op name -> massage keyword. A static op has its massage
   in hero-verb-aliases."
  {'coordinates :coord-pairs})

(defn ^:private wrap-massage
  "Wrap inner-form in the massage fn for `massage`. With no massage fn,
   return inner-form."
  [massage inner-form]
  (if-let [f (massage-fns massage)]
    `(~f ~inner-form)
    inner-form))

(defn ^:private bool-return? [v]
  (= "boolean" (simple-name (str (get-in v [:returns :type])))))

(defn short-op-name
  "Promoted short name of a Geometry method: get-X gives x, is-X gives x?, has-X gives has-x?,
   and another boolean op gets a `?` (equalsTopo gives equals-topo?)."
  [method v]
  (let [m    (kebab method)
        base (cond
               (str/starts-with? m "get-") (subs m 4)
               (str/starts-with? m "is-")  (str (subs m 3) "?")
               (str/starts-with? m "has-") (str m "?")
               :else m)]
    (if (and (bool-return? v) (not (str/ends-with? base "?")))
      (str base "?")
      base)))

(defn ^:private promoted-name-for
  "The bare promoted symbol for a receiver-call group."
  [[[_class method _kind] entries]]
  (symbol (short-op-name method (-> entries first second))))

(defn ^:private geom-receiver-groups
  "Groups that are receiver-call ops on a Geometry-subtype class."
  [groups]
  (filter (fn [[[class _ kind] _]]
            (and (= :receiver-call kind) (geometry-subtypes-set class)))
          groups))

;; Polygon.getBoundary and Geometry.getBoundary both promote to boundary. The base class
;; dispatch works for each subtype.
(defn ^:private dedup-geom-groups
  "Keep one group for each promoted name; the Geometry base class group wins."
  [geom-groups]
  (->> geom-groups
       (group-by promoted-name-for)
       vals
       (map (fn [gs]
              (or (first (filter #(= "org.locationtech.jts.geom.Geometry" (ffirst %))
                                 gs))
                  (first gs))))))

;; Qualified symbols: syntax-quote would resolve them into cg.ts-macros.
(defn ^:private emit-promoted-arity
  "Return one [argv body] that lifts the receiver over cg.geo/-geo-map and calls
   cg.geo/dispatch-receiver-call with a flat arg vector."
  [jvm? [k _ :as entry] col-name massage]
  (let [arg-syms (vec (for [i (range (count (:params k)))] (symbol (str "a" i))))
        x        (gensym "x")
        g        (gensym "g")]
    `([~x ~@arg-syms]
      (cg.geo/-geo-map ~x
                       (fn [~g] ~(wrap-massage massage
                                               (dispatch-call jvm? 'cg.geo/dispatch-receiver-call
                                                              entry g arg-syms)))
                       ~col-name))))

;; The Geometry surface has no same-arity overloads that differ only by type.
(defn ^:private promoted-receiver-forms
  "Return the promoted defn for a receiver-call group, with one arity for each param count."
  [jvm? [_gk entries :as group]]
  (let [pname    (promoted-name-for group)
        col-name (name pname)
        massage  (receiver-massages pname)
        by-arity (->> entries
                      (group-by (fn [[k _]] (count (:params k))))
                      (sort-by key)
                      (map (comp first val)))
        arities  (map #(emit-promoted-arity jvm? % col-name massage) by-arity)]
    [`(defn ~pname ~@arities)]))

;; A static op does not get a bare promoted name by rule, because the names
;; collide: `simplify` across three simplifier classes, `union` across
;; Geometry and the operation classes. Only this alias map mints a bare name
;; for a static op. The op must have a Geometry as param 0. It is lifted over
;; GeoInput and dispatched positionally with that geometry as arg 0.

;; Selector keys: :class :method name the static op. :massage is a massage-fns key for the result.
;; :const-args are literal tail args (an OverlayNG opcode), dropped from the promoted arity.
;; :fold adds a 1-arg arity that reduces the binary op over a collection. :variant
;; {:opt kw :alt-class fqn} adds an opts arity that calls alt-class when (get opts opt) is false.
;; :opts-arg names a helper whose (helper opts) gives the final param, with and without opts.
;; :prop-strategy adds an opts arity that sets result properties by :property-strategy.
(def hero-verb-aliases
  "Bare promoted name -> selector of a :static-call op whose param 0 is a Geometry. A name here
   wins over the Geometry-receiver promotion of that name, which stays as geometry-<op>."
  (let [overlay-ng-robust "org.locationtech.jts.operation.overlayng.OverlayNGRobust"]
    {'simplify                    {:class "org.locationtech.jts.simplify.TopologyPreservingSimplifier"
                                   :method "simplify"
                                   :variant {:opt :preserve-topology
                                             :alt-class "org.locationtech.jts.simplify.DouglasPeuckerSimplifier"}}
     'douglas-peucker-simplify    {:class "org.locationtech.jts.simplify.DouglasPeuckerSimplifier"
                                   :method "simplify"}
     'make-valid                  {:class "org.locationtech.jts.geom.util.GeometryFixer"
                                   :method "fix"}
     'get-minimum-area-rectangle  {:class "org.locationtech.jts.algorithm.MinimumAreaRectangle"
                                   :method "getMinimumRectangle"}
     'get-minimum-width-rectangle {:class "org.locationtech.jts.algorithm.MinimumDiameter"
                                   :method "getMinimumRectangle"}
     'nearest-points              {:class "org.locationtech.jts.operation.distance.DistanceOp"
                                   :method "nearestPoints"
                                   :massage :coord-pairs}
     'closest-points              {:class "org.locationtech.jts.operation.distance.DistanceOp"
                                   :method "closestPoints"
                                   :massage :coord-pairs}
     ;; OverlayNGRobust.overlay(g1, g2, opcode) with the JTS OverlayNG opcodes:
     ;; INTERSECTION 1, UNION 2, DIFFERENCE 3, SYMDIFFERENCE 4.
     'intersection                {:class overlay-ng-robust :method "overlay" :const-args [1] :prop-strategy true}
     'union                       {:class overlay-ng-robust :method "overlay" :const-args [2] :fold true :prop-strategy true}
     'difference                  {:class overlay-ng-robust :method "overlay" :const-args [3]}
     'sym-difference              {:class overlay-ng-robust :method "overlay" :const-args [4]}
     ;; The 2-arg form uses the defaults, as Geometry.buffer(distance) does.
     'buffer                      {:class "org.locationtech.jts.operation.buffer.BufferOp"
                                   :method "bufferOp"
                                   :opts-arg 'cg.geo/buffer-params}}))

(defn ^:private emit-hero-static-arity
  "Return one [argv body] for a hero alias that lifts param 0 over cg.geo/-geo-map and calls
   cg.geo/dispatch-static-call with the other params, then the const-args."
  [jvm? [k _ :as entry] col-name massage const-args]
  (let [n-extra    (- (count (:params k)) 1 (count const-args))
        extra-syms (vec (for [i (range 1 (inc n-extra))] (symbol (str "a" i))))
        x          (gensym "x")
        g          (gensym "g")]
    `([~x ~@extra-syms]
      (cg.geo/-geo-map ~x
                       (fn [~g] ~(wrap-massage massage
                                               (dispatch-call jvm? 'cg.geo/dispatch-static-call
                                                              entry (into [g] (concat extra-syms const-args)))))
                       ~col-name))))

(defn ^:private emit-hero-fold-arity
  "Return the [coll] arity of a :fold hero: reduce the binary op over the features of `coll`.
   The accumulator keeps the properties of the first feature."
  [jvm? entry const-args]
  (let [coll (gensym "coll")
        fs   (gensym "fs")
        acc  (gensym "acc")
        f    (gensym "f")]
    `([~coll]
      (let [~fs (vec ~coll)]
        (reduce (fn [~acc ~f]
                  ~(dispatch-call jvm? 'cg.geo/dispatch-static-call
                                  entry (into [acc f] const-args)))
                (first ~fs) (rest ~fs))))))

(defn ^:private emit-hero-variant-arity
  "Return the [x extra... opts] arity of a :variant hero: the default op when
   (get opts opt-key true) is true, else the same entry on alt-class."
  [jvm? [k v :as entry] col-name massage const-args {:keys [opt alt-class]}]
  (let [n-extra    (- (count (:params k)) 1 (count const-args))
        extra-syms (vec (for [i (range 1 (inc n-extra))] (symbol (str "a" i))))
        alt        [(assoc k :class alt-class) (dissoc v :js-path)]
        x          (gensym "x")
        g          (gensym "g")
        opts       (gensym "opts")
        args       (into [g] (concat extra-syms const-args))]
    `([~x ~@extra-syms ~opts]
      (cg.geo/-geo-map
       ~x
       (fn [~g]
         ~(wrap-massage massage
                        `(if (get ~opts ~opt true)
                           ~(dispatch-call jvm? 'cg.geo/dispatch-static-call entry args)
                           ~(dispatch-call jvm? 'cg.geo/dispatch-static-call alt args))))
       ~col-name))))

(defn ^:private emit-hero-opts-arities
  "Return the two arities of an :opts-arg hero, where (helper opts) gives the final param.
   The base arity calls the helper with {}."
  [jvm? [k _ :as entry] col-name massage helper]
  (let [n-user    (- (count (:params k)) 2)   ; minus the anchor and the helper tail
        user-syms (vec (for [i (range 1 (inc n-user))] (symbol (str "a" i))))
        x         (gensym "x")
        g         (gensym "g")
        opts      (gensym "opts")
        body      (fn [opts-form]
                    `(cg.geo/-geo-map
                      ~x
                      (fn [~g] ~(wrap-massage massage
                                              (dispatch-call jvm? 'cg.geo/dispatch-static-call
                                                             entry (conj (into [g] user-syms)
                                                                         (list helper opts-form)))))
                      ~col-name))]
    [`([~x ~@user-syms] ~(body {}))
     `([~x ~@user-syms ~opts] ~(body opts))]))

(defn ^:private emit-hero-prop-strategy-arity
  "Return the [x extra... opts] arity of a :prop-strategy hero, which sets the result properties
   by (get opts :property-strategy :keep-first) over the two input features."
  [jvm? [k _ :as entry] col-name massage const-args]
  (let [n-extra    (- (count (:params k)) 1 (count const-args))
        extra-syms (vec (for [i (range 1 (inc n-extra))] (symbol (str "a" i))))
        x          (gensym "x")
        g          (gensym "g")
        opts       (gensym "opts")]
    `([~x ~@extra-syms ~opts]
      (cg.geo/apply-prop-strategy
       (cg.geo/-geo-map
        ~x
        (fn [~g] ~(wrap-massage massage
                                (dispatch-call jvm? 'cg.geo/dispatch-static-call
                                               entry (into [g] (concat extra-syms const-args)))))
        ~col-name)
       ~x ~(first extra-syms)
       (get ~opts :property-strategy :keep-first)))))

(defn ^:private hero-static-group-for
  "Return the entries of the [class method :static-call] group for a selector
   whose param 0 is a Geometry subtype, or nil when there are none."
  [groups {:keys [class method]}]
  (some (fn [[[c m kind] entries]]
          (when (and (= c class) (= m method) (= kind :static-call))
            (let [anchored (filter (fn [[k _]]
                                     (geometry-subtypes-set (first (:params k))))
                                   entries)]
              (when (seq anchored) (vec anchored)))))
        groups))

(defn ^:private hero-static-forms
  "Emit one promoted defn for each hero alias, with one arity for each param
   count of the selected group. An alias with no matching group is skipped."
  [jvm? groups]
  (into []
        (for [[pname selector] (sort-by key hero-verb-aliases)
              :let [entries (hero-static-group-for groups selector)]
              :when entries
              :let [col-name   (name pname)
                    massage    (:massage selector)
                    const-args (:const-args selector)
                    by-arity   (->> entries
                                    (group-by (fn [[k _]] (count (:params k))))
                                    (sort-by key)
                                    (map (comp first val)))
                    base       (if-let [helper (:opts-arg selector)]
                                 (mapcat #(emit-hero-opts-arities jvm? % col-name massage helper) by-arity)
                                 (map #(emit-hero-static-arity jvm? % col-name massage const-args) by-arity))
                    folds      (when (:fold selector)
                                 (map #(emit-hero-fold-arity jvm? % const-args) by-arity))
                    variants   (when-let [variant (:variant selector)]
                                 (map #(emit-hero-variant-arity jvm? % col-name massage const-args variant) by-arity))
                    props      (when (:prop-strategy selector)
                                 (map #(emit-hero-prop-strategy-arity jvm? % col-name massage const-args) by-arity))
                    arities    (concat base folds variants props)]]
          `(defn ~pname ~@arities))))

(defn emit-all
  "Return a vector of the raw <class>-<method> defns, the promoted receiver ops and the hero aliases.
   With `jvm?`, each dispatcher call also gets the jvm-invoker of the entry."
  ([registry dispatchers] (emit-all registry dispatchers false))
  ([registry dispatchers jvm?]
   (let [entries  (in-scope-entries registry)
         groups   (->> entries
                       (group-by group-key)
                       (sort-by key))
         raw      (into [] (mapcat #(emit-group jvm? % dispatchers) groups))
         promoted (into [] (mapcat #(promoted-receiver-forms jvm? %)
                                   ;; A hero alias wins over the Geometry-receiver
                                   ;; promotion of the same name.
                                   (remove #(hero-verb-aliases (promoted-name-for %))
                                           (dedup-geom-groups (geom-receiver-groups groups)))))
         heroes   (hero-static-forms jvm? groups)]
     (-> raw (into promoted) (into heroes)))))

(defmacro define-all-ts-fns
  "Expand to a `(do ...)` of defn and register-bare-op-fn! forms for each in-scope registry entry.
   The defns call the dispatchers of the calling namespace. `:jvm` makes each call take a direct invoker."
  [& [platform]]
  (let [registry (load-registry)
        forms    (emit-all registry
                           {:receiver-call 'dispatch-receiver-call
                            :static-call   'dispatch-static-call
                            :ctor          'dispatch-ctor}
                           (= :jvm platform))]
    `(do ~@forms)))

(defn check-data
  "Return a map that describes what `define-all-ts-fns` emits.
   cg.geo/check-report reads its keys."
  []
  (let [registry  (load-registry)
        entries   (in-scope-entries registry)
        groups    (group-by group-key entries)
        by-pkg    (group-by (fn [[k _]]
                              (str "geo." (package-leaf (:class k))
                                   "." (kebab-class (:class k))))
                            entries)
        by-shape  (->> entries
                       (group-by (fn [[_ v]] (:kind (:shape v))))
                       (map (fn [[k vs]] [k (count vs)]))
                       (into (sorted-map)))]
    {:total-entries  (count registry)
     :minted         (count groups)
     :registered     (count groups)
     :minted-arities (count entries)
     :by-shape       by-shape
     :by-package     (into (sorted-map)
                           (map (fn [[k vs]] [k (count vs)]) by-pkg))}))
