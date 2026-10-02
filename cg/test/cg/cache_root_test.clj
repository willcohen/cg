;; Copyright (c) 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.cache-root-test
  "cache-root: the cg.cache.dir system property, else CG_CACHE_DIR, else
   $XDG_CACHE_HOME/cg, else ~/.cache/cg. The tests pass the environment as a
   map."
  (:require [clojure.test :refer [deftest is]]
            [cg.util :as util]))

(set! *warn-on-reflection* true)

(defn with-cache-dir-property
  "Call f with the cg.cache.dir system property set to v (nil clears it),
   then put the old value back."
  [v f]
  (let [old (System/getProperty "cg.cache.dir")]
    (try
      (if v
        (System/setProperty "cg.cache.dir" v)
        (System/clearProperty "cg.cache.dir"))
      (f)
      (finally
        (if old
          (System/setProperty "cg.cache.dir" old)
          (System/clearProperty "cg.cache.dir"))))))

(deftest the-home-cache-is-the-default
  (with-cache-dir-property nil
    #(is (= (str (System/getProperty "user.home") "/.cache/cg")
            (util/cache-root {})))))

(deftest xdg-cache-home-comes-before-the-home-cache
  (with-cache-dir-property nil
    #(is (= "/x/cg" (util/cache-root {"XDG_CACHE_HOME" "/x"})))))

(deftest cg-cache-dir-comes-before-xdg
  (with-cache-dir-property nil
    #(is (= "/c" (util/cache-root {"CG_CACHE_DIR" "/c" "XDG_CACHE_HOME" "/x"})))))

(deftest the-property-comes-first
  (with-cache-dir-property "/p"
    #(is (= "/p" (util/cache-root {"CG_CACHE_DIR" "/c"})))))

(deftest an-empty-value-is-not-set
  (with-cache-dir-property ""
    #(is (= "/x/cg" (util/cache-root {"CG_CACHE_DIR" "" "XDG_CACHE_HOME" "/x"})))))
