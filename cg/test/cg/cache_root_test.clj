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
            [cg.test-runner :refer [with-system-property]]
            [cg.util :as util]))

(set! *warn-on-reflection* true)

(defn- with-cache-dir-property [v f]
  (with-system-property "cg.cache.dir" v f))

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
