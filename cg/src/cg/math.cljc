;; Copyright (c) 2025, 2026 Will Cohen
;;
;; Part of cg, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns cg.math
  "Math functions for .cg files, with one API on the JVM (clojure.math and
  clojure.core/abs) and on JS (the global Math object)."
  (:refer-clojure :exclude [abs])
  #?(:clj (:require [clojure.math :as m])))

#?(:clj (set! *warn-on-reflection* true))

(defn sin
  "Returns the sine of an angle (in radians)."
  [x]
  #?(:clj (m/sin x)
     :cljs (js/Math.sin x)))

(defn cos
  "Returns the cosine of an angle (in radians)."
  [x]
  #?(:clj (m/cos x)
     :cljs (js/Math.cos x)))

(defn tan
  "Returns the tangent of an angle (in radians)."
  [x]
  #?(:clj (m/tan x)
     :cljs (js/Math.tan x)))

(defn asin
  "Returns the arc sine of x, in radians."
  [x]
  #?(:clj (m/asin x)
     :cljs (js/Math.asin x)))

(defn acos
  "Returns the arc cosine of x, in radians."
  [x]
  #?(:clj (m/acos x)
     :cljs (js/Math.acos x)))

(defn atan
  "Returns the arc tangent of x, in radians."
  [x]
  #?(:clj (m/atan x)
     :cljs (js/Math.atan x)))

(defn atan2
  "Returns the angle theta from the conversion of rectangular coordinates (x, y)
   to polar coordinates (r, theta). Result is in radians."
  [y x]
  #?(:clj (m/atan2 y x)
     :cljs (js/Math.atan2 y x)))

(defn sinh
  "Returns the hyperbolic sine of x."
  [x]
  #?(:clj (m/sinh x)
     :cljs (js/Math.sinh x)))

(defn cosh
  "Returns the hyperbolic cosine of x."
  [x]
  #?(:clj (m/cosh x)
     :cljs (js/Math.cosh x)))

(defn tanh
  "Returns the hyperbolic tangent of x."
  [x]
  #?(:clj (m/tanh x)
     :cljs (js/Math.tanh x)))

(defn to-degrees
  "Converts an angle from radians to degrees."
  [radians]
  #?(:clj (m/to-degrees radians)
     :cljs (* radians (/ 180 js/Math.PI))))

(defn to-radians
  "Converts an angle from degrees to radians."
  [degrees]
  #?(:clj (m/to-radians degrees)
     :cljs (* degrees (/ js/Math.PI 180))))

(defn abs
  "Returns the absolute value of x."
  [x]
  #?(:clj (clojure.core/abs x)
     :cljs (js/Math.abs x)))

(defn sqrt
  "Returns the square root of x."
  [x]
  #?(:clj (m/sqrt x)
     :cljs (js/Math.sqrt x)))

(defn cbrt
  "Returns the cube root of x."
  [x]
  #?(:clj (m/cbrt x)
     :cljs (js/Math.cbrt x)))

(defn pow
  "Returns x raised to the power of y."
  [x y]
  #?(:clj (m/pow x y)
     :cljs (js/Math.pow x y)))

(defn exp
  "Returns Euler's number e raised to the power of x."
  [x]
  #?(:clj (m/exp x)
     :cljs (js/Math.exp x)))

(defn expm1
  "Returns e^x - 1."
  [x]
  #?(:clj (m/expm1 x)
     :cljs (js/Math.expm1 x)))

(defn log
  "Returns the natural logarithm (base e) of x."
  [x]
  #?(:clj (m/log x)
     :cljs (js/Math.log x)))

(defn log10
  "Returns the base 10 logarithm of x."
  [x]
  #?(:clj (m/log10 x)
     :cljs (js/Math.log10 x)))

(defn log1p
  "Returns ln(1 + x)."
  [x]
  #?(:clj (m/log1p x)
     :cljs (js/Math.log1p x)))

(defn floor
  "Returns the largest integer less than or equal to x."
  [x]
  #?(:clj (m/floor x)
     :cljs (js/Math.floor x)))

(defn ceil
  "Returns the smallest integer greater than or equal to x."
  [x]
  #?(:clj (m/ceil x)
     :cljs (js/Math.ceil x)))

(defn round
  "Returns the closest integer to x."
  [x]
  #?(:clj (m/round x)
     :cljs (js/Math.round x)))

(defn rint
  "Returns the double value closest to x and equal to a mathematical integer."
  [x]
  #?(:clj (m/rint x)
     :cljs (js/Math.round x)))  ; JS has no rint. Math.round differs on a tie.

(defn signum
  "Returns the signum function of x: -1.0 if negative, 0.0 if zero, 1.0 if positive."
  [x]
  #?(:clj (m/signum x)
     :cljs (js/Math.sign x)))

(defn hypot
  "Returns sqrt(x^2 + y^2) without intermediate overflow or underflow."
  [x y]
  #?(:clj (m/hypot x y)
     :cljs (js/Math.hypot x y)))

(defn copy-sign
  "Returns a value with the magnitude of the first argument and the sign of the second."
  [magnitude sign]
  #?(:clj (m/copy-sign magnitude sign)
     :cljs (* (js/Math.abs magnitude) (js/Math.sign sign))))

(defn random
  "Returns a random double between 0.0 (inclusive) and 1.0 (exclusive)."
  []
  #?(:clj (m/random)
     :cljs (js/Math.random)))

(def PI
  "The ratio of the circumference of a circle to its diameter."
  #?(:clj m/PI
     :cljs js/Math.PI))

(def E
  "Euler's number, the base of natural logarithms."
  #?(:clj m/E
     :cljs js/Math.E))
