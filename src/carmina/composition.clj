(ns carmina.composition
   (:require [clojure.math.numeric-tower :as math]))

;; 1/f noise
(defn roll-change [number die-count]
  "We want to reroll the die each time that the bit the die represents flips.
   To do this, check if the iterated value divided by 2 times the position
   changes."
  (let [n (mod number (math/expt 2 die-count))]
    (for [x (range 0 die-count)
          :let
          [y (if (not=
                  (quot n (math/expt 2 x))
                  (quot (mod (dec' n) (math/expt 2 die-count)) (math/expt 2 x)))
             true
             false)]] y)))

(defn- roll [x]
  "Iterate a set of dice to the next count and rerolls where needed."
  {:count (inc (x :count))
   :die-count (x :die-count)
   :sides (x :sides)
   :die (let [n (roll-change (x :count) (x :die-count))]
          (for [i (range 0 (x :die-count))]
            (if (true? (nth n i))
              (rand-int (x :sides))
              (nth (x :die) i)
              )))})

(defn one-f-noise [die-count sides]
  "Generates a list of integers using 1/f noise."
  (let [len (math/expt 2 die-count)
        n (take len
                (iterate roll { :count 0
                               :die-count die-count
                               :die (make-array Integer/TYPE die-count)
                               :sides sides
                               } ))]
    (for [i (range 0 len)]
      (reduce + (into [] ((nth n i) :die))))))

;; stochastic
(defn triangular-min [max-num]
  "Triangular distrubution biased toward zero"
  (min (rand-int max-num) (rand-int max-num)))

(defn triangular-max [max-num]
  "Triangular distrubution biased toward the maximum"
  (max (rand-int max-num) (rand-int max-num)))

(defn triangular [max-num]
  "Triangular distrubution biased toward the mean"
  (quot (+ (rand-int max-num) (rand-int max-num)) 2))
