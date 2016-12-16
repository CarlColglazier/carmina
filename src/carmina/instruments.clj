(ns carmina.instruments
  (:use
   [overtone.live]
   [overtone.inst.piano]
   )
  (:require [overtone.libs.event :as e]))

(defrecord SynthState
    [filter-freq])

(defonce fffreq (atom 440))
(def synth-state (ref (SynthState. 440)))
(def memory (agent {}))

(on-event [:midi :note-on]
          (fn [m]
            (send memory
                  (fn [mem]
                    (let [n (:note m)
                          v (:velocity m)
                          filter-freq (swap! fffreq (fn [e] (:filter-freq @synth-state)))
                          s (saw-one :note n :velocity v :filter-freq filter-freq)]
                      (assoc mem n s)))))
          ::play-note)

(on-event [:midi :note-off]
          (fn [m]
            (send memory
                  (fn [mem]
                    (let [n (:note m)]
                      (when-let [s (get mem n)]
                        (ctl s :gate 0))
                      (dissoc mem n))))
            )
          ::release-note)

(on-event [:midi :control-change]
          (fn [e]
            (let [control-number (:data1 e)]
              (case control-number
                3 (dosync (alter synth-state assoc-in [:filter-freq] (* 25 (:velocity e)))
                          (ctl saw-one :filter-freq (* 25 (:velocity e)))
                          )
              )))
          ::controller)

