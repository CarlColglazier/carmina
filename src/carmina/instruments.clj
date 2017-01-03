(ns carmina.instruments
  (:use
   [overtone.live]
   [overtone.inst.drum]
   )
  (:require [overtone.libs.event :as e]))

(def metro (metronome 72))
((metro) now)
(defn looper [metro]
  (let [now (metro)]
    (at (metro now) (kick2))
    (at (metro (+ 0.25 now)) (closed-hat2))
    (at (metro (+ 0.5 now)) (snare))
    (at (metro (+ 0.75 now)) (closed-hat2))
    (at (metro (+ 1.5 now)) (snare))
                                        ;(apply-by (metro (+ 2 now)) looper metro [])))
    ))

(looper metro)

(defrecord SynthState
    [attack decay sustain release filter-freq resonance])

(def synth-state (ref (SynthState.
                       0.1 1.0 1.0 0.1 440 0.2)))
(def memory (agent {}))
(definst saw-one [note 60
                  velocity 100
                  gate 1
                  filter-freq 440
                  attack 0.1
                  decay 1.0
                  sustain 1.0
                  release 0.1
                  resonance 0.2]
  (let [
        env (env-gen (adsr attack decay sustain release) :gate gate :action FREE)
        freq (midicps note)
        amp (/ velocity 127)]
    (pan2 (rlpf (* amp env (saw freq)) filter-freq resonance))))

(on-event [:midi :note-on]
          (fn [m]
            (send memory
                  (fn [mem]
                    (let [n (:note m)
                          v (:velocity m)
                          filter-freq (:filter-freq @synth-state)
                          resonance (:resonance @synth-state)
                          attack (:attack @synth-state)
                          decay (:decay @synth-state)
                          sustain (:sustain @synth-state)
                          release (:release @synth-state)
                          s (saw-one :note n :velocity v :filter-freq filter-freq
                                     :attack attack :decay decay :sustain sustain
                                     :release release :resonance resonance)]
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

(defn alter-synth [control value state inst]
  (dosync (alter state assoc-in [control] value)
          (ctl inst control value)))

(on-event [:midi :control-change]
          (fn [e]
            (let [control-number (:data1 e)]
              (case control-number
                1 (alter-synth :filter-freq (* 25 (:velocity e)) synth-state saw-one)
                9 (alter-synth :resonance (/ (:velocity e) 127) synth-state saw-one)
                16 (alter-synth :attack (/ (:velocity e) 127) synth-state saw-one)
                17 (alter-synth :decay (/ (:velocity e) 127) synth-state saw-one)
                18 (alter-synth :sustain (/ (:velocity e) 127) synth-state saw-one)
                19 (alter-synth :release (/ (:velocity e) 127) synth-state saw-one)
              )))
          ::controller)
(kill saw-one)
(saw-one)
(event-debug-on)
