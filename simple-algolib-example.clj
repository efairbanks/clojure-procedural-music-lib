(load-file "./algolib.clj")

(def bpm (* 110 1))
(def clk (metronome bpm))

(defn play-beat [clock tick]
  (do (play-rhythm-for-tick
       (E 5 8)
       4
       tick
       clock
       port-res
       [:freq 800
        :amp 0.1])))

(defn play
  [clock tick]
  (let [beat (clock)]
    (do (at (clock beat) (play-beat clock tick))
        (apply-by (clock (inc beat)) play [clock (+ tick 1)]))))

(play clk 0)
