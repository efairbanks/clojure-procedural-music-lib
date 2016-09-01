(use 'overtone.live)
(use 'overtone.helpers.file)
(use '[clojure.string :as str])

                                        ; ------------------------ ;
                                        ; --- VARIABLE HELPERS --- ;
                                        ; ------------------------ ;

(def e-env {})

(defn e-get
  ([key] (e-env key))
  ([key default] (if (= nil (e-env key)) default (e-env key))))

(defn e-set [key value]
  (do (def e-env (assoc e-env key value))
      value))

                                        ; --------------------------------- ;
                                        ; --- ALGORITHMIC MUSIC HELPERS --- ;
                                        ; --------------------------------- ;

(defn cmap
  [func list]
  (if (empty? list)
    '()
    (cons
     (func (first list))
     (cmap func (rest list)))))

(defn split-seq
  "Extract a tail of same elements: [1 1 0 0 0] -> [[1 1] [0 0 0]]"
  [s]
  (let [l (last s)]
    (split-with #(not= l %) s)))

(defn recombine
  ([a b] [a b])
  ([a b c] [a b c])
  ([a b c & more]
   (let [s (concat [a b c] more)
         [head tail] (split-seq s)
         recombined (map concat head tail)
         r-len (count recombined)]
     (if (empty? head)  ;; even pattern
       s
       (apply recombine (concat
                         recombined
                         (drop r-len (drop-last r-len s))))))))

(defn gen-pat
  ([indices size index]
   (if (< index size)
     (if (some (fn [indic] (== indic index)) indices)
       (cons 1 (gen-pat indices size (+ index 1)))
       (cons 0 (gen-pat indices size (+ index 1))))
     []))
  ([indices size]
   (gen-pat indices size 0)))

(defn E
  "Get a pattern of 0s and 1s representing the specified Euclidian/Bjorklund distribution."
  [k n]
  (let [seed (concat (repeat k [1]) (repeat (- n k) [0]))]
        (flatten (apply recombine seed))))

(defn shl
  "Shift a sequence left n times."
  [seq times]
  (if (or (empty? seq) (< times 1))
    seq
    (shl (concat (rest seq) [(first seq)]) (- times 1))))

(defn shr
  "Shift a sequence right n times."
  [seq times]
  (if (or (empty? seq) (< times 1))
    seq
    (shr (concat [(last seq)] (drop-last seq)) (- times 1))))

(defn notes-to-intervals [notes]
  (if (not-empty notes)
    (cons
     (mod (first notes) 12)
     (notes-to-intervals (rest notes)))
    []))

(defn shift-notes [notes shiftby]
  (if (not-empty notes)
    (cons
     (+ (first notes) shiftby)
     (shift-notes (rest notes) shiftby))))

(defn clip-notes [notes min max]
  (if (not-empty notes)
    (let [note (first notes)]
      (if (and (>= note min) (<= note max))
        (cons note (clip-notes (rest notes) min max))
        (clip-notes (rest notes) min max)))))

(defn notes-field-helper [notes min max]
  (if (not-empty notes)
    (let [good-notes (clip-notes notes min max)]
      (concat good-notes (notes-field-helper (shift-notes good-notes 12) min max)))))

(defn notes-field [notes min max]
  (clip-notes (notes-field-helper (notes-to-intervals notes) 0 max) min max))

(defn xpose-note [note floor]
  (let [lowest-note (mod note 12)]
    (letfn [(helper [n f] (if (>= n f) n (helper (+ n 12) f)))] (helper lowest-note floor))))

(defn xpose-notes [notes floor]
  (if (not-empty notes)
    (cons (xpose-note (first notes) floor) (xpose-notes (rest notes) floor))
    []))

(defn wrap-at [seq index]
  (if (not-empty seq)
    (nth seq (mod index (count seq)))
    []))

(defn reduce-pattern [filter-pattern pattern]
  (if (empty? filter-pattern)
    pattern
    (if (empty? pattern)
      []
      (let [filter-step (first filter-pattern)
            next-filter-pattern (if (> (first pattern) 0) (rest filter-pattern) filter-pattern)
            pattern-step (if (and (> (first pattern) 0) (> filter-step 0)) (first pattern) 0)
            next-pattern (rest pattern)]
        (cons pattern-step (reduce-pattern next-filter-pattern next-pattern))))))

(defn pattern-sum-at-index [index patterns]
  (apply + (cmap (fn [pattern] (wrap-at pattern index)) patterns )))

(defn sum-patterns-helper [index args]
  (let [max-length (apply max (cmap count args))]
    (if (>= index max-length)
      []
      (cons (pattern-sum-at-index index args)
            (sum-patterns-helper (+ 1 index) args)))))

(defn sum-patterns [& args]
  (cons (pattern-sum-at-index 0 args)
        (sum-patterns-helper 1 args)))

(defn E-filter [n pattern]
  (reduce-pattern (E n (apply + pattern)) pattern))

(defn E-structure [pattern e-filters]
  (if (empty? e-filters)
    pattern
    (sum-patterns
     pattern
     (E-structure (E-filter (first e-filters) pattern) (rest e-filters)))))

(defn normalize [numbers]
  (let [numbers-max (apply max numbers)]
    (cmap
     (fn [x] (* x (/ 1 numbers-max)))
     numbers)))

(defn substitute-pattern [substitutions pattern]
  (cmap (fn [element] (wrap-at substitutions element)) pattern))

(def sub-pattern substitute-pattern)

(defn clump [n coll]
  (if (or (empty? coll) (>= n (count coll)))
    [coll]
    (cons (take n coll)
          (clump n (drop n coll)))))

(defn clump-shuffle [n shuffle-coll coll]
  (flatten (sub-pattern (clump n coll) shuffle-coll)))

(defn dif [cond func default & args]
  (if cond
    (apply func args)
    default))

                                        ; -------------------------- ;
                                        ; --- SEQUENCING HELPERS --- ;
                                        ; -------------------------- ;

(defn rhythm-to-beats
  ([rhythm length] (rhythm-to-beats rhythm length 0 (count rhythm)))
  ([rhythm length index rlen]
   (if (== (count rhythm) 0) []
       (if (== (first rhythm) 0)
         (rhythm-to-beats (rest rhythm) length (+ index 1) rlen)
         (cons (* (/ index rlen) length)
               (rhythm-to-beats (rest rhythm) length (+ index 1) rlen))))))

(defn beats-for-tick [seq tick]
  (if (= (count seq) 0)
    []
    (if (and (>= (first seq) tick) (< (first seq) (+ tick 1)))
      (cons (- (first seq) tick) (beats-for-tick (rest seq) tick))
      (beats-for-tick (rest seq) tick))))

(defn rhythm-beats-for-tick [rhythm numbeats tick]
  (beats-for-tick (rhythm-to-beats rhythm numbeats) (mod tick numbeats)))

(defn process-params [params]
  (cmap (fn [param] (if (clojure.test/function? param)
                     (param)
                     param)) params))

(defn play-beats [beats clock instrument params]
  (if (not= (count beats) 0)
    (do (at (clock (+ (first beats) (clock))) (apply instrument (process-params params)))
        (play-beats (rest beats) clock instrument params))))

(defn play-rhythm-for-tick [rhythm numbeats tick clock instrument params]
  (play-beats (rhythm-beats-for-tick rhythm numbeats tick) clock instrument params))

(defn prft [rhythm numbeats instrument params]
  (play-rhythm-for-tick rhythm numbeats
                        (e-get :tick 0)
                        (e-get :clock (metronome 120))
                        instrument params))

                                        ; --- ;

(defn play-beats-chord [beats clock instrument chord name params]
  (if (not= (count beats) 0)
    (do
      (at (clock (+ (first beats) (clock)))
          (do
            (cmap (fn [synth] (ctl synth :gate 0)) (e-get name []))
            (e-set name nil)
            (cmap (fn [note]
                    (let [synth-instance (apply instrument
                                                (concat [:freq (midi->hz note)]
                                                        (process-params params)))]
                      (e-set name (concat (e-get name []) [synth-instance])))
                    ) chord)))
      (play-beats-chord (rest beats) clock instrument chord name params))))

(defn play-chord-rhythm-for-tick [rhythm numbeats tick clock instrument chord name params]
  (play-beats-chord (rhythm-beats-for-tick rhythm numbeats tick)
                    clock instrument chord name params))

(defn pcrft [rhythm numbeats instrument chord name params]
  (play-chord-rhythm-for-tick rhythm numbeats
                        (e-get :tick 0)
                        (e-get :clock (metronome 120))
                        instrument chord name params))

                                        ; --- ;

(comment
  (defn play-chord-rhythm-for-tick
    [rhythm numbeats tick clock instrument chord name params]
    (do
      (cmap (fn [synth] (ctl synth :gate 0)) (e-get name []))
      (e-set name nil)
      (cmap
       (fn [note]
         (play-rhythm-for-tick
          rhythm
          numbeats
          tick
          clock
          (fn [& args]
            (e-set name (concat (e-get name []) [(apply instrument args)])))
          (concat [:freq (midi->hz note)] params))) chord))))

(comment
  (play-chord-rhythm-for-tick
   (E 5 8)
   4
   0
   (metronome 120)
   port-res
   [60 64 67]
   :super-saw-test
   []))

                                        ; ------------------------- ;
                                        ; --- STRUCTURE HELPERS --- ;
                                        ; ------------------------- ;

(defn get-song-section [tick] (/ tick 64))

(defn section-index
  ([tick length-in-ticks]
   (/ tick length-in-ticks))
  ([tick length-in-ticks count-to]
   (* count-to (/ (mod tick length-in-ticks) length-in-ticks))))

                                        ; ------------------------ ;
                                        ; --- OVERTONE HELPERS --- ;
                                        ; ------------------------ ;

(defn load-sample-dir
  ([dir]
   (load-sample-dir (ls-file-paths dir) "wav"))
  ([filepaths extension]
   (if (empty? filepaths)
     []
     (if (= (lower-case (file-extension (first filepaths))) extension)
       (cons (load-sample (first filepaths)) (load-sample-dir (rest filepaths) extension))
       (load-sample-dir (rest filepaths) extension)))))

                                        ; ---------------------------
                                        ; --- INST/FX DEFINITIONS ---
                                        ; ---------------------------

(definst psamp
  [buf 0 amp 1 rate 1 sr 1 offset 0 outbus 0]
  (out outbus
       (* (decimator (play-buf 1 buf rate :start-pos (* offset (buf-frames buf)) :action FREE)
                     (* 44100 sr))[amp amp])))

(definst noise
  [dur 1 outbus 0 amp 0.5]
  (let [env (env-gen
             (envelope
              [0 1 0]
              [0.01 dur])
             :action FREE)
        env (pow env 5)]
    (out outbus (* (hpf [(white-noise) (white-noise)] (* env 13000)) env amp))))

(definst buzz
  [freq 440 amp 0.6 dur 1 outbus 0]
  (let [env (env-gen
             (envelope
              [0 1 0]
              [0.01 dur])
             :action FREE)
        saw [(saw (- freq 0.05)) (saw (+ 0.05 freq))]]
    (out outbus (* amp (* saw env)))))

(definst detune [freq 440 amp 0.2 dur 2 outbus 0]
  (let [env (env-gen (envelope [0 1 0] [0.1 dur]) :action FREE)
        randomness (* (sin-osc (n-rand -0.01 0.01 1) (n-rand -3.14 3.14 1))
                      (* freq 0.011)
                      (n-rand -1 1 1))
        tone (hpf (lpf (pulse (+ [(* freq 1.01) (* freq 0.99)] randomness))
                       (* 3 freq))
                  (* 2 freq))]
    (out outbus (* tone env amp))))

(definst hats
  [amp 1 outbus 0]
  (let [env (env-gen
             (envelope
              [0 0.1 0]
              [0.005 0.05])
             :action FREE)
        noise (white-noise)]
    (out outbus (* (* noise env) [amp amp]))))

(definst kick
  [amp 1 outbus 0]
  (out outbus
       (let [env (env-gen
                  (envelope
                   [0 1 0]
                   [0.005 0.15])
                  :action FREE)
             sin (sin-osc
                  (* 400 (* env env env env env)))]
         (* (* sin env) [amp amp]))))

(definst ploop
  [buf 0 amp 1 rate 1 offset 0 t-trig [1 :tr] outbus 0]
  (out
   outbus
   (let
       [breaks
        (play-buf
         1
         buf
         (* rate (buf-rate-scale buf))
         t-trig
         (* offset (buf-frames buf)) 1)]
     (* [amp amp]
        (db->amp 13)
        (compander
         breaks
         breaks
         (db->amp -32)
         1
         1/3
         0.003
         0.03)))))

(definst ploop-grain
  [buf 0 amp 1 rate 1 offset 0 dur 1 t-trig [1 :tr] outbus 0]
  (out
   outbus
   (let
       [index (/ (phasor:ar
                  t-trig
                  (* dur (/ (buf-frames buf) (sample-rate)))
                  0
                  (buf-frames buf)
                  (* offset (buf-frames buf)))
                 (buf-frames buf))
        breaks
        (warp1
         1
         buf
         index
         (* rate (buf-rate-scale buf))
         (/ 1.0 20.0)
         -1
         4
         )]
     (* [amp amp]
        (db->amp 13)
        (compander
         breaks
         breaks
         (db->amp -32)
         1
         1/3
         0.003
         0.03)))))

(definst prog-res [freq 440 amp 0.2 clamp 0.05 res 0.3 attack 0.03 decay 1 exp 8 outbus 0]
  (let [env (pow (env-gen (envelope [0 1 0] [attack decay]) :action FREE) exp)
        tone (saw [(+ freq 0.1)
                   (- freq 0.1)])
        ret (rlpf tone (+ (* env (- 20000 freq) clamp) freq) res)
        ret (* ret env amp)]
    (out outbus ret)))

(definst port-res [freq 440 amp 0.2 clamp 0.05 res 0.3 attack 0.03 decay 1 exp 8 outbus 0]
  (let [env (pow (env-gen (envelope [0 1 0] [attack decay]) :action FREE) exp)
        freq (lag freq 0.1)
        tone (saw [(+ freq 0.1)
                   (- freq 0.1)])
        ret (rlpf tone (+ (* env (- 20000 freq) clamp) freq) res)
        ret (* ret env amp)]
    (out outbus ret)))

(definst wub [freq 55 amp 1 scaler 16.01 attack 0.1 hold 0.01 release 0.1 outbus 0]
  (let [env (env-gen (envelope [0 1 1 0] [attack hold release]) :action FREE)
        mod (* 5000 env (mix (atan (* 5 env (sin-osc (* freq scaler [1 3 5 7 11 13]))))))
                                        ;mod (* env mod)
        mod (+ freq mod)
        output (sin-osc mod)
        output (rlpf output (* env 20000) 0.1)
        output (atan (* 20 env output))
        output (* output env amp)]
    (out outbus [output output])))

  (definst fmbass
    [freq 110
     amp 1
     clamp 0.5
     gate 1
     attack 0.1
     decay 0.9
     sustain 0
     release 0
     outbus 0]
    (let [
          lfo-1 (* (sin-osc (* freq 10.003) 0) 300)
          lfo-2 (* (sin-osc (* freq 2.002) 0) 200)
          env (env-gen (adsr attack decay sustain release 1) gate :action FREE)
          modulator (sin-osc (* 3 freq))
          camulator (sin-osc (+ (* 1.5 freq)
                                (* clamp 2 (* modulator (+ (* env 800)
                                                           (* env lfo-1))))))
          carrier (sin-osc (+ freq
                              (* camulator clamp 2 (+ (* modulator (* env 400))
                                                      (* env lfo-2)))))
          output (* amp env carrier)
          ]
      (out outbus [output output])))

(definst chimes [freq 440 amp 1 outbus 0]
  (let [env (env-gen (envelope [0 1 (db->amp -25) 0] [0.001 0.05 0.5]) :action FREE)
        mod (mix (sin-osc (* [2 4 8] freq)))
        output (* (sin-osc (+ freq (* mod 1000)))
                  (sin-osc (* freq 2.005))
                  (sin-osc (* freq 3.007))
                  (sin-osc (* freq 6.997))
                  env)
        output (* output amp)
        output [output output]]
    (out outbus output)))

(definst vibstab [freq 440 amp 1 outbus 0]
  (let [env (env-gen (envelope [0.1 1 0] [0.5 0.001]) :action FREE)
        output (saw (+ freq (* freq 0.04 env (sin-osc 9))))
        output (* output amp env)
        output (lpf output (* 2.5 freq))
        output (hpf output (* 3.5 freq))
        output [output output]]
    (out outbus output)))

(definst noisy-pulse-sweep [freq 440
                            amp 1
                            outbus 0]
  (let [env (env-gen (envelope [0 1 0] [1 1]) :action FREE)
        mod (* 350 (hpf (pink-noise) 50))
        output (lpf (pulse (+ freq mod) (* 0.5 env)) (* 4 freq))
        output (* amp env output)
        output [output output]]
    (out outbus output)))

(definst port-res-adsr [freq 440 amp 0.2 clamp 0.05 res 0.3 attack 0.03 decay 1 exp 8 outbus 0
                        gate 1]
  (let [env (pow (env-gen (adsr attack decay 0 0.005) gate :action FREE) exp)
        freq (lag freq 0.1)
        tone (saw [(+ freq 0.1)
                   (- freq 0.1)])
        ret (rlpf tone (+ (* env (- 20000 freq) clamp) freq) res)
        ret (* ret env amp)]
    (out outbus ret)))

(comment
  (do
    (definst weird [freq 25]
      klank
      (let [
            output (ringz (bpf (white-noise) (mouse-x 20 1000) 0.05)
                          (+ 2000 (* [1 2 3 4 5 6 7 8 9 10 11 12 13 14] freq (sin-osc 55 0 50 100))) 1)
            ] (out 0 [(mix output) (mix output)])))

    (kill weird)
    (weird)))

(definst super-saw [freq 440
                    amp 1
                    width 3
                    cutoff 1
                    gate 1
                    attack 0.05
                    decay 0.1
                    sustain 0.95
                    release 1
                    outbus 0]
  (let [num-voices 40
        freqs (cmap (fn [n] (- n (* num-voices 0.5))) (range num-voices))
        freqs (cmap (fn [n] (+ n (* (rand) freq 0.001))) freqs)
        freqs (/ freqs (/ num-voices 2))
        freqs (* freqs (* freq 0.01 width))
        freqs (+ freqs freq)
        output (mix (lf-saw:ar freqs (repeatedly num-voices rand)))
        output (* output (env-gen (adsr attack decay sustain release 1) gate :action FREE))
        output (lpf output (* cutoff 18000))
        output (* output amp)
        output [output output]]
    (out outbus output)))

(definst super-sqr [freq 440
                    amp 1
                    width 3
                    cutoff 1
                    gate 1
                    attack 0.05
                    decay 0.1
                    sustain 0.95
                    release 1
                    outbus 0]
  (let [num-voices 40
        freqs (cmap (fn [n] (- n (* num-voices 0.5))) (range num-voices))
        freqs (cmap (fn [n] (+ n (* (rand) freq 0.001))) freqs)
        freqs (/ freqs (/ num-voices 2))
        freqs (* freqs (* freq 0.01 width))
        freqs (+ freqs freq)
        output (mix (lf-pulse:ar freqs (repeatedly num-voices rand) 0.4))
        output (* output (env-gen (adsr attack decay sustain release 1) gate :action FREE))
        output (lpf output (* cutoff 18000))
        output (* output amp)
        output [output output]]
    (out outbus output)))

(comment
  (definst super-sqr [freq 440 amp 1 width 3 gate 1 outbus 0]
    (let [num-voices 80
          freqs (cmap (fn [n] (- n (* num-voices 0.5))) (range num-voices))
          freqs (cmap (fn [n] (+ n (* (rand) freq 0.001))) freqs)
          freqs (/ freqs (/ num-voices 2))
          freqs (* freqs (* freq 0.01 width))
          freqs (+ freqs freq)
          output (mix (lf-pulse:ar freqs (repeatedly num-voices rand) 0.4))
          output (* output (env-gen (adsr 0.05 0.1 0.95 1 1) gate :action FREE))
          output (* output amp)
          output [output output]]
      (out outbus output))))

(definst super-sin [freq 440 amp 1 width 3 gate 1 outbus 0]
  (let [num-voices 80
        freqs (cmap (fn [n] (- n (* num-voices 0.5))) (range num-voices))
        freqs (cmap (fn [n] (+ n (* (rand) freq 0.001))) freqs)
        freqs (/ freqs (/ num-voices 2))
        freqs (* freqs (* freq 0.01 width))
        freqs (+ freqs freq)
        output (mix (sin-osc freqs (repeatedly num-voices rand)))
        output (* output (env-gen (adsr 0.05 0.1 0.95 1 1) gate :action FREE))
        output (* output amp)
        output [output output]]
    (out outbus output)))

(definst glitch
  [ringbuf 0 bus 0 retrigmix 0 t-retrig [1 :tr] retrigspeed 1 retrigdur 1]
  (let [index (phasor:ar 0 (buf-rate-scale ringbuf) 0 (buf-frames ringbuf))
        index (* index (- 1 retrigmix))
        write (buf-wr (in bus 2) ringbuf index 1)
        retrigindex (phasor:ar
                     t-retrig
                     (* retrigspeed (buf-rate-scale ringbuf))
                     0
                     (* retrigdur retrigspeed (sample-rate)))
        bufframeslatch (latch:ar index t-retrig)
        retrigindex (+ retrigindex bufframeslatch)
        retrigindex (* retrigindex retrigmix)
        index (+ index retrigindex)
                                        ;index (wrap:ar index 0 (buf-frames ringbuf))
        read (buf-rd 2 ringbuf (- index 200) 1 1)]
    (out 0 (* 0.11 (atan (* 5 read))))))

(definst fx
  [inbus 0 outbus 0]
  (out outbus (rlpf (in inbus 2) 16000)))

                                        ; ----------------- ;
                                        ; --- ENV STUFF --- ;
                                        ; ----------------- ;

(take 3 [0 1 2])

(defn ienv-get-index-helper
  ([durations index]
   (ienv-get-index-helper durations index 0))
  ([durations index durations-index]
   (let [index (mod index (sum durations))]
     (if (< index (sum (take durations-index durations)))
       (- durations-index 1)
       (ienv-get-index-helper durations index (+ durations-index 1))))))

(defn ienv-circular-sum-helper
  ([coll index n]
   (if (> n 0)
     (+ (wrap-at coll index) (ienv-circular-sum-helper coll (+ index 1) (- n 1)))
     0)))

(defn ienv
  ([levels durations index exp]
   (Math/pow (ienv levels durations index) exp))
  ([levels durations index]
   (let [
         index (mod index (sum durations))
         current-index (ienv-get-index-helper durations index)
         next-index (mod (+ current-index 1) (count levels))
         index-start (ienv-circular-sum-helper durations 0 current-index)
         index-end (ienv-circular-sum-helper durations 0 next-index)
         index-delta (- index-end index-start)
         normalized-index (/ (- index index-start) index-delta)
         interpolated-levels (+ (* (wrap-at levels current-index)
                                   (- 1 normalized-index))
                                (* (wrap-at levels next-index)
                                   normalized-index))
         ] interpolated-levels)))

                                        ; ------------ ;
                                        ; --- PLAY --- ;
                                        ; ------------ ;

(defn play-beat [clock tick]
  (println "please define play-beat"))

(defn play
  ([clock tick]
   (let [beat (clock)]
     (do (at (clock beat) (play-beat clock tick))
         (apply-by (clock (inc beat)) play [clock (+ tick 1)]))))
  ([clock tick play-beat]
   (let [beat (clock)]
     (do (at (clock beat) (play-beat clock tick))
         (apply-by (clock (inc beat)) play [clock (+ tick 1)])))))

                                        ; --------------------- ;
                                        ; --- EXAMPLE USAGE --- ;
                                        ; --------------------- ;

(def bpm 165)
(def clk (metronome bpm))

(comment
  (def chds
    [
     (chord :c4 :major)
     (chord :g4 :major)
     (chord :a4 :minor)
     (chord :f4 :minor)
     ]))

(def chds
  [
   (chord :c4 :major)
   (chord :b4 :sus4)
   (chord :g4 :major)
   (chord :a4 :minor)
   (chord :f4 :minor)
   (chord :e4 :dim)
   ])

(defn cycleChds
  []
  (def chds (concat (rest chds) [(first chds)])))

(def TR-909 "/Users/eric/Documents/Samples/Sample Packs/drum machine samples/Roland TR-909/TR-909/")

(def breaks (load-sample-dir "/sounds/breaks"))

(def bd909 (sample (str TR-909 "BASSDRUM/BDRUM1.WAV")))
(def sn909 (sample (str TR-909 "SNARES/SNARE1.WAV")))
(def cp909 (sample (str TR-909 "SNARES/CLAP1.WAV")))
(def hh909 (sample (str TR-909 "CYMBALS/HHCLOSE1.WAV")))

(def bd909 (load-sample (str TR-909 "BASSDRUM/BDRUM1.WAV")))
(def sn909 (load-sample (str TR-909 "SNARES/SNARE1.WAV")))
(def cp909 (load-sample (str TR-909 "SNARES/CLAP1.WAV")))
(def hh909 (load-sample (str TR-909 "CYMBALS/HHCLOSE1.WAV")))
(def apache (load-sample "/Users/eric/Documents/Samples/Breaks/78881_OaSyntax_160_apache.wav"))

(def glitchbus (audio-bus 2))
(def glitchinst (glitch (buffer (* 1 44100) 2) glitchbus 0 0 1 1))
(def fxbus (audio-bus 2))
(def fxinst (fx fxbus 0))

;example of how to use
(comment
  (defn play-beat [clock tick]
    (do
      (comment)
      (play-rhythm-for-tick
       (concat
        (E 4 16))
       4
       tick
       clock
       ctl
       [break
        :buf (wrap-at
              breaks
              (wrap-at (wrap-at [[2] [3 4 10 0] [7] [5 8 1]] (get-song-section tick)) (mod (* 7 tick) 11)))
        :rate 1
        :offset (wrap-at [(/ (mod tick 4) 4) (/ (mod (* 11 tick) 8) 8)] (get-song-section tick))
        :t-trig 1
        :amp (* 1 0.78)
        :outbus glitchbus])
      (comment)
      (play-rhythm-for-tick
       (concat
        (E 13 40))
       20
       tick
       clock
       ctl
       [glitchinst
        :retrigmix (wrap-at (E (wrap-at [0 4] (get-song-section tick)) 23) tick)
        :t-retrig 0
        :retrigdur (/ (beat-ms (wrap-at [0.5 1 0.125 0.25 0.75] tick) bpm) 1000)
        :retrigspeed (wrap-at [1 0.5 1 -1 1.5 1 -0.5] tick)])
      (comment)
      (play-rhythm-for-tick
       (E 1 8)
       64
       tick
       clock
       noise
       [:amp 0.1 :dur 15 :outbus glitchbus])
      (comment)
      (play-rhythm-for-tick
       (concat
        (E 3 8)
        (E 4 8)
        (E 3 8)
        (E 5 8))
       16 tick clock psamp [bd909 0.23 0.9 :outbus (wrap-at [fxbus glitchbus] tick)])
      (comment)
      (play-rhythm-for-tick
       (concat
        (E 5 8))
       8 tick clock psamp [hh909 0.1 0.2 0.03 :outbus fxbus])
      (play-rhythm-for-tick
       (concat
        (shr (E 2 20) 8))
       20 tick clock psamp [sn909 0.25 0.35 0.3 :outbus glitchbus])
      (play-rhythm-for-tick
       (concat
        (shr (E 2 20) 8))
       20 tick clock psamp [cp909 0.3 0.75 0.2 :outbus glitchbus])
      (comment)
      (play-rhythm-for-tick
       (concat
        (E (wrap-at [0 11] (get-song-section tick)) 20))
       10 tick clock buzz [(+ (rand 3) (midi->hz (xpose-note (first (first chds)) 30))) 0.25 :outbus glitchbus])
      (play-rhythm-for-tick
       (concat
        (E 11 20))
       10 tick clock buzz [(+ (rand 6) (midi->hz (xpose-note (second (first chds)) 58))) 0.07 :outbus fxbus])
      (play-rhythm-for-tick
       (concat
        (E (wrap-at [0 5] (/ (get-song-section tick) 2)) 8))
       4
       tick
       clock
       buzz
       [(+ (rand 10) (midi->hz (wrap-at (notes-field (first chds) 70 87) tick))) 0.05 :outbus fxbus])
      (play-rhythm-for-tick
       (concat
        (E 5 8))
       16 tick clock cycleChds [])))

  (defn play
    [clock tick]
    (let
        [beat (clock)]
      (do (at (clock beat) (play-beat clock tick))
          (def tick (+ tick 1))
          (apply-by (clock (inc beat)) play [clock (+ tick 1)]))))

  (play clk 0))
