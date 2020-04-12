(in-package #:thundersnow)

(defsynth default ((gate 1) (freq 440) (amp 0.5) (pan 0) (out 0))
  (let* ((env (env-gen.kr (asr 0.01 1 0.1) :gate gate :act :free))
         (sig (sin-osc.ar freq 0 0.2)))
    (out.ar out (pan2.ar sig pan (* env amp)))))

(pb :-metronome
  :instrument :default
  :dur 1
  :legato 0.1
  :midinote (pif (pnary #'= 0
                        (pnary #'mod (pbeat) 4))
                 69
                 60)
  :quant 1)
