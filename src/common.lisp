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

;;; initialization

(defparameter *initialized* nil
  "True after `thundersnow-initialize' has been run.")

(defun load-config ()
  "Load the user's custom configurations by checking a few common locations."
  (loop :for file :in (list (concatenate 'string (uiop:getenv "XDG_CONFIG_HOME") "/thundersnow/config.lisp")
                            (concatenate 'string (uiop:getenv "HOME") ".thundersnow.lisp"))
        :until (load file :if-does-not-exist nil)))

(defun thundersnow-initialize ()
  "Run thundersnow's initialization routine; read config files, etc."
  (load-config)
  (setf *initialized* t))
