;;;; cl-collider.lisp - all cl-collider-specific functionality.
;; This file is loaded by utility.lisp only if the cl-collider package exists.

(in-package #:thundersnow/common)

(unless (cl-collider:synthdef-metadata :default)
  (cl-collider:defsynth :default ((gate 1) (freq 440) (amp 0.5) (pan 0) (out 0))
    (let* ((env (cl-collider:env-gen.kr (cl-collider:asr 0.01 1 0.1) :gate gate :act :free))
           (sig (cl-collider:sin-osc.ar freq 0 0.2)))
      (cl-collider:out.ar out (cl-collider:pan2.ar sig pan (* env amp))))))
