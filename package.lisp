(defpackage :maclisp
  (:use :cl)
  (:export :iota
           :phi
           :phi
           :pi
           ;; APROPOS
           :apropos
           :apropos-sorted
           ;; BS
           :timit
           :ntimit
           )
  (:shadow :pi
           :apropos))

(in-package :maclisp)

