(defpackage :maclisp
  (:use :cl)
  (:export
   ;; IOTA
   :iota
   :phi
   :pi
   ;; APROPOS
   :apropos
   :apropos-sorted
   ;; BS
   :timit
   :ntimit
   )
  (:shadow
   ;; IOTA
   :pi
   ;; APROPOS
   :apropos
   ;; SETS
   :union
   :subsetp
   ;; CARCDR
   :def-carcdr
   ))

(in-package :maclisp)

