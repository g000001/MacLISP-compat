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
   ;; LSETS
   :adjoin
   :setdiff
   :union
   :intersection
   :setremq
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
   ;; LSETS
   :adjoin
   :union
   )
  ))

(in-package :maclisp)

