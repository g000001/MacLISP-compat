(defpackage :maclisp
  (:use :cl)
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
   :intersection
   :let
   :let*
   :check-type
   :error
   )
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
   ;; thread
   "THREADP"
   "THREAD-CONS"
   "THREAD-CAR"
   "THREAD-CDR"
   "THREAD-UNCDR"
   "THREAD-LAST"
   "THREAD-FIRST"
   "THREAD-LENGTH"
   "THREAD-LENGTH-CDRING"
   "THREAD-LENGTH-UNCDRING"
   "THREAD-RECLAIM"
   "THREAD-RECLAIM-CDRING"
   "THREAD-RECLAIM-UNCDRING"
   "THREAD-RECLAIM-1"
   )
  )

(in-package :maclisp)
