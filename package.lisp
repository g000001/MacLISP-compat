(cl:in-package :cl-user)

#|(cl:delete-package :maclisp)|#
(cl:defpackage :maclisp
  (:use)
  (:nicknames :ml)
  (:export
   ;; IOTA
   :do
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
   :subsetp
   :let
   :let*
   :check-type
   :error
   :cerror
   :def-carcdr
   )
  )



(cl:defpackage :maclisp.internal
  (:use :cl :named-readtables
        :maclisp)
  (:shadowing-import-from :maclisp
                          :pi
                          ;; APROPOS
                          :apropos
                          :do
                          ;; SETS
                          :union
                          :subsetp
                          ;; CARCDR
                          ;; LSETS
                          :adjoin
                          :union
                          :intersection
                          :let
                          :let*
                          :check-type
                          :error
                          :cerror
                          ))
