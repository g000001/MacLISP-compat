;;;; readtable.lisp

(cl:in-package :maclisp.internal)
(in-readtable :common-lisp)

(named-readtables:defreadtable :maclisp
  (:merge :standard)
  (:syntax-from :standard #\\ #\/)
  #|(:syntax-from :standard #\/ #\\)|#
  (:case :upcase))


;;; *EOF*

