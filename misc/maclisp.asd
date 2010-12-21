;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; -*-
(in-package :cl-user)

(asdf:defsystem :MACLISP
  :name "MacLISP"
  :components ((:FILE "reader")
               (:file "package")
               (:file "iota")))


