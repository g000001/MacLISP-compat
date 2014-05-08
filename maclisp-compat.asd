;;;; /home/mc/lisp/work/maclisp-compat/maclisp-compat.asd

(asdf:defsystem #:maclisp-compat
  :serial t
  :depends-on (:named-readtables)
  :components ((:file "package")
               (:file "base")
               (:file "LET")
               (:file "IOTA")
               (:file "BS")
               (:file "APROPOS")
               (:file "LSETS")
               (:file "THREAD")))
