;;;; /home/mc/lisp/work/maclisp-compat/maclisp-compat.asd

(asdf:defsystem #:maclisp-compat
  :serial t
  :components ((:file "package")
               (:file "base")
               (:file "IOTA")
               (:file "BS")
               (:file "APROPOS")))

