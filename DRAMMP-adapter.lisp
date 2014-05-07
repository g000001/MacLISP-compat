(in-package :maclisp)


#|(merge-pathnames 
 (make-pathname :name "drammp" 
                :type "LISP"
                :case :common)
 (pathname-directory *load-truename*))|#


(named-readtables:defreadtable :nil
  (:merge :standard)
  (:syntax-from :standard #\\ #\/)
  (:syntax-from :standard #\/ #\\)
  (:dispatch-macro-char #\# #\T
                        (lambda (stream char arg)
                          (declare (ignore stream char arg))
                          'cl:t)))


(defpackage :si
  (:export :GET-PRIMITIVE-SEQUENCE
           :NON-CIRCULAR-DEPTH-LIMIT
           :DRAMMP
           :get-primitive-sequence
           :circularity-error))


#|(defmacro typecaseq (key &rest cases)
  `(cl:typecase ,key
     ,@cases))|#


(deftype bits () 'cl:bit-vector)

(define-condition WRONG-TYPE-ARGUMENT (wrng-type-arg) ())

;; (CERROR proceedp restartp cond msg . args)
;; http://maclisp.info/pitmanual/error.html#21.1.4

(defmacro cerror (proceedp restartp cond msg &rest args)
  (declare (ignore proceedp restartp))
  (let ((cond (if (keywordp (eval cond))
                  (intern (string (eval cond)) :maclisp)
                  cond)))
    `(cl:cerror "" ',cond :message ,msg :argument ',args)) )


(defmacro MULTIPLE-VALUE (vars value-form)
  `(cl:multiple-value-setq ,vars ,value-form))
