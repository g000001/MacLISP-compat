;; -*-LISP-*-
(IN-PACKAGE :MACLISP)

;;; debug var
(defvar *RSET nil)

(defmacro keep (&body body)
  (declare (ignore body))
  (values))

;;; http://maclisp.info/pitmanual/error.html#error
(defun error (&optional msg datum kwd)
  (cl:error ";~@[~A~] ~@[~A~] ~@[~A~]~%"
            kwd
            datum
            msg))

(define-condition wrng-type-arg (cl:error)
  ((argument :reader wrng-type-arg-argument :initarg :argument)
   (message :reader wrng-type-arg-message :initarg :message))
  (:report (lambda (condition stream)
             (format stream
                     (wrng-type-arg-message condition)
                     (wrng-type-arg-argument condition)))))

(define-condition wrng-no-args (cl:error)
  ((argument :reader wrng-no-args-argument :initarg :argument)
   (message :reader wrng-no-args-message :initarg :message))
  (:report (lambda (condition stream)
             (format stream
                     (wrng-no-args-message condition)
                     (wrng-no-args-argument condition)))))

(defun exploden (expr)
  (map 'list
    (lambda (x) (char-code x))
    (princ-to-string
     (read-from-string
      (write-to-string expr)))))

(defun explodec (expr)
  (map 'list
    (lambda (x) (intern (string x)))
    (princ-to-string
     (read-from-string
      (write-to-string expr)))))

(defun mapatoms (function)
  (let (ans)
    (do-all-symbols (s)
      (push (funcall function s) ans))
    ans))

(defun getcharn (string-designator pos)
  (char-code
   (char (string string-designator)
         (1- pos))))

(defun flatc (expr)
  (length
   (princ-to-string
    (read-from-string
     (write-to-string expr)))))

(defun plist (sym)
  (and (symbolp sym)
       (symbol-plist sym)))

(defun alphalessp (x y)
  (null (not (string< x y))))

(setf (symbol-function 'add1) #'1+)
(setf (symbol-function 'greaterp) #'>)
(setf (symbol-function 'lessp) #'<)
(setf (symbol-function 'DIFFERENCE) #'-)
(setf (symbol-function 'DIFFERENCE) #'-)
(setf (symbol-function 'REMAINDER) #'rem)

(defun bigp (obj)
  (typep obj 'bignum))

(defun lsh (integer count)
  (ash (- integer) count))

(defun minus (&optional number)
  (if number
      (- number)
      0))

(defun implode (expr)
  (values (intern (format nil "~{~A~}" expr))))

(defun memq (item list)
  (member item list :test #'eq))

(defun delq (item list)
  (declare (list list))
  (delete item list :test #'eq))


;;; fixme
(defmacro herald (&rest args)
  (declare (ignore args))
  nil)

(defmacro defconst (var val &optional doc)
  `(defconstant ,var ,val ,@(and doc (list doc))))


(defmacro without-interrupts (&body body)
  `(#+sbcl sb-sys:without-interrupts
    #-sbcl progn
    ,@body))


#|(defmacro CHECK-TYPE (var type-test-predicate using-function)
   (cond ((and var (symbolp var)) () )
	 ((fboundp 'si:check-typer)
	   (setq var (si:check-typer var #'SYMBOLP '|CHECK-TYPE MACRO|)))
	 ('T (error '|Not a SYMBOL| var)))
   `(SETQ ,var (SI:CHECK-TYPER ,var ,type-test-predicate ,using-function)))|#

(defun munkam (fixnum)
  #+sbcl (sb-kernel:make-lisp-obj fixnum))

(defun maknum (q)
  #+sbcl (sb-kernel:get-lisp-obj-address q))

(defun +internal-lossage (id fn datum)
  (cl:error "~&;System error, or system code incomplete: Id '~A' in function ~A.~%~
            ;+INTERNAL-LOSSAGE (~@*~A ~A ~A)~%~
~%~
            ;BKPT FAIL-ACT~%"
         id
         fn
         datum))

(defmacro defsimplemac (name (&rest args) &body body)
  `(defmacro ,name (,@args) ,@body))


(defmacro GEN-LOCAL-VAR (&optional var (gentempper () gp))
  (declare (ignore gentempper))
  `(gensym ,@(and var)))

(defmacro check-type (&rest args)
  (declare (ignore args))
  nil)


