;; -*-LISP-*-
(IN-PACKAGE :MACLISP)

(define-condition wrng-type-arg (simple-error)
  ((argument :reader wrng-type-arg-argument :initarg :argument)
   (message :reader wrng-type-arg-message :initarg :message))
  (:report (lambda (condition stream)
             (format stream
                     (wrng-type-arg-message condition)
                     (wrng-type-arg-argument condition)))))

(defun exploden (expr)
  (map 'list
    (lambda (x) (char-code x))
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

