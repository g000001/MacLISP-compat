(cl:in-package :maclisp.internal)


(def-carcdr cadadadadddr
            cdadadadadddr
            cddddddddddddddddddddr)

(PROGN
 (DEFUN CADADADADDDR (LIST) ((LAMBDA (X) (CADADR (CADADR (CDDR X)))) LIST))
 (DEFUN CDADADADADDDR (LIST) ((LAMBDA (X) (CDADAR (CDADAR (CDDDR X)))) LIST))
 (DEFUN CDDDDDDDDDDDDDDDDDDDDR (LIST) ((LAMBDA (X) (CDDDDR (CDDDDR (CDDDDR (CDDDDR (CDDDDR X)))))) LIST)))



