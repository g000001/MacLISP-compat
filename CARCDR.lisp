;;; -*-LISP-*-

;;; Purpose:  to permit long names, like CADADADADDDR, to be easily
;;;    macro-defined into appropriate sequences of CARs and CDRs.
;;; Use: (DEF-CARCDR CADADADADDDR CADADADDDDDR ... )
;;;	where the names must have at least 5 A/D's.

;;; Produces a format internal to the compiler when being expanded
;;;    for optimal compilation.  For interpretation, produces a
;;;    LAMBDA form with a composition of initial carcdr functions
;;;    of up to 4 deep, which should be (already) defined primitively.

(IN-PACKAGE :MACLISP)

(DEFMACRO DEF-CARCDR (&rest carcdrs)
    `(PROGN ,@(mapcar (lambda (x)
                        `(defun ,x (list)
                           ,(c*r (list x 'list))))
                      carcdrs)))

(DEFUN C*R (X)
#|   (LET (((NAME ARG1 . L) X))|#
     (destructuring-bind (NAME ARG1 . L) X
	(AND L (ERROR 'WRNG-NO-ARGS
                      :message "~A Extra args in call to C*R macro"
                      :argument X))
	(AND (OR (< (LENGTH (SETQ L (EXPLODEC NAME))) 7)
		 (NOT (EQ (CAR L) 'C))
		 (NOT (EQ (CAR (SETQ L (NREVERSE (CDR L)))) 'R))
		 (DO ((L (SETQ L (NREVERSE (CDR L))) (CDR L))) ((NULL L))
		     (AND (NOT (MEMBER (CAR L) '(A D))) (RETURN 'T))))
	     (ERROR 'WRNG-TYPE-ARG
                    :message "~A Invalid name for C*R macro|"
                    :argument X))
	`((LAMBDA (X) ,(|c*r-expander\|| l 'x))
          ,arg1)))

(DEFUN |c*r-expander\|| (L ARG)
  (COND ((< (LENGTH L) 5) `(,(implode (nconc (list 'C) l '(R))) ,arg))
	((LET* ((3TAIL (NTHCDR 3 L)) (4TAIL (CDR 3TAIL)))
	       (RPLACD 3TAIL () )
	       (|c*r-expander\|| L (|c*r-expander\|| 4TAIL ARG))))))
