;;;   CNVD  			-*-Mode:Lisp;Package:SI;Lowercase:T-*-
;;;   ****************************************************************
;;;   ***** NIL ******** Certify-no-var-dependency/| *****************
;;;   ****************************************************************
;;;   ** (c) Copyright 1981 Massachusetts Institute of Technology ****
;;;   ****************************************************************

(herald CNVD /2)

#M (include ((lisp) subload lsp))

#M (eval-when (eval compile)
     (subload SHARPCONDITIONALS)
     )

#+(or LISPM (and NIL (not MacLISP)))
  (globalize "Certify-no-var-dependency/|")

		     
(declare (special BAD-VARS BOUND-VARS ALL-LOCALS)
	 (genprefix |Cnvd|)
     #M  (setq DEFMACRO-FOR-COMPILING () ))   




(DEFUN |Certify-no-var-dependency/|| (FORM)
    ; This functions says "yes" if the evaluation of FORM does not depend upon
    ;   any of the variables in BAD-VARS, and where ALL-LOCALS is a flag with
    ;   non-null meaning that there are no special variables in the BAD-VARS
    ; Requires these three special variables to be bound by the caller:
    ;  	BAD-VARS   (sart at list of variables for which dependency is checked)
    ;   BOUND-VARS (start at () ) 
    ;   ALL-LOCALS (start at 'T)
   (PROG (X)
      A  (COND ((ATOM FORM)				  ;True iff FORM can be
		(RETURN (COND ((NOT (SYMBOLP FORM)))	  ; guaranteed not have
			      ((MEMQ FORM BOUND-VARS))	  ; any free references
			      ((MEMQ FORM BAD-VARS) () )  ; to any variable in
			      ('T)))))			  ; BAD-VARS
	 (LET (FL)
	   (ERRSET (SETQ FORM (MACROEXPAND FORM) FL 'T) () )
	   (IF (NULL FL) (RETURN () )))
	  ;;Find out if recursing on args is all that's needed (and maybe
	  ;;  do a little work along the way!)
	 (IF (COND 
	       ((NOT (ATOM (CAR FORM)))
	         (COND 
		   ((EQ (CAAR FORM) 'LAMBDA)
		     (LET ((BOUND-VARS (IF (ATOM (CADAR FORM)) 
					   (CONS (CADAR FORM) BOUND-VARS)
					   (APPEND (CADAR FORM) BOUND-VARS))))
		      (|Certify-no-var-dependency/|| `(PROGN ,.(cddar form)))))
		   ((SETQ X (MACROEXPAND-1* (CAR FORM)))
		    (SETQ FORM (CONS (CAR X) (CDR FORM)))
		    (GO A))
		   ('T (RETURN () ))))
	       ((SYMBOLP (CAR FORM))
	         (IF (EQ (CAR FORM) 'QUOTE) 
		     (RETURN 'T)
		     (IF (MEMQ (CAR FORM) '(FUNCTION *FUNCTION))
			 (IF (ATOM (CADR FORM)) 
			     (RETURN 'T)
			     (PROG2 (SETQ FORM (CADR FORM)) (GO A)))))
		 #-Lispm (+internal-try-autoloadp (car form))
		 (COND ((EQ (GET (CAR FORM) '|side-effectsp/||) '|mmcdrside/||)
			  ;;Note how this cooperates with the established
			  ;; convention described in MACAID for side-effectsp/|
			 'T)
		    #M ((NOT (SYSP (CAR FORM))) () )
		       ((|APPLICABLEP-cnvd/|| (CAR FORM))
		         (COND ((MEMQ (CAR FORM) '(FUNCALL APPLY MAPC MAP MAPF 
						   MAPCON MAPLIST MAPATOMS 
						   MAPCAR MAPCAN *APPLY))
			         (IF (AND (NOT (ATOM (CADR FORM))) 
					  (SYMBOLP (CADADR FORM)))
				     (|Certify-no-var-dependency/|| 
				       `(,(cadadr form) () ))))
			       ((MEMQ (CAR FORM) '(EVAL *EVAL READ *READ)) () )
			       ('T)))
		       ((MEMQ (CAR FORM) '(OR AND SETQ PSETQ PROG1 PROG2 PROGN 
					   CATCH *CATCH CATCHALL CATCH-BARRIER 
					   ERRSET UNWIND-PROTECT )))
		       ((OR (MEMQ (CAR FORM) '(STATUS SSTATUS SIGNP))
			    (AND (EQ (CAR FORM) 'DO) (SYMBOLP (CADR FORM))))
		        (SETQ FORM (CDR FORM)) 
			'T)))
	       ('T (RETURN () )))
	     (RETURN (|map-cnvd/|| (CDR FORM) 'T)))
	 (RETURN 
	   (COND ((NOT (SYMBOLP (CAR FORM))) () )
		 ((EQ (CAR FORM) 'COND) 
		   (DO ((Y (CDR FORM) (CDR Y)))
		       ((NULL Y) 'T)
		     (IF (NOT (|map-cnvd/|| (CAR Y) 'T)) (RETURN () ))))
		((EQ (CAR FORM) 'PROG)
		  (LET ((BOUND-VARS (APPEND (CADR FORM) BOUND-VARS)))
		    (|map-cnvd/|| (CDDR FORM) () )))
		((AND (EQ (CAR FORM) 'DO) 
		      (OR (NULL (CADR FORM)) (NOT (ATOM (CADR FORM)))))
		  (SETQ X (MAPCAR #'(LAMBDA (X)
				      (IF (ATOM X) 
					  (LIST X)
					  (LIST (CAR X) (CADR X) (CADDR X))))
				  (CADR FORM)))
		  (AND (|map-cnvd/|| (MAPCAR #'CADR X) 'T)
		       (LET ((BOUND-VARS (NCONC (MAPCAR 'CAR X) BOUND-VARS))) 
			 (AND (|map-cnvd/|| (MAPCAR #'CADDR X) 'T)
			      (|map-cnvd/|| (CDDDR FORM) () )))))
		((MEMQ (CAR FORM) '(CASEQ TYPECASEQ))
		  (COND ((NOT (|Certify-no-var-dependency/|| (CADR FORM))) () )
			((DO ((Y (CDDR FORM) (CDR Y)))
			     ((NULL Y) 'T)
			   (IF (NOT (|map-cnvd/|| (CDAR Y) 'T)) 
			       (RETURN () ))))))
		(ALL-LOCALS (|map-cnvd/|| (CDR FORM) 'T))
		 ;;;If all the BAD-VARS are local, then this line will permit 
		 ;;; the use of random functions in FORM, since there can be no 
		 ;;; non-lexical variable dependencies. 
	       ))))



(defun |APPLICABLEP-cnvd/|| (x &aux (fbp (fboundp x)))
  (and fbp 
      #-MacLISP (subrp (fsymeval x))
      #+MacLISP (not (memq (car fbp) '(FEXPR FSUBR)))
      #+Lispm   (NOT (MEMQ X '(COND PROG SETQ OR AND STATUS SSTATUS DO PSETQ
				DO-NAMED ERRSET CATCH *CATCH CATCHALL)))
	 ))


(defun |map-cnvd/|| (form symbolp)
  (do ((y form (cdr y)))			;Requires two vars to be setup
      ((null y) 'T)				; BAD-VARS, and BOUND-VARS
    (and (not (|Certify-no-var-dependency/|| (car y))) 
	 (or symbolp (not (symbolp (car y))))
	 (return () ))))
