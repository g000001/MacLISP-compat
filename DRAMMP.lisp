(in-package :maclisp)
(named-readtables:in-readtable :nil)

;;;   DRAMMP 						  -*-LISP-*-
;;;   **************************************************************
;;;   *** NIL *** Del, Rem, Ass, Mem, and Pos type functions *******
;;;   **************************************************************
;;;   ** (C) Copyright 1981 Massachusetts Institute of Technology **
;;;   ****** This is a Read-Only file! (All writes reserved) *******
;;;   **************************************************************

(herald DRAMMP /19)

#-NIL (include ((lisp) subload lsp))
#-NIL (eval-when (eval compile)
	(subload SHARPCONDITIONALS))

#+(local MacLISP)
  (eval-when (eval compile)
     (subload MACAID)
      ;; Remember, EXTMAC down-loads CERROR
     (subload EXTMAC)
     (subload EXTEND)
     (subload VECTOR)
     (subload SUBSEQ)
     (if (fboundp 'OWN-SYMBOL) (own-symbol LENGTH NREVERSE))
     )


(defun si:GET-PRIMITIVE-SEQUENCE (z fun &optional Q-seq-p &aux type)
   "Ascertain whether the 1st arg is a primitive sequence, [or Q-sequence,
    if 'Q-seq-p' is non-()], and signal a correctable error if not.  Returns
    the possibly-corrected value, and the general type."
  (do () 
      ((setq type (typecaseq z 
			(PAIR   'LIST)
			(VECTOR 'VECTOR)
			(STRING (if (null Q-seq-p) 'STRING))
			(BITS   (if (null Q-seq-p) 'BITS))
			(T (if (null z) 'LIST)))))
    (setq z (cerror #T () ':WRONG-TYPE-ARGUMENT 
		    "~1G~S is not a ~:[~;Q-~]sequence -- ~S"
		    () z Q-seq-p fun)))
  (values z type))

(defvar SI:NON-CIRCULAR-DEPTH-LIMIT 100000.)


;;;; SI:DRAMMP


(defun SI:DRAMMP (x oseq funname vecp pred access ret-type 
		 &optional (starti 0) (cntr SI:NON-CIRCULAR-DEPTH-LIMIT cntrp))
  (if (null oseq) 
      () 
      (let ((seq oseq)
	    (typx (ptr-typep x))
	    (typs (typecaseq oseq
			(PAIR 'PAIR)
			((VECTOR VECTOR-S) (and vecp 'VECTOR)))))
	(if (null typs)
	    (multiple-value 
	       (seq typs) 
	      (si:get-primitive-sequence seq (car funname) #T)))
	(cond 
	  ((and (null cntrp) 
		(eq pred 'EQUAL) 
	  #-NIL (eq typs 'PAIR)
		(eq-for-equal? x))
	        (caseq (cdr funname) 
		   (MEM    (memq x seq))
		   (ASS    (assq x seq))
		   (DEL    (delq x seq))
		   (RASS   (rassq x seq))
		   (DELASS (delassq x seq))
		   (MEMASS (memassq x seq))
		   (POSASS (posassq x seq))
		   (POSMEM (posq x seq))))
	  (    (prog (item i n lvec slot delp back-slot del-scanner posp pairp)
		 (declare (fixnum i n))
		 (setq i (1- starti)  n (1+ cntr))
		 (caseq ret-type 
		   (DEL (setq delp #T del-scanner seq))
		   (POS (setq posp #T)))
		 (cond ((eq typs 'PAIR) (setq pairp #T))
		       (#T (setq lvec (vector-length seq))))
	       A (cond 
		   ((not (< (setq n (1- n)) 0)))
		   ((null cntrp) 
		    (setq n (si:circularity-error (car funname) (list seq))))
		   (#T (setq seq () lvec (- SI:NON-CIRCULAR-DEPTH-LIMIT))))
		 (cond ((eq typs 'PAIR)
			 (cond (delp 
				 (if (null seq)
				     (return del-scanner)))
			       (#T (or seq (return () ))
				   (and posp (setq i (1+ i)))))
			 (setq slot (car seq)))
		       (#T (setq i (1+ i))
			   (or (< i lvec) (return () ))
			   (setq slot (vref seq i))))
		   ;Access the relevant item from the sequence
		 (cond ((eq access 'CAR) (setq item slot))
		       ((atom slot) (go b))
		       ((setq item (if (eq access 'CDAR) 
				       (cdr slot)
				       (car slot)))))
		   ;Calculate the "equivalence"
		 (cond ((cond ((eq x item))
			      ((not (eq pred 'EQUAL)) 
			       (if (eq pred 'EQ) 
				   () 
				   (funcall pred x item)))
			      ((not (eq (ptr-typep item) typx)) () )
			      ((caseq typx 
				  (STRING (null (string-mismatchq x item)))
				  (FIXNUM (= x item))	
				  (FLONUM (=$ x item))
				  (T      (EQUAL x item)))))
			(cond (delp 
			        (if (null back-slot)
				    (setq del-scanner (cdr del-scanner))
				     ;;'seq' should be eq to (cdr back-slot)
				    (rplacd back-slot (cdr seq)))
				(setq seq (cdr seq))
				(go A))
			      (#T (return (caseq ret-type 
					    (ASS slot)
					    (MEM seq)
					    (POS i)))))))
	       B (if delp (setq back-slot seq))
		 (if pairp (setq seq (cdr seq)))
		 (go A)))))))



(eval-when (eval compile)
     (setq defmacro-for-compiling () )
)

(defmacro GEN-DRAMMP/| (&rest form &aux name vecp access ret-type ans stnnm)
   `(PROGN 
      'COMPILE 
      ,.(mapcan 
	 #'(lambda (x)
	    (desetq (funname vecp access ret-type) x)
	     ;; First comes the generalized function, like ASS and MEM.
	    (setq 
	     ans
	     `((DEFUN ,(cdr funname) 
		   (PRED ITEM SEQ &OPTIONAL (START 0) 
					    (CNT SI:NON-CIRCULAR-DEPTH-LIMIT))
		 (SI:DRAMMP ITEM 
			    SEQ 
			    '(,(cdr funname) . ,(cdr funname))
			    ',vecp 
			    PRED 
			    ',access 
			    ',ret-type 
			    START 
			    CNT))))
	     ;;Then if permitted comes the one with EQUAL testing
	    (cond 
	     ((car funname)
	       (setq stnnm (gentemp))
	       (setq ans 
		     (nconc 
		       `((DEFUN ,(car funname) 
			      (ITEM SEQ &OPTIONAL (START 0) 
					(CNT SI:NON-CIRCULAR-DEPTH-LIMIT))
			    (SI:DRAMMP ITEM 
				       SEQ 
				       ',funname 
				       ',vecp 
				       'EQUAL 
				       ',access 
				       ',ret-type 
				       START 
				       CNT))
			 (DEFUN ,stnnm (X)
			    (LET (((() ITEM SEQ . MORE) X))
			      (VALUES 
			        `(SI:DRAMMP ,ITEM 	;"item"
					    ,SEQ 	;"sequence" 
					    ',',funname 
					    ',',vecp 
					    'EQUAL 
					    ',',access 
					    ',',ret-type 
					    ,.MORE	;possibly &optinals
					    )
				#T)))
			 (PUSH ',stnnm (GET ',(car funname) 'SOURCE-TRANS)))
		       ans)))))
	 form)))

(eval-when (eval compile)
     (setq defmacro-for-compiling #T defmacro-displace-call MACROEXPANDED)
)

(gen-drammp/| ((          RASSOC        .   RASS)  T  CDAR ASS) 
	      ((#-MacLISP  ASSOC  #M () .    ASS)  T  CAAR ASS)
	      ((#-MacLISP DELETE  #M () .    DEL) ()   CAR DEL)
	      ((#-MacLISP MEMBER  #M () .    MEM) ()   CAR MEM)
	      ((        DELASSOC        . DELASS) ()  CAAR DEL)
	      ((        MEMASSOC        . MEMASS) ()  CAAR MEM)
	      ((        POSASSOC        . POSASS)  T  CAAR POS) 
	      ((       POSMEMBER        . POSMEM)  T   CAR POS) )




#M (progn 'compile 

(defun RASSQ (x ll) 
  (if *RSET (check-type ll #'LISTP 'RASSQ))
  (do ((l ll (cdr l)) (e))
      ((null l) () )
    (and (pairp (setq e (car l)))
	 (eq x (cdr e)) 
	 (return e))))

(defun POSASSQ (x seq)
  (if (null seq)
      () 
      (typecaseq seq 
	(PAIR  (do ((l seq (cdr l)) (e) (i 0 (1+ i)))
		   ((null l) () )
		 (declare (fixnum i))
		 (and (pairp (setq e (car l)))
		      (eq x (car e)) 
		      (return i))))
	 ;; VECTOR-POSASSQ comes in the VECTOR file for MacLISP
	((VECTOR VECTOR-S) (VECTOR-POSASSQ x seq))
	(T (multiple-value (seq) (si:get-primitive-sequence seq 'POSASSQ #T))
	   (posassq x seq)))))

(defun MEMASSQ (x ll)
  (if *RSET (check-type ll #'LISTP 'MEMASSQ))
  (cond ((null ll) () )
	((null (setq x (assq x ll))) () )
	((memq x ll))))

)



