;;;  BACKQ    				-*-Mode:Lisp;Package:SI;Lowercase:T-*-
;;;  *************************************************************************
;;;  ***** MacLISP ****** BACKQuote reader macro function ********************
;;;  *************************************************************************
;;;  ** (c) Copyright 1981 Massachusetts Institute of Technology *************
;;;  *************************************************************************

;;;  NOTE WELL TWO WARNINGS:
;;;	1) comma is defined as a readmacro character all the time, 
;;;	   not just within BACKQUOTE
;;; 	2) A flag is noticed, "BACKQUOTE-EXPAND-WHEN", which if not set to
;;; 	   "READ" will cause the ` readmacro to produce a highly-macroified
;;; 	   program which GRINDEF can parse and print out exactly as read-in.


(herald BACKQ /53)

(include ((lisp) subload lsp))

#-NIL 
(eval-when (eval compile)
     (subload SHARPCONDITIONALS)
     (subload VECTOR)
     )

(eval-when (compile)
   (setq DEFMACRO-FOR-COMPILING ()  DEFMACRO-DISPLACE-CALL () )
)

(defmacro /`SUB-READ (&rest x) 
     ;In order to "bootstrap"-read this file, we must start out using
     ;   maclisp's old reader - when it is fully in, then the definition
     ;   of /`SUB-READ is changed to be SUB-READ
 #-NIL 		 `(READ)		;standard MacLISP case
 #+NIL (progn 
	 #-MacLISP  `(SUB-READ ,.x) 	;standard NIL case
	 #+MacLISP  `(OLD-READ)		;bootstrap case, with NILAID
	))


(declare (special BACKQUOTE-EXPAND-WHEN  |`-,-level/||)
	 (*expr |+ibx-qsequence/|| |+ibx-pairs/||))

#M 
(declare (*fexpr READMACROINVERSE) (special |+ibx-vecp/|| |+ibx-uhunkp/||))
#-Lispm 
(declare (own-symbol |`-expander/|| |`,/|| |`,@/|| |`,./||))

#-NIL 
(defmacro TYPECASEQ (&rest w)
   `(CASEQ (TYPEP ,(car w)) 
      ,.(mapcar '(lambda (x) (cons (sublis '((PAIR . LIST)) (car x)) 
				   (cdr x)))
		(cdr w))))



;;; Readmacro function for backquote
(defun |+INTERNAL-`-macro/|| #-NIL () #N (C S)  
  #N   (and (not (eq s READ-STREAM)) (reader-error s))
       (let* ((|`-,-level/|| (1+ |`-,-level/||))
	      (form (cons '|`-expander/|| (/`sub-read () read-stream)) ))
	     (cond ((or (eq BACKQUOTE-EXPAND-WHEN 'READ) 
			(and (memq BACKQUOTE-EXPAND-WHEN '(EVAL COMPILE)) 
			     (memq COMPILER-STATE '(MAKLAP COMPILE DECLARE))))
		    (macroexpand form))
		   ('T form))))

;;; Readmacro function for comma
(defun |+INTERNAL-,-macro/|| #-NIL () #N (C S)
   #N  (and (not (eq s READ-STREAM)) (reader-error s))
       (and (< |`-,-level/|| 1)
	    (ERROR '|Comma not inside backquote, or too many commas|))
       (let ((|`-,-level/|| (1- |`-,-level/||)))
	    (cons (caseq (tyipeek)
			 (#/@ (tyi) '|`,@/||)
			 (#/. (tyi) '|`,./||)
			 (T 	     '|`,/||))
		  (/`sub-read () read-stream))))


;; Internal backquote expander function 
(defun |+ibx/|| (x)
  (cond ((null x) ''() )
	((typecaseq x
	     (PAIR  (|+ibx-pairs/|| x)) 
	  #N ((VECTOR VECTOR-S) (|+ibx-qsequence/|| x 'VECTOR))
	   ;; ###### Here we could extend it for STRUCTures
	  #N (EXTEND (|+ibx-qsequence/|| x 'EXTEND))
	      ;; ###### Add code here for LISPM and MULTICS vectors!
	     (T (cond 
		   #M ((hunkp x) 
		        (cond ((and |+ibx-vecp/|| (vectorp x)) 
			         ;;Real NIL vectors are atoms, but in MacLISP ?
			        (|+ibx-qsequence/|| x 'VECTOR))
			      ((and |+ibx-uhunkp/|| 
				    (funcall |+ibx-uhunkp/|| x))
			         ;; Well, What do we do with random usrhunks?
			        (list 'QUOTE x))
			     ((|+ibx-qsequence/|| x 'HUNK))))
		 ('T (list 'QUOTE x))))))))



(defun |+ibx-pairs/|| (x) 
   (cond ((eq (car x) '|`,/||) (cdr x))	;Found ",<mumble>"
	 ((eq (car x) '|`-expander/||)	;Recursive ` instance, so
	  (setq x (macroexpand x))	; expand the inner one.  And
	  (|+ibx/|| X))			; now for this level!
	 ((let ((a (car x)) (d (cdr x)) d-is-pairp dqp) 
	     ;;Otherwise look at car and cdr
	    (if (or (memq a '(|`,./|| |`,@/||))
		    (memq (car d) '(|`,./||  |`,@/||)))
		(error '|",@" or ",." in illegal context| x))
	    (cond ((and (pairp a) (memq (car a) '(|`,./|| |`,@/||)))
		    ;;Found ",@<mumble>" or ",.<mumble>"
		   (setq d-is-pairp (pairp (setq d (|+ibx/|| d))))
		   (cond ((and d-is-pairp 
			       (eq (car d) 'QUOTE) 
			       (eq (cadr d) '() ))
			  (cdr a))
			 ('T (setq dqp (if (eq (car a) '|`,@/||)
					   'APPEND 
					   'NCONC)	;else  |`,./||
				   a (cdr a))
			      ;; (NCONC a (NCONC ...)) ==> (NCONC a ...)
			     (cond ((and d-is-pairp (eq (car d) dqp))
				    (list* dqp a (cdr d)))
				   ((list dqp a d))))))
	     ('T (setq a (|+ibx/|| a)) 
		  ;;Standard case is to Tack-To-Front by (CONS A  ...)
		 (setq d-is-pairp (pairp (setq d (|+ibx/|| d)))
		       dqp (and d-is-pairp (eq (car d) 'QUOTE)))
		 (cond ((and dqp (pairp a) (eq (car a) 'QUOTE))
			(list 'QUOTE (cons (cadr a) (cadr d))))
		       ((and dqp (eq (cadr d) '() ))
			(list 'LIST a))
		       ((and d-is-pairp (memq (car d) '(LIST LIST*)))  
			(list* (car d) a (cdr d)))
		       ((list 'LIST* a d)))))))))

(defun |+ibx-qsequence/|| (x constructor-name)
   (do ((i (1- (caseq constructor-name  
		      (VECTOR (vector-length x))
		  #M  (HUNK (hunksize x))
		  #N  (EXTEND (error '|+ibx-qsequence/||)) ))
	   (1- i))
	(z) (element) (constructp))
       ((< i 0) 
	(cond (constructp 
	   #M   (if (eq constructor-name 'HUNK)
		    (setq z (nconc (cdr z) (list (car z)))))
		(cons constructor-name z))
	      ('T (list 'QUOTE x))))
     (declare (fixnum i))
     (setq element (caseq constructor-name 
			  (VECTOR (vref x i))
		     #M   (HUNK (cxr i x))
		     #N   (T (si:xref x i)) ))
     (push (setq element (|+ibx/|| element)) z)
      ;;If no expanded element of the vector is 'evaluable' then it is fully
      ;;  "quotified", and we don't need to construct it up.
     (and element 
	  (typecaseq element
		(PAIR (not (eq (car element) 'QUOTE)))
		(SYMBOL 'T))
	  (setq constructp 'T))))


;;;; MACRO to do the "compilation" into LISP code of the read-in form

(eval-when (compile)
	   (setq DEFMACRO-FOR-COMPILING 'T  
		 DEFMACRO-DISPLACE-CALL MACROEXPANDED )
)

#M (def-or-autoloadable FLUSH-MACROMEMOS DEFMAX)

(defun |+INTERNAL-macro-loser/|| (Y)  
   (ERROR '| -- Internal 'comma' marker found outside 'backquote' context| Y))

;; merely caches the value of (status status USRHUNK) for |`-expander/||
#M (setq |+INTERNAL-macro-loser/|| (status status USRHUNK))

(defmacro |`-expander/|| (&rest x) 
   (declare (special |+INTERNAL-macro-loser/||))
    #-MacLISP  
   (|+ibx/|| x)
    #+MacLISP  
   (let* ((|+ibx-uhunkp/|| (if |+INTERNAL-macro-loser/|| (status USRHUNK)))
	   (|+ibx-vecp/||  (and |+ibx-uhunkp/|| (get 'VECTOR 'VERSION))))
	  (|+ibx/|| X))
)

(mapc '(lambda (x) (putprop x '|+INTERNAL-`-grindmacros/|| 'GRINDMACRO))
      '(|`-expander/||  |`,/||  |`,@/||  |`,./|| ))

#M (setq |+ibx-vecp/|| () ) 

(setq |`-,-level/|| 0)
(setq-if-unbound BACKQUOTE-EXPAND-WHEN 'EVAL)

