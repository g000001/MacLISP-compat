;;;   GENFNS	 					-*-LISP-*-
;;;   **************************************************************
;;;   ***** MACLISP ****** LISP FUNCTIONS ENUMERATOR (GENFNS) ******
;;;   **************************************************************
;;;   ** (C) COPYRIGHT 1979 MASSACHUSETTS INSTITUTE OF TECHNOLOGY **
;;;   ****** THIS IS A READ-ONLY FILE! (ALL WRITES RESERVED) *******
;;;   **************************************************************

;;;  To use this file, simply get a current LISP, load this file in,
;;;	and do (GENFNS).  It will then produce the summary file QIOORD
;;;  Its purpose is to produce a listing of all function names and
;;;	variables in the initial LISP;  for output, it will make the file
;;;	.INFO.;QIOORD verno      	on ITS systems
;;;	<MACLISP>QIOORD.DOC.verno 	on DEC20 systems
;;;	LISP:QIOORD.DOC		 	on DEC10 systems
;;;	DSK:[MAC,LSP]QIOORD.DOC		on the SAIL system
;;;  To write onto another file, do (GENFNS (DEV USR) FN1 FN2)


(DECLARE (SPECIAL * + - ^R ^W BASE QUUX CHRCT OUTFILES
		  UWRITE LINEL ALLSTATUSOPS)
	 (*FEXPR GENFNS)
	 (*EXPR *GENFNS PRINCRUFT PRINPROPS GETVALUES PRIN10 2PRIN10)
	 (*LEXPR LINEL)
	 (FIXNUM I J NL NC MX SP))

(SSTATUS FEATURE NOLDMSG)

(DEFUN GENFNS FEXPR (V)
    (LET ((^R 'T) (^W 'T) (DEFAULTF DEFAULTF))
	 (COND (V 
		(SETQ V (NAMELIST V))
		(APPLY 'UWRITE (CAR V))
		(*GENFNS)
		(APPLY 'UFILE (CDR V)))
	       ((STATUS FEATURE ITS) 
		(UWRITE DSK /.INFO/.)
		(*GENFNS)
		(APPLY 'UFILE `(QIOORD ,(status lispversion))))
	       ((STATUS FEATURE DEC20) 
		(UWRITE DSK MACLISP)
		(*GENFNS)
		(APPLY 'UFILE `((DSK MACLISP) QIOORD DOC ,(status lispversion))))
	       ((COND ((STATUS FEATURE SAIL) (UWRITE DSK (MAC LSP)) 'T)
		      ((STATUS FEATURE DEC10) (UWRITE LISP) 'T))
		(*GENFNS)
		(UFILE QIOORD DOC)))))

(MAPC (FUNCTION (LAMBDA (X) (PUTPROP X 'LAP 'FUNNYFN)))
      '(*DELQ *DELETE *APPEND *TIMES *GREAT *LESS
		 *PLUS *NCONC *APPLY *EVAL *PRINT
		 *PRIN1 *PRINC *TERPRI *TYO GETDDTSYM
		 LAPSETUP/| TTSR/| LH/|
		 SQOZ/| PUTDDTSYM GCPROTECT
		 GETMIDASOP PURCOPY PURIFY
		 FASLAPSETUP/| PAGEBPORG))

(MAPC (FUNCTION (LAMBDA (X) (PUTPROP X 'LAP 'FUNNYVAR)))
      '(*PURE PURCLOBRL PURE TTSR/| LAPSETUP/|
		PUTPROP BPORG BPEND GCPROTECT))


(DEFUN INTERNAL-TEST (X)
   (COND ((GETL X '(FUNNYFN FUNNYVAR)) () )
	 ((GET X 'INTERNAL-TEST))
	 ((OR (AND (EQ (GETCHAR X 1) '+)
		   (MEMQ (GETCHAR X 2) '(I /i))
		   (MEMQ (GETCHAR X 3) '(N /n))
		   (MEMQ (GETCHAR X 4) '(T /t))
		   (MEMQ (GETCHAR X 5) '(E /e))
		   (MEMQ (GETCHAR X 6) '(R /r))
		   (MEMQ (GETCHAR X 7) '(N /n))
		   (MEMQ (GETCHAR X 8) '(A /a))
		   (MEMQ (GETCHAR X 9) '(L /l))
		   (EQ (GETCHAR X 10.) '/-))
	      (LET ((N (FLATC X)))
		   (DO I 1 (1+ I) (> I N)
		       (AND (MEMQ (GETCHAR X I) '(/|  | |  |`| |,|))
			    (RETURN 'T)))))
	  (PUTPROP X 'INTERNAL-TEST 'T)
	  'T)))


(SETQ ALLSTATUSOPS '(
		     + ABBREVIATE ARRAY BPSH BPSL BREAK CHTRAN CLI CRFIL
		     CRUNIT DATE DAYTIME DIVOV DOW EVALHOOK FASLOAD FEATURE
		     FILEMODE FLPDL FLUSH FTV FTVSIZE FTVTITLE FXPDL GCMAX
		     GCMIN GCSIZE GCTIME GCWHO HACTRN HOMEDIR HSNAM ITS JCL
		     JNAME JNUMBER LINMODE LISPVERSION LOSEF MACRO MAR MEMFREE
		     NEWLINE NOFEATURE OSPEED PDL PDLMAX PDLNAMES PDLROOOM
		     PDLSIZE PUNT PURSIZE PURSPACENAMES RANDOM SEGLOG SPCNAMES
		     SPCSIZE SPDL SSTATUS STATUS SUBSYS SYNTAX SYSTEM TABSIZE
		     TERPRI TOPLEVEL TTY TTYCONS TTYINT TTYREAD TTYSCAN TTYSIZE
		     TTYTYPE UDIR UNAME UREAD USERI UUOLINKS UWRITE WHO1 WHO2
		     WHO3 XUNAM _ 
		     ))

(MAPC '(LAMBDA (X) ((LAMBDA (Y) (COND ((CDDDDR Y)	;PDP-10 ONLY
				       (RPLACD (CDDDDR Y) NIL)
				       (PUTPROP (IMPLODE Y) X 'STATUSOP))))
		    (EXPLODEN X)))
      ALLSTATUSOPS)

(DEFUN PRIN10 (X) ((LAMBDA (BASE *NOPOINT) (PRINC X)) 10. T))

(DEFUN 2PRIN10 (X)
	((LAMBDA (BASE *NOPOINT)
		 (PRINC (// X 10.))
		 (PRINC (\ X 10.)))
	 10. T))

(DEFUN PHLATC (F X)
	(COND (F (FLATC X))
	      ((+ 2 (FLATC (CAR X))))))

(DEFUN PRINCRUFT (M L)
    (PROG (MX NL NC AT LL)
       (COND (L (SETQ L (COND ((SETQ AT (ATOM (CAR L)))
			       (SORT L (FUNCTION ALPHALESSP)))
			      ((SORTCAR L (FUNCTION ALPHALESSP)))))
		(TERPRI)
		(TERPRI)
		(PRINC M)
		(TERPRI)
		(TERPRI)
		(SETQ MX (DO ((X L (CDR X))
			      (I 0 (MAX I (PHLATC AT (CAR X)))))
			     ((NULL X) I)))
		(SETQ NL (// LINEL (+ MX 4)))
		(SETQ NC (// (+ (LENGTH L) (1- NL)) NL))
		(DO ((I NL (1- I)))
		    ((PROG2 (SETQ LL (CONS L LL)) (= I 1)))
		    (DO ((J NC (1- J)))
			((OR (NULL L) (ZEROP J)))
			(SETQ L (CDR L))))
		(SETQ LL (NREVERSE LL))
		(DO ((I NC (1- I)) (SP 0 0))
		    ((ZEROP I))
		    (MAP (FUNCTION (LAMBDA (X)
			    (COND ((CAR X)
				(PRINC '|    |)
				(DO ((J SP (1- J)))
				    ((ZEROP J))
				    (PRINC '| |))
				(COND (AT (PRINC (CAAR X)))
				      (T (PRINC (CDAAR X))
					 (PRINC '| |)
					 (PRINC (CAAAR X))))
				(SETQ SP (- MX (PHLATC AT (CAAR X))))
				(RPLACA X (CDAR X))))))
			 LL)
		    (TERPRI))))))


(DEFUN *GENFNS NIL
  (PROG (DATE TIME USRSUBRS USRLSUBRS USRFSUBRS LAPFNS INTFNS STATUSOPS 
	 SYSVARS LAPVARS SYSARRAYS USERAUTOS SYSAUTOS PMFLAG)
	(TERPRI)
	(COND ((STATUS FEATURE ITS) (PRINC '|ITS |))
	      ((STATUS FEATURE DEC20) (PRINC '|TOPS-20//TENEX |))
	      ((STATUS FEATURE SAIL) (PRINC '|SAIL |))
	      ((STATUS FEATURE CMU) (PRINC '|CMU |))
	      ((STATUS FEATURE TOPS-10) (PRINC '|TOPS-10 |)))
	(PRINC '|MacLISP |)
	(PRINC (STATUS LISPVERSION))
	(PRINC '| Functions and Other Features|)
	(TERPRI)
	(PRINC '|This file was created by |)
	(PRINC (STATUS USERID))
	(PRINC '| on |)
	(DO ((X (EXPLODEN (STATUS DOW)) (CDR X))
	     (N 0 40))
	    ((NULL X))
	    (TYO (+ (CAR X) N)))
	(PRINC '|, |)
	(SETQ DATE (STATUS DATE))
	(PRINC (DO ((I (CADR DATE) (1- I))
		    (L '(|January| |February| |March| |April| |May| |June|
			 |July| |August| |September| |October|
			 |November| |December|)
		       (CDR L)))
		   ((= I 1) (CAR L))))
	(PRINC '| |)
	(PRIN10 (CADDR DATE))
	(PRINC '|, 19|)
	(PRIN10 (CAR DATE))
	(SETQ TIME (STATUS DAYTIME))
	(PRINC '| at |)
	(AND (> (CAR TIME) 11.)
	     (RPLACA TIME (- (CAR TIME) 12.))
	     (SETQ PMFLAG T))
	(AND (ZEROP (CAR TIME)) (RPLACA TIME '0))
	(PRIN10 (CAR TIME))
	(PRINC '|:|)
	(2PRIN10 (CADR TIME))
	(COND (PMFLAG (PRINC '| PM|))
	      ((PRINC '| AM|)))
	(TERPRI)
	(SETQ LINEL (LINEL (OR UWRITE TYO)))
	(MAPATOMS (FUNCTION 
		   (LAMBDA (QUUX)
		    ((LAMBDA (F V)
			(MAPCAR (FUNCTION (LAMBDA (P)
				    (COND ((MEMQ P '(SUBR FSUBR LSUBR))
					   (COND ((EQ F 'LAP) (PUSH QUUX LAPFNS))
						 ((INTERNAL-TEST QUUX) (PUSH QUUX INTFNS))
						 ((EQ P 'SUBR) (PUSH QUUX USRSUBRS))
						 ((EQ P 'FSUBR) (PUSH QUUX USRFSUBRS))
						 ((EQ P 'LSUBR) (PUSH QUUX USRLSUBRS)) ))
					  ((EQ P 'VALUE)
					   (COND ((EQ V 'LAP) (PUSH QUUX LAPVARS))
						 (T (PUSH QUUX SYSVARS))))
					  ((EQ P 'ARRAY) (PUSH QUUX SYSARRAYS))
					  ((EQ P 'AUTOLOAD) 
					   (COND ((INTERNAL-TEST QUUX)
						  (PUSH QUUX SYSAUTOS))
						 ((PUSH QUUX USERAUTOS))) ))))
				(STATUS SYSTEM QUUX)))
		(GET QUUX 'FUNNYFN)
		(GET QUUX 'FUNNYVAR)))))
	(MAPC (FUNCTION (LAMBDA (X)
		(AND (APPLY 'STATUS (LIST 'STATUS X))
		     (PUSH (CONS (OR (GET X 'STATUSOP)
				     ((LAMBDA (Y)
					      (COND ((CDDDDR Y)	;PDP-10 ONLY
						     (IMPLODE (APPEND Y '(/  ?))))
						    (T X)))
				      (EXPLODEN X)))
				 (COND ((APPLY 'STATUS
					       (LIST 'SSTATUS X))
					'*)
				       (T '/ )))
			   STATUSOPS))))
	      (STATUS STATUS))
	(PRINCRUFT '|User SUBRs:| USRSUBRS)
	(PRINCRUFT '|User FSUBRs:| USRFSUBRS)
	(PRINCRUFT '|User LSUBRs:| USRLSUBRS)
	(PRINCRUFT '|STATUS options (* = can use with SSTATUS too):| STATUSOPS)
	(PRINCRUFT '|Initial arrays:| SYSARRAYS)
	(PRINPROPS '|Initial User AUTOLOAD properties:|
		   (MAPCAR (FUNCTION (LAMBDA (X)
				(CONS X (GET X 'AUTOLOAD))))
			   USERAUTOS))
	(PRINCRUFT '|LAP and FASLOAD functions:| LAPFNS)
	(PRINCRUFT '|Internal system functions:| INTFNS)
	(PRINPROPS '|Internal system AUTOLOAD properties:|
		   (MAPCAR (FUNCTION (LAMBDA (X)
				(CONS X (GET X 'AUTOLOAD))))
			   SYSAUTOS))
	(PRINPROPS '|System variables:| (GETVALUES SYSVARS))
	(PRINPROPS '|LAP and FASLOAD variables:| (GETVALUES LAPVARS))
))

(DEFUN GETVALUES (X)
       ((LAMBDA (^R ^W * + - OUTFILES DEFAULTF UREAD UWRITE)
		(MAPCAR (FUNCTION (LAMBDA (Y)
			   (CONS Y
				 (COND ((BOUNDP Y)
					(SYMEVAL Y))
				       (T 'UNBOUND)))))
			X))
	NIL NIL '* '+ '- NIL '((DSK LOSER) @ @) NIL NIL))

(DEFUN PRINPROPS (M L)
       (COND (L (SETQ L (SORTCAR L (FUNCTION ALPHALESSP)))
		(TERPRI)
		(TERPRI)
		(PRINC M)
		(TERPRI)
		(TERPRI)
		(PRINC '|    NAME OF ATOM                   INITIAL VALUE|)
		(TERPRI)
		(TERPRI)
		(DO ((X L (CDR X)))
		    ((NULL X))
		  (PRINC '|    |)
		  (PRIN1 (CAAR X))
		  (DO ((I (- 32. (CHARPOS (OR UWRITE T)))  (1- I)))
		      ((NOT (PLUSP I)))
		    (DECLARE (FIXNUM I))
		    (PRINC '| |))
		  (PRINC '|   |)
		  (PRIN1 (CDAR X))
		  (TERPRI)))))

