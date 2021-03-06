;;;   FASLAP 						  -*-LISP-*-
;;;   **************************************************************
;;;   ***** MacLISP ****** (Assembler for compiled code) ***********
;;;   **************************************************************
;;;   ** (C) Copyright 1981 Massachusetts Institute of Technology **
;;;   ****** This is a read-only file! (All writes reserved) *******
;;;   **************************************************************



(SETQ FASLVERNO '#.(let* ((file (caddr (truename infile)))
			   (x (readlist (exploden file))))
			  (setq |verno| (cond ((fixp x) file) ('/392)))))

(EVAL-WHEN (COMPILE) 
     (AND (OR (NOT (GET 'COMPDECLARE 'MACRO))
	      (NOT (GET 'OUTFS 'MACRO)))
	  (LOAD `(,(cond ((status feature ITS) '(DSK COMLAP))
			 ('(LISP)))
		  CDMACS
		  FASL)))
)


;;; This assembler is normally part of the compiler, and produces
;;;   binary (FASL) files suitable for loading with FASLOAD.



(EVAL-WHEN (COMPILE) (COMPDECLARE) (FASLDECLARE) (GENPREFIX |/|fl|) )



(DEFUN FASLVERNO ()
    (PRINC '|/FASLAP Assembler |)
    (PRINC FASLVERNO)
    (PRINC '| |))




(DEFUN FASLIFY (LL FL)
   (PROG (Y)
	 (COND ((EQ FL 'LIST))
	       ((OR (EQ FL 'LAP)
		    (AND (NULL FL) (NOT (ATOM LL)) (EQ (CAR LL) 'LAP)))
		(DO ((Z LL (AND ^Q (READ EOF))) (EOF (LIST ()))) 
		    ((NULL Z) (SETQ LL (NREVERSE (CONS () Y))))
		  (AND (NULL ^Q) 
		       (PROG2 (PDERR CURRENTFN |Has EOF in middle of LAP code|)
			      (ERR 'FASLAP)))
		  (PUSH Z Y)))
	       (FL (SETQ FBARP 'T)
		   (BARF () |FASLIFY is losing|))
	       (T (SETQ Y LL LL ()) (GO B)))
     A	 (AND (NULL LL) (RETURN ()))
	 (SETQ Y (CAR LL))
     B	 (COND ((ATOM Y))			;IGNORE RANDOM ATOMS
	       ((EQ (CAR Y) 'LAP)				;PROCESS LAP
		(SETQ CURRENTFN (CADR Y))
		(FASLPASS1 LL)
		(SETQ LL (FASLPASS2 LL))
		(SETQ FILOC (+ FILOC *LOC))
		(AND (NOT (EQ COMPILER-STATE 'COMPILE))
		     TTYNOTES 
		     (PROG (^W ^R)
			   (INDENT-TO-INSTACK 0)
			   (PRIN1 CURRENTFN)
			   (PRINC '| Assembled|))))
	       ((MUNGEABLE Y) (COLLECTATOMS Y) (BUFFERBIN 14. -1_18. Y))
	       (T (COND ((EQ (CAR Y) 'DECLARE) 
			 (ERRSET (MAPC 'EVAL (CDR Y)) ())
			 (SETQ Y ()))
			((OR (EQ (CAR Y) 'COMMENT) (NOT (EQ (CAR Y) 'QUOTE))))
			((SUBMATCH (CADR Y) '(THIS IS THE LAP FOR))
			 (SETQ Y
			       (AND UNFASLCOMMENTS 
				    (SUBST  (CADDDR (CDDADR Y)) 
					    'DATA 
					    ''(THIS IS THE UNFASL
						    FOR LISP FILE DATA)))))
			((SUBMATCH (CADR Y) '(COMPILED BY LISP COMPILER))
			 (SETQ Y ())))
		  (COND ((AND Y (OR UNFASLCOMMENTS
				    (NOT (MEMQ (CAR Y) '(COMMENT QUOTE)))))
			 ((LAMBDA (^R ^W OUTFILES)
				  (TERPRI)  ;PUT NON-MUNGEABLE INTO UNFASL FILE
				  (COND ((AND (NOT (ATOM Y))
					      (EQ (CAR Y) 'QUOTE))
					 (PRINC '/') (SETQ Y (CADR Y))))
				  (PRIN1 Y) (PRINC '/ ))
			  T T UFFIL)
			 (SETQ UNFASLSIGNIF T)))))
     (SETQ LL (CDR LL))
     (GO A)))


;;; FASLPASS1 PERFORMS PASS 1 PROCESSING FOR A LAP FUNCTION.
;;; THIS INCLUDES DEFINING SYMBOLS, doing the COLLECTATOMS work for
;;;  most address fields [e.g., for xxx in (OP AC xxx IDX)], so that
;;;  the USERATOMS-HOOK wont ever have to cause auotloadings during
;;;  the middle of a function,  AND SAVING VARIOUS PIECES 
;;;  OF INFORMATION FOR PASS 2.

(DEFUN FASLPASS1 (Q)				;Q HAS (LAP FOO SUBR) OR WHATEVER
    ((LAMBDA (BASE IBASE)
	(PROG (AMBIGSYMS N EXPR)
	      (AND (NOT (EQ (CAAR Q) 'LAP)) 
		   (SETQ FBARP 'T)
		   (DBARF Q |Not a LAP listing - FASLPASS1|))
	      (SETQ *LOC 0)
	      (SETQ CURRENTFN (CADAR Q) CURRENTFNSYMS ())
	      (PUSH CURRENTFN ENTRYNAMES)
	      (PUTPROP CURRENTFN FILOC 'ENTRY)
	      (AND UNFASLCOMMENTS (NOTE-IN-UNFASL FILOC (CAR Q) ()))			;Tells about entry points
	      (DO Z (CDR Q) (CDR Z) (COND ((NULL Z) 
					   (DBARF () |No () [or "NIL"] in LAP code - FASLPASS1|)
					   (SETQ FBARP 'T))
					  ((NULL (SETQ EXPR (CAR Z)))))
		  (COND ((ATOM EXPR) 
			 (FASLDEFSYM EXPR (LIST 'RELOC (+ FILOC *LOC))))
			((EQ (CAR EXPR) 'ENTRY)
			 (COND ((GET (CADR EXPR) 'ENTRY)
				(PDERR CURRENTFN |Multiple ENTRY with duplicated name|)
				(ERR 'FASLAP))
			       (T (PUSH (CADR EXPR) ENTRYNAMES)
				  (PUTPROP (CADR EXPR) (SETQ DATA (+ FILOC *LOC)) 'ENTRY)
				  (AND UNFASLCOMMENTS
				       (NOTE-IN-UNFASL DATA EXPR () )))))
			((EQ (CAR EXPR) 'DEFSYM)		;DEFSYM
			 (DO X (CDR EXPR) (CDDR X)		;SO DEFINE THE SYMBOLS
				(NOT (AND X (CDR X)))		;NOTE THAT EVAL IS USED,
			     (FASLDEFSYM (CAR X) (EVAL (CADR X)))))	; NOT FASLEVAL
			((EQ (CAR EXPR) 'DDTSYM)		;DECLARE DDT SYMBOLS
			 (SETQ DDTSYMP T)			;REMEMBER THAT THIS FN HAD DDTSYM
			 (MAPC (FUNCTION *DDTSYM) (CDR EXPR)))	;TRY TO GET THEM FROM DDT
			((EQ (CAR EXPR) 'EVAL)			;EVALUATE RANDOM FROBS
			 (MAPC (FUNCTION EVAL) (CDR EXPR)))
			((EQ (CAR EXPR) 'SYMBOLS)		;SYMBOLS - FOR NOW, JUST
			 (SETQ SYMBOLSP T))			; REMEMBER THAT ONE HAPPENED
			((MEMQ (CAR EXPR) '(SIXBIT ASCII BLOCK))	;HAIRY BLOBS
			 (SETQ *LOC (+ *LOC (SETQ N (BLOBLENGTH EXPR)))))
			((NOT (MEMQ (CAR EXPR) '(COMMENT ARGS)))
			 (RECLITCOUNT EXPR T)
			 (SETQ *LOC (1+ *LOC)))))
	      (SETQ LITLOC *LOC)		;REMEMBER WHERE TO ASSEMBLE LITERALS
	      (SETQ LITERALS (NREVERSE LITERALS))))
	8. 8.))



(DEFUN RECLITCOUNT (insn PASS1P)	
    ;;On pass 1, merely ascertain number of code words using literals, and
    ;; check the COLLECTATOMS problem
   (COND ((AND (CDDR insn)
	       (SETQ insn (COND ((OR (EQ (CADDR insn) '/@)
				     (EQ (CADR insn) '/@))
				 (CADDDR insn))
				((CADDR insn))))
	        ;; Note that this lets HUNKs go thru
	       (NOT (ATOM insn)))
	  (COND ((NOT (EQ (CAR insn) '%))
		 (cond ((or (memq (car insn) '(QUOTE FUNCTION SPECIAL ARRAY 
						     EVAL SQUID))
			    (eq (car insn) SQUID))
			(collectatoms (cadr insn))))
		 0)
		((LAPCONST (CDR insn)) 0)
		(PASS1P 
		   ;;On pass1, not really interested in count
		  (and (not (eq pass1p 'COLLECTATOMS))
		       (PUSH (CDR insn) LITERALS))
		  (reclitcount (cdr insn) 'COLLECTATOMS)
		  0)
		((MEMQ (CADR insn) '(SIXBIT ASCII BLOCK))
		 (BLOBLENGTH (cdr insn)))		      
		((1+ (RECLITCOUNT (cdr insn) () )))))
	 (0)))



;;; FASLPASS2 PERFORMS PASS 2 PROCESSING FOR A LAP FUNCTION.
;;; THIS INCLUDES RETRIEVING INFORMATION SAVED ON PASS 1
;;; (IN PARTICULAR SYMBOLS), HANDLING DDT SYMBOLS TO BE
;;; RETRIEVED AT LOAD TIME, PROCESSING LITERALS, DEFINING
;;; ENTRY POINTS TO THE LOADER, AND OF COURSE CONVERTING
;;; INSTRUCTIONS TO BINARY CODE. THE FUNCTION MAKEWORD IS
;;; CALLED TO PROCESS INDIVIDUAL LAP STATEMENTS.

(DEFUN FASLPASS2 (Q)			;Q HAS LAP LISTING
    ((LAMBDA (BASE IBASE LITCNT)
	(PROG (DDTSYMS AMBIGSYMS LASTENTRY ENTRYPOINTS LITERALP 
		UNDEFSYMS OLOC EXPR OLITERALS LL N TEM)
	      (SETQ OLITERALS LITERALS OLOC *LOC *LOC 0)
	      (COLLECTATOMS (CDR (SETQ EXPR (CAR Q))))		;MUST COLLECT NAME AND TYPE OF SUBR
	      (PUSH (CONS (CONS (CADR EXPR) (CADDR EXPR)) (GET CURRENTFN 'ENTRY)) 
		    ENTRYPOINTS)				;SAVE ENTRY POINT INFO
	      (COND ((GET CURRENTFN 'SYMBOLSP)			;SYMBOLS PSEUDO ANYWHERE MAKES ENTRY DEFINED
		     (BUFFERBIN 13. 0 CURRENTFN)))			; - OUTPUT AS DDT SYMBOL
	      (SETQ LASTENTRY CURRENTFN)
	      (DO Z (CDR Q) (CDR Z) (COND ((NULL (SETQ EXPR (CAR Z)))
					   (SETQ LL Z)
					   T))
		  (COND ((ATOM EXPR)					;MAYBE A TAG SHOULD BE
			 (COND (SYMBOLSP (BUFFERBIN 13. 0 EXPR))))	; OUTPUT AS A DDT SYMBOL
			((EQ (CAR EXPR) 'ENTRY)				;ENTRY POINT
			 (COND ((NOT (= (SETQ N (+ FILOC *LOC)) 
					(GET (CADR EXPR) 'ENTRY)))	;BETTER BE AT
				(BARF (CADR EXPR) |Phase screw at ENTRY - FASLPASS2|)))
			 (COLLECTATOMS (CDR EXPR))			;COLLECT NAME AND TYPE
			 (PUSH (CONS (CONS (CADR EXPR)			;SAVE INFO ABOUT ENTRY
					   (COND ((CDDR EXPR)
						  (CADDR EXPR))
						 ((CADDAR Q))))
				     N)
			       ENTRYPOINTS)
			 (AND SYMBOLSP (BUFFERBIN 13. 0 (CADR EXPR)))
			 (SETQ LASTENTRY (CADR EXPR)))
			((EQ (CAR EXPR) 'ARGS)					;ARGS DECLARATION
			 (COND ((EQ (CADR EXPR) LASTENTRY)			;SHOULD BE JUST AFTER ENTRY
				(PUTPROP (CADR EXPR) (CADDR EXPR) 'ARGSINFO))	;SAVE INFO
			       ('T (COND ((GET (CADR EXPR) 'ENTRY)		;TWO WAYS TO BARF AT LOSER
					  (PDERR EXPR |Misplaced ARGS info|))
					 ((PDERR EXPR |Function not seen for this info|)))
				   (ERR 'FASLAP)) ))
			((EQ (CAR EXPR) 'SYMBOLS)		;TURN DDT SYMBOLS OUTPUT
			 (SETQ SYMBOLSP (CADR EXPR)))		; SWITCH ON OR OFF
			((EQ (CAR EXPR) 'EVAL)			;EVALUATE RANDOM FROBS
			 (MAPC (FUNCTION EVAL) (CDR EXPR)))
			((EQ (CAR EXPR) 'DDTSYM)		;SAVE DDTSYMS TO PUT
			 (MAPC '(LAMBDA (X) (AND (NOT (MEMQ X DDTSYMS)) (PUSH X DDTSYMS)))
			       (CDR EXPR)))
			((NOT (MEMQ (CAR EXPR) '(DEFSYM COMMENT))) (MAKEWORD EXPR))))

	      (AND (OR LITERALS (NOT (= *LOC LITLOC))) (GO PHAS))
	      (SETQ LITERALP T)		;THIS LETS FASLEVAL KNOW WE'RE DOING LITERALS
	      (MAPC (FUNCTION MAKEWORD) OLITERALS)	;SO ASSEMBLE ALL THEM LITERALS
	      (AND (NOT (= *LOC (+ LITLOC LITCNT))) (GO PHAS))
	      (MAPC '(LAMBDA (X) 
			(SETQ TEM (GET (CAAR X) 'ARGSINFO))
			(BUFFERBIN 11. (BOOLE 7 (LSH (ARGSINFO (CAR TEM)) 27.)
					        (LSH (ARGSINFO (CDR TEM)) 18.) 
						(CDR X))
				       (CAR X)))
		    ENTRYPOINTS)
	      (AND DDTSYMS						;BARF ABOUT DDT SYMBOLS
		   (COND ((NULL DDTSYMP)
			  (WARN DDTSYMS |Undefined symbols - converted to DDT symbols|))
			 ((WARN DDTSYMS |DDT symbols|))))
	      (AND UNDEFSYMS (PROG2 (PDERR UNDEFSYMS |Undefined symbols|) 
				    (ERR 'FASLAP)))
	      (REMPROPL 'SYM CURRENTFNSYMS)
	      (REMPROPL 'SYM DDTSYMS)
	      (MOBYSYMPOP SYMPDL)			;RESTORE DISPLACED SYMBOLS
	      (RETURN LL)					;NORMAL EXIT
	PHAS  (BARF () |Literal phase screw|)))
	8. 8. 0))	

(DEFUN ARGSINFO (X) (COND ((NULL X) 0) ((= X 511.) X) ((1+ X))))

;;; FASLEVAL IS ONLY USED BY MAKEWORD, TO EVALUATE THE
;;; FIELDS OF A LAP INSTRUCTION.

(DEFUN FASLEVAL (X)			;EVALUATE HAIRY FASLAP EXPRESSION
	(COND ((NUMBERP X) X)		;A NUMBER IS A NUMBER IS A NUMBER
	      ((ATOM X)
	       (COND ((EQ X '*) (LIST 'RELOC (+ FILOC *LOC)))	;* IS THE LOCATION COUNTER
		     ((GET X 'GLOBALSYM))			;TRY GETTING GLOBARSYM PROP
		     ((GET X 'SYM))				;TRY GETTING SYM PROPERTY
		     ((OR (NULL X) (MEMQ X UNDEFSYMS)) 0)	;0 FOR LOSING CASES
		     (((LAMBDA (Y) (AND Y (PUTPROP X Y 'SYM))) (GETMIDASOP X)))
		     ((NULL DDTSYMP)				;MAYBE CAN PASS THE BUCK ON
		      (PUSH X DDTSYMS)				; TO FASLOAD (IT WILL GET
		      (*DDTSYM X))				; SYMBOL FROM DDT WHEN LOADING)
		     (T (PUSH X UNDEFSYMS) 0)))			;OH, WELL, GUESS IT'S UNDEFINED
	      ((EQ (CAR X) 'QUOTE) 
		(COND ((ATOM (CADR X)) X)
		      ((EQ (CAADR X) SQUID)
			(COND ((EQ (CADR (SETQ X (CADR X))) MAKUNBOUND)
			       '(0 (() 34)))
			      (X)))
		      ((EQ (CDADR X) GOFOO) (LIST 'EVAL (CAADR X)))
		      (X)))
	      ((OR (MEMQ (CAR X) '(SPECIAL FUNCTION ARRAY)) (EQ (CAR X) SQUID))
	       X)
	      ((EQ (CAR X) 'EVAL) (CONS SQUID (CDR X)))
	      ((EQ (CAR X) '%)
	       (COND ((NOT (= FSLFLD 1))		  	;LITERALS MUST BE IN ADDRESS FIELD
		      (PDERR X |Literal not in address field|)
		      (ERR 'FASLAP))
		     ((LAPCONST (CDR X)))			;MAYBE IT'S A LAP CONSTANT
		     ((NOT LITERALP)
		      (SETQ LITERALS (CDR LITERALS))		;KEEPING COUNT OF THE NUMBER OF LITERALS
		      ((LAMBDA (RLC)
			       (SETQ LITCNT 
				     (+ LITCNT 
					(COND ((MEMQ (CADR X) '(SIXBIT ASCII BLOCK))
						(BLOBLENGTH (CDR X)))
					      ((ZEROP (RECLITCOUNT (CDR X) ())) 1)
					      (T (SETQ RLC (+ RLC (RECLITCOUNT (CDR X) ())))
						 (- RLC LITCNT -1)))))
			       (LIST 'RELOC (+ FILOC LITLOC RLC))) 
			  LITCNT))
		    ((PROG2 () 				;HO! HO! HO! YOU THINK THIS WILL WORK??
			    (FASLEVAL '*)
			    (MAKEWORD (CDR X))))))
	      ((MEMQ (CAR X) '(ASCII SIXBIT))			;A WORD OF ASCII
		 (CAR (PNGET (CADR X) 
			     (COND ((EQ (CAR X) 'ASCII) 7) (6)))))	;OR OF SIXBIT
	      ((EQ (CAR X) 'SQUOZE)				;A WORD OF SQUOZE [MAY BE EITHER
	       (SQOZ/| (CDR X)))				; (SQUOZE SYMBOL) OR (SQUOZE # SYMBOL)]
	      ((EQ (CAR X) '-)					;SUBTRACTION (OR MAYBE NEGATION)
	       (COND ((NULL (CDDR X))
		      (FASLMINUS (FASLEVAL (CADR X))))
		     ((FASLDIFF (FASLEVAL (CADR X))
				(FASLEVAL (CDDR X))))))
	      ((EQ (CAR X) '+)					;ADDITION
	       (FASLPLUS (FASLEVAL (CADR X))
			 (FASLEVAL (CDDR X))))
	      ((CDR X) (FASLPLUS (FASLEVAL (CAR X))		;A RANDOM LIST GETS ADDED UP
				 (FASLEVAL (CDR X))))
	      ((FASLEVAL (CAR X)))))				;SUPERFLUOUS PARENS - RE-FASLEVAL

;;; THE VALUE OF FASLEVAL IS ONE OF THE FOLLOWING FROBS:
;;; 	<NUMBER>			A NUMBER
;;;	(<NUMBER> -GLITCHES-)		NUMBER (PLUS GLITCHES)
;;;	(RELOC <NUMBER> -GLITCHES-)	RELOCATABLE VALUE (PLUS GLITCHES)
;;;	(SPECIAL <ATOM>)		REFERENCE TO VALUE CELL
;;;	(QUOTE <S-EXPRESSION>)		S-EXPRESSION CONSTANT
;;;	(FUNCTION <ATOM>)		REFERENCE TO FUNCTION [SAME AS (QUOTE <ATOM>)]
;;;	(ARRAY <ATOM>)			REFERENCE TO ARRAY POINTER
;;;	FOO				RESULT OF INVALID ARGS TO FASLEVAL
;;;
;;; A "GLITCH" IS ONE OF THE FOLLOWING:
;;;	(() <NUMBER> . <SIGN>)		GLOBALSYM [<NUMBER> INDICATES WHICH ONE]
;;;	(<SQUOZE> () . <SIGN>)		DDT SYMBOL, VALUE UNKNOWN [<SQUOZE> IS A NUMBER]
;;;	(<SQUOZE> <VALUE> . <SIGN>)	DDT SYMBOL, VALUE KNOWN TO DDT ABOVE FASLAP
;;; <SIGN> IS EITHER - FOR NEGATIVE OR () FOR POSITIVE.
;;;
;;; FASLPLUS, FASLMINUS, AND FASLDIFF ARE USED TO PERFORM ARITHMETIC ON THESE FROBS.
;;; NO ARITHMETIC CAN BE PERFORMED ON THE SPECIAL, QUOTE, FUNCTION, ARRAY, AND FOO FROBS.
;;; ARITHMETIC CAN BE PERFORMED ON ALL THE OTHERS, EXCEPT THAT ONE CANNOT CREATE
;;; A NEGATIVE RELOC FROB, I.E. ONE CAN SUBTRACT A RELOC FROM A RELOC, BUT NOT
;;; A RELOC FROM AN ABSOLUTE.

(DEFUN FASLPLUS (K Q)				;ADD TWO FROBS
	(COND ((NUMBERP K)
	       (COND ((NUMBERP Q) (+ K Q))
		     ((EQ (CAR Q) 'RELOC)
		      (CONS 'RELOC (CONS (+ K (CADR Q)) (CDDR Q))))
		     ((NUMBERP (CAR Q))
		      (CONS (+ K (CAR Q)) (CDR Q)))
		     ('FOO)))
	      ((EQ (CAR K) 'RELOC)
	       (COND ((NUMBERP Q)
		      (CONS 'RELOC (CONS (+ Q (CADR K)) (CDDR K))))
		     ((NUMBERP (CAR Q))
		      (CONS 'RELOC (CONS (+ (CAR Q) (CADR K))
				   (APPEND (CDR Q) (CDDR K)))))
		     ('FOO)))
	      ((NUMBERP (CAR K))
	       (COND ((NUMBERP Q)
		      (CONS (+ Q (CAR K)) (CDR K)))
		     ((EQ (CAR Q) 'RELOC)
		      (CONS 'RELOC (CONS (+ (CAR K) (CADR Q))
				   (APPEND (CDR K) (CDDR Q)))))
		     ((NUMBERP (CAR Q))
		      (CONS (+ (CAR K) (CAR Q))
			    (APPEND (CDR K) (CDR Q))))
		     ('FOO)))
	      ('FOO)))

(DEFUN FASLDIFF (K Q)				;SUBTRACT TWO FROBS
	(COND ((NUMBERP K)
	       (COND ((NUMBERP Q) (- K Q))
		     ((NUMBERP (CAR Q))
		      (CONS (- K (CAR Q)) (FASLNEGLIS (CDR Q))))
		     ('FOO)))
	      ((EQ (CAR K) 'RELOC)
	       (COND ((NUMBERP Q)
		      (CONS 'RELOC (CONS (- (CADR K) Q) (CDDR K))))
		     ((EQ (CAR Q) 'RELOC)
		      (CONS (- (CADR K) (CADR Q))
			    (APPEND (CDDR K) (FASLNEGLIS (CDDR Q)))))
		     ((NUMBERP (CAR Q))
		      (CONS 'RELOC
			    (CONS (- (CADR K) (CAR Q))
				  (APPEND (CDDR K)
					  (FASLNEGLIS (CDR Q))))))
		     ('FOO)))
	      ((NUMBERP (CAR K))
	       (COND ((NUMBERP Q)
		      (CONS (- (CAR K) Q) (CDR K)))
		     ((NUMBERP (CAR Q))
		      (CONS (- (CAR K) (CAR Q))
			    (APPEND (CDR K) (FASLNEGLIS (CDR Q)))))
		     ('FOO)))
	      ('FOO)))

(DEFUN FASLMINUS (Q)				;NEGATE A FROB
	(COND ((NUMBERP Q) (- Q))
	      ((NUMBERP (CAR Q))
	       (CONS (- (CAR Q)) (FASLNEGLIS (CDR Q))))
	      ('FOO)))

(DEFUN FASLNEGLIS (K)				;NEGATES A LIST OF GLITCHES
	(MAPCAR (FUNCTION (LAMBDA (Q)
			(CONS (CAR Q)
			      (CONS (CADR Q)
				    (COND ((CDDR Q) ())
					  ('-))))))
		K))

;;; LAPCONST IS A "SEMI-PREDICATE" WHICH WHEN APPLIED TO THE CDR
;;; OR A LITERAL DETERMINES WHETHER OR NOT IT IS ONE OF A NUMBER
;;; OF SPECIAL "LAP CONSTANTS" WHICH ARE DEFINED IN LISP (IN A
;;; TABLE AT LOCATION R70) SINCE COMPILED CODE USES THEM SO OFTEN.
;;; IF NOT, IT RETURNS (); IF SO, IT RETURNS A FASLEVAL FROB
;;; INDICATING A REFERENCE TO R70 AS A GLOBALSYM.

(DEFUN LAPCONST (X)					;SPECIAL LAP CONSTANTS ARE
    (COND ((NOT (SIGNP E (CAR X))) 
	   (AND (NULL (CDR X)) (LAPC1 (CAR X))))	;(% '()), (% FIX1), OR (% FLOAT1)
	  ((NULL (CDR X)) '(0 (() -1)))		;(% 0) OR (% 0.0)
	  ((OR  (NOT (FIXP (CADR X)))
		(NOT (= (CADR X) 0)) 
		(NULL (SETQ X (CDDR X))))
	    ())
	  ((NULL (CDR X)) (LAPC1 (CAR X)))		;(% 0 0 '()), (% 0 0 FIX1), OR (% 0 0 FLOAT1)
	  ((AND (FIXP (CAR X))
		(< (CAR X) 16. )
		(> (CAR X) 0)
		(FIXP (CADR X))
		(= (CAR X) (CADR X)))
	     (LCA (CAR X)))))			;(% 0 0 N N)  FOR 0 < N < 16.

(DEFUN LAPC1 (X)
    (COND ((EQ X 'FIX1) '(-2 (() -1)))
	  ((EQ X 'FLOAT1) '(-1 (() -1)))
	  ((AND (EQ (TYPEP X) 'LIST) (EQ (CAR X) 'QUOTE) (EQ (CADR X) '()) 
	   '(0 (() -1))))))



 
;;; ATOMINDEX is used to retrieve the index of an atom (this
;;; index must have been previously defined by COLLECTATOMS).
;;; Symbol atoms have ATOMINDEX properties; indices of
;;; numbers are kept in a hash table called NUMBERTABLE.

(eval-when (eval compile)
  (setq useratoms-non-types '(LIST SYMBOL FIXNUM FLONUM BIGNUM))
    ;; memorize x as a user-atom we've collected.  Gets
    ;; (atom . index) as the argument
  (defmacro USERATOMS-INTERN (x)
    `(PUSH ,x USERATOMS-INTERN))
    ;; get the user-atom x's atomindex, or nil if it doesn't have one
  (defmacro USERATOMS-LOOKUP (x)
    `(CDR (ASSQ ,x USERATOMS-INTERN)))
  )


(DEFUN ATOMINDEX (X TYPE)
   (let ((user-index (if (not (memq type '#.useratoms-non-types))
			 (useratoms-lookup x))))
      (cond ((not (null user-index)) user-index)
	    ((null x) 0)
	    (T (and (null type) (setq type (typep x)))
	       (setq type (cond ((eq type 'symbol) (get x 'atomindex))
				((not (memq type '(fixnum flonum bignum))) ())
				((cdr (hassocn x type)))))
	       (and (null type) (barf x |Atomindex screw|))
	       type))))



;; COLLECTATOMS finds all atoms in an s-expression and assigns an atomindex
;; to each one which doesn't already have one. These index assignments are also
;; output into the binary file. It is through these indices that s-expressions
;; are described to the loader.

;; The hook USERATOMS-HOOKS if non-null should be a list of function to invoke
;; on each object being COLLECTATOMSed.  If one returns non-null, the return
;; value should be the NCONS of the form to be EVAL'd to create the frob.
;;
;; See also ATOMINDEX

(defun COLLECTATOMS (x)
   (do ((user-object nil nil)
	(type) (marker))
       ((null x))
     (cond ((null x) (return () ))  ;() is always pre-collected
	   ((eq (setq type (typep x)) 'LIST)
	    (collectatoms (car x))
	    (setq x (cdr x)))	   ;Loop until no more
	   ((eq type 'SYMBOL)
	    (cond ((null (get x 'ATOMINDEX))
		   (push x allatoms)
		   (cond ((setq marker
				(getl x '(+INTERNAL-STRING-MARKER
					  +INTERNAL-TEMP-MARKER)))
			  (setq user-object  ;code to generate uninterned sym!
				`(pnput ',(pnget x 7) nil))
			  (collectatoms user-object)
			  (setq user-object
				`(,useratoms-intern-frob
				  ,user-object
				  ,x . ,(setq atomindex (1+ atomindex))))
			  (bufferbin 14. -2_18. user-object)
			  (putprop x (cdddr user-object) 'ATOMINDEX)
			  (cond ((eq (car marker) '+INTERNAL-STRING-MARKER)
				 (setq user-object  ;Self-evaling, with marker
				       `(setq ,x ',x))
				 (collectatoms user-object)
				 (bufferbin 14. -1_18. user-object)))
			  (cond (user-string-mark-in-fasl 
				 (setq user-object 
				       `(DEFPROP ,x T ,(car marker)))
				 (collectatoms user-object)
				 (bufferbin 14. -1_18. user-object))))
			 ('T (putprop x 
				      (setq atomindex (1+ atomindex))
				      'atomindex)
			     (bufferbin 10. 0 x)))))
	    (return () ))
	   ((memq type '(FIXNUM FLONUM BIGNUM))
	    (let ((bkt (hassocn x type)))
		 (cond ((null (cdr bkt))
			(setq atomindex (1+ atomindex))
			(rplacd bkt (list (cons type (cons x atomindex))))
			(bufferbin 10. 0 x))))
	    (return () ))
	   ;; Someday, it may be that we want to allow ordinary MacLISP
	   ;;  data types to be filtered thru this USERATOMS-HOOK, and the
	   ;;  next two clauses will have to be moved up to the beginning of 
	   ;;  this COND then;  but for now, it is verrrry slow.
	   ((useratoms-lookup x) (return () ))	;Don't repeat
	   ((and useratoms-hooks
		 (do ((hooks useratoms-hooks (cdr hooks)))
		     ((or (null hooks)
			  (setq user-object (funcall (car hooks) x)))
		      user-object)))
	     ;;Hunks will generally have a symbol in their CXR 1
	    (and (not (atom user-object)) (collectatoms (car user-object)))
	    (useratoms-intern `(,x . ,(setq atomindex (1+ atomindex))))
	    (bufferbin 14. -2_18.
		       `(,useratoms-intern-frob ,(car user-object)
			   ,x . ,atomindex))
	    (return () ))	   ;No more
	   ((hunkp x)
	    (do i (1- (hunksize x)) (1- i) (< i 0)
		(collectatoms (cxr i x)))
	    (return () ))
	   (T (barf x |Unrecognizable datum -- Collectatoms|)))))


(DEFUN HASSOCN (X TYPE)
    (PROG (BKT OBKT FIXFLOP I)
	  (SETQ FIXFLOP (MEMQ TYPE '(FIXNUM FLONUM)))
	  (SETQ I (\ (ABS (SXHASH X)) 127.))
	  (AND (MINUSP I) (SETQ I 0))
	  (SETQ OBKT (NUMBERTABLE I))
	A (COND ((NULL (SETQ BKT (CDR OBKT)))
		 (RETURN (COND (OBKT) 		        ;RETURN (<MUMBLE> . ())
			       ((STORE (NUMBERTABLE I)
				       (LIST ()))))))	;THE "LAST" OF A BKT
		((NOT (EQ TYPE (CAAR BKT))))
		((COND ((NOT FIXFLOP) (EQUAL X (CADAR BKT)))
		       (T (= X (CADAR BKT))))
		 (RETURN (CDAR BKT))))			;RETURN (N . INDEX)
	  (SETQ OBKT BKT)
	  (GO A)))

;;; FASLDEFSYM IS USED TO DEFINE SYMBOLS; IT ALSO CHECKS FOR VARIOUS
;;; ERRORS, INCONSISTENCIES, AND AMBIGUITIES.

(DEFUN FASLDEFSYM (SYM VAL)				;DEFINE A SYMBOL
	(PROG (Z)
	      (COND ((GET SYM 'GLOBALSYM) 
		     (PDERR SYM |Cant redefine a GLOBALSYM - FASLDEFSYM|)
		     (ERR 'FASLAP))
		    ((SETQ Z (GET SYM 'SYM))		;MAYBE IT'S ALREADY DEFINED?
		     (COND ((EQUAL Z VAL) (RETURN Z))	;REDEFINING TO SAME VALUE DOESN'T HURT
			   ((NOT (MEMQ SYM AMBIGSYMS))	;ELSE IT IS AN AMBIGUOUS SYMBOL
			    (PUSH SYM AMBIGSYMS)	;OH, WE'LL REDEFINE IT, ALL RIGHT,
			    (AND (NOT (MEMQ SYM CURRENTFNSYMS))	; BUT WE'LL ALSO BARF
				 (SETQ MAINSYMPDL (PUSH (CONS SYM Z) SYMPDL))))))
		    (T (PUSH SYM CURRENTFNSYMS)))
	      (RETURN (PUTPROP SYM VAL 'SYM))))		;SO DEFINE THE SYMBOL (MUST RETURN THE VALUE)

(DEFUN BLOBLENGTH (X)				;DETERMINES LENGTH OF A BLOB
       (COND ((EQ (CAR X) 'SIXBIT)		;SIXBIT
	      (// (+ 5 (FLATC (CADR X))) 6))
	     ((EQ (CAR X) 'ASCII)		;ASCII (actually, ASCIZ)
	      (1+ (// (FLATC (CADR X)) 5)))
	     ((NUMBERP (SETQ DATA (CADR X))) 	;MUST BE BLOCK - ACCEPT NUMBER
	      DATA )
	     ((AND (SYMBOLP DATA)		;ACCEPT SYMBOL With numeric VAL
		   (NUMBERP (SETQ DATA (GET DATA 'SYM))))
	      DATA)
	     (T (PDERR X |Undefined arg for block expression|)
		(ERR 'FASLAP) )))

(DEFUN SUBMATCH (X Y)	;"true" IFF LIST Y IS A PREFIX OF LIST X
    (DO ((X X (CDR X)) (Y Y (CDR Y)))
	((NULL Y) T)
      (AND (NULL X) (RETURN ()))			;X WAS TOO SHORT
      (AND (NOT (EQ (CAR X) (CAR Y))) (RETURN ()))))	;THEY DONT MATCH

(DEFUN MUNGEABLE    (X)		;SHOULD RANDOM S-EXPR BE PUT IN BINARY FILE
	(NOT (OR (MEMQ (CAR X) '(QUOTE COMMENT DECLARE))	;NOT IF QUOTED OR COMMENT
		 (AND (EQ (CAR X) 'EVAL)		;NOT IF (EVAL 'FOO)
		      (EQ (TYPEP (CADR X)) 'LIST)	; (THIS GIVES US A HOOK TO
		      (EQ (CAADR X) 'QUOTE)))))		; AVOID MUNGING IF DESIRED)

(DEFUN MOBYSYMPOP (L)
    (DO X L (CDR X) (NULL X)
	(PUTPROP (CAAR X) (CDAR X) 'SYM)))

;;; LISTOUT OUTPUTS AN S-EXPRESSION AS A SEQUENCE OF LIST-SPECS.
;;; EACH LIST-SPEC MAY BE AS FOLLOWS:
;;;	     0,,N	THE ATOM WHOSE ATOMINDEX IS N
;;;	100000,,N	LISTIFY THE LAST N ITEMS, TO CREATE A NEW ITEM
;;;	200000,,N	MAKE A DOTTED LIST OUT OF THE LAST N+1 ITEMS
;;;	300000,,0	MERELY EVALUATE THE TOP THING ON THE STACK
;;;	7XXXXD,,INS	TERMINATE, D IS INFORMATION DIGIT, INS MAY BE 
;;;			THE LH OF THE INSTRUCTION FOR A TYPE 5 WORD
;;; LISTOUT DOES NOT GENERATE THE TERMINATION WORD

(defun LISTOUT (x)
  (let* ((type (typep x))
	 (index (if (not (memq type '#.useratoms-non-types))
		    (useratoms-lookup x))))
       (cond ((not (null index)) (faslout index))
	     ((eq type 'RANDOM) 
	      (barf *LOC |Relative location of QUOTE randomness|))
	     ((and (eq type 'LIST) 
		   (or (eq (car x) SQUID)
		       (eq (car x) useratoms-intern-frob)))
	      (setq squidp 'T)
	      (listout (cadr x))
	      (and (eq (car x) SQUID) (faslout 3_33.)))
	     ((EQ TYPE 'LIST)
	      (DO ((I 0 (1+ I)) (Y X (CDR Y)) (N 0))
		  ((COND ((NULL Y) 
			  (SETQ N 1_33.)    ;FASL code to make up standard LIST
			  'T)		    ;  terminating in the null list
			 ((OR (NOT (PAIRP Y)) (EQ (CAR Y) SQUID))
			  (LISTOUT Y)	    ;Output the non-() list terminator
			  (SETQ N 2_33.)    ; and signal FASL code for 
			  'T))		    ; non-standard list.
		   (FASLOUT (BOOLE 7 I N))) ;<typ-cod>_15.,,<length>
		 (LISTOUT (CAR Y))))
	     ((HUNKP X)
	      (DO ((I 1 (1+ I)) (N (HUNKSIZE X)))
		  ((NOT (< I N))
		   (LISTOUT (CXR 0 X))
		   (FASLOUT (BOOLE 7 4_33. N)))
		  (LISTOUT (CXR I X))))
	     ('T (FASLOUT (ATOMINDEX X TYPE))) )))

;;; BUFFERBIN TAKES TWO ARGUMENTS: A NUMBER, WHICH IS THE
;;; RELOCATION TYPE, AND SOME OBJECT. THE FORMAT OF THIS SECOND
;;; OBJECT DEPENDS ON THE TYPE, AS FOLLOWS:
;;; #	TYPE		FORMAT OF SECOND AND THIRD OBJECTS
;;; 0	ABSOLUTE	<FIXNUM>
;;; 1	RELOCATABLE	<FIXNUM>
;;; 2	SPECIAL		<FIXNUM>
;;; 3	SMASHABLE CALL	<FIXNUM>
;;; 4	QUOTED ATOM	<FIXNUM>	ATOM
;;; 5	QUOTED LIST	<FIXNUM> 	<LIST>
;;; 6	GLOBALSYM	<FIXNUM>
;;; 7	GETDDTSYM	<SQUOZE-VAL>	<() OR FIXNUM>
;;; 8	ARRAY REFERENCE	<ATOMINDEX>
;;; 9	[UNUSED]
;;; 10.	ATOMINDEX INFO	0		<ATOM>
;;; 11.	ENTRY INFO	ARGSINFO	(<NAME> . <TYPE>)
;;; 12.	LOC 		<FIXNUM>
;;; 13.	PUTDDTSYM	0		<ATOM>
;;; 14.	EVAL MUNGEABLE	<-N,,0>		<RANDOM-SEXP>
;;; 15.	END OF BINARY	[IGNORED - IN PRACTICE () IS USED]



(DEFUN BUFFERBIN (TYP N X)
  (DECLARE (FIXNUM TYP))       
  (STORE (BTAR BINCT) TYP)
  (STORE (BXAR BINCT) N)
  (STORE (BSAR BINCT) X)
  (COND ((AND (NOT (= TYP 17)) (< BINCT 8.)) (SETQ BINCT (1+ BINCT)))
	(T (DO ((N 0 (BOOLE 7 (LSH N 4) (BTAR I)))  ;PACK 9 TYPE BYTES INTO
		(I 0 (1+ I)))			    ;ONE WORD
	       ((> I BINCT) (FASLOUT (LSH N (* 4 (- 8. BINCT))))))
	   (DO I 0 (1+ I) (> I BINCT)
	     (SETQ TYP (BTAR I) N (BXAR I))
	     (COND ((OR (< TYP 5) (= TYP 6) (= TYP 8.)) (FASLOUT N))
		   (T (SETQ X (BSAR I)) 
		      (COND ((= TYP 5)  
			     (SETQ SQUIDP ())
			     (LISTOUT X)
			     (FASLOUT (BOOLE 7 -1_18. (LSH N -18.)))
			     (FASLOUT (COND (SQUIDP 0) ((SXHASH X)))))
			    ((= TYP 10.)
			     (LET ((TYPE (TYPEP X)))
			       (COND ((EQ TYPE 'SYMBOL)
				      (SETQ X (PNGET X 7))
				      (FASLOUT (LENGTH X))
				      (MAPC 'FASLOUT X))
				     ((EQ TYPE 'BIGNUM)
				      (FASLOUT (BOOLE 7 3_33. 
						      (COND ((MINUSP X) 7_18.)
							    (0))
						      (LENGTH (CDR X))))
				      (MAPC 'FASLOUT (REVERSE (CDR X))))
				     ((MEMQ TYPE '(FIXNUM FLONUM))
				      (FASLOUT (COND ((EQ TYPE 'FIXNUM) 1_33.)
						     (2_33.)))
				      (FASLOUT (LSH X 0)))
				     (T (BARF (LIST TYP N type X)
					      | - BUFFERBIN screw type 10|)))))
			    ((= TYP 11.)
			     (FASLOUT (LOGIOR (LSH (ATOMINDEX (CAR X) 'SYMBOL) 
						   18.)
					      (ATOMINDEX (CDR X) 'SYMBOL)))
			     (FASLOUT N))
			    ((= TYP 14.) (LISTOUT X) (FASLOUT N))
			    ((= TYP 15.) (FASLOUT #.(car (pnget '|*FASL+| 6))))
			    ((= TYP 7) (FASLOUT N) (AND X (FASLOUT X)))
			    ((= TYP 13.) (FASLOUT (SQOZ/| (LIST X))))
			    (T (BARF (LIST TYP N X) | - BUFFERBIN screw|))))))
	   (SETQ BINCT 0))))



(DEFUN POPNCK@ MACRO (L)
       (SUBST (CADR L)
	      'tag 
	      '(COND ((NULL (SETQ L (CDR L))) (GO DONE))
		     ((EQ (CAR L) '/@) (SETQ WRD (BOOLE 7 WRD 20_18.)) (GO tag)))))

(DEFUN MKEVAL MACRO (L)
       (SUBST (CADR L) 
	      'n 
	      '(PROG2 (SETQ FSLFLD n)
		      (AND (EQ (SETQ SYM (FASLEVAL (CAR L))) 'FOO) (GO MKWERR)) 
		      (SETQ TYPE (TYPEP SYM)))))

(DEFUN MAKEWORD (L)
    (DECLARE (FIXNUM WRD NN II REL LN))
    (PROG (WRD NN SYM TYPE OPGL ACGL ADDRGL INDXGL NOGL REL SYL OL)
	  (SETQ NOGL T REL 0 WRD 0 OL L)
	  (COND ((EQ (CAR L) 'SQUOZE) 
		 (BINOUT (SQOZ/| (CDR L)))
		 (SETQ *LOC (1+ *LOC))
		 (RETURN ()))
		((EQ (CAR L) 'BLOCK)
		 (SETQ TYPE (TYPEP (SETQ SYM (CADR L))))
		 (AND (EQ TYPE 'SYMBOL) (SETQ TYPE (TYPEP (SETQ SYM (GET SYM 'SYM)))))
		 (AND (NOT (EQ TYPE 'FIXNUM)) (GO MKWERR))
		 (DO II SYM (1- II) (ZEROP II) (BINOUT 0))
		 (SETQ *LOC (+ *LOC SYM))
		 (RETURN ()))
		((COND ((EQ (CAR L) 'ASCII) (SETQ NN 7) T)
		       ((EQ (CAR L) 'SIXBIT) (SETQ NN '6) T))
		 (MAPC 'BINOUT (SETQ SYM (PNGET (CADR L) NN)))
		 #%(LET ((LN (LENGTH SYM)))
		       (COND ((NOT (ZEROP (SETQ NN (- (BLOBLENGTH L) LN))))
			      (BINOUT 0)
			      (AND (NOT (= 1 NN)) 
				   (BARF L |How Much ASCII? - MAKEWORD|))
			      (SETQ LN (+ NN LN))))
		       (SETQ *LOC (+ *LOC LN)))
		 (RETURN ())))
	  (MKEVAL 3)
	  (COND ((MEMQ TYPE '(FIXNUM FLONUM)) (SETQ WRD SYM))
		((NOT (EQ TYPE 'LIST)) (GO MKWERR))
		((EQ (CAR SYM) 'RELOC) 
		 (SETQ REL 1 WRD (CADR SYM))
		 (AND (SETQ OPGL (CDDR SYM)) (SETQ NOGL ())))
		((NUMBERP (CAR SYM)) (SETQ NOGL () OPGL (CDR SYM) WRD (CAR SYM)))
		(T (GO MKWERR)))
      A	  (POPNCK@ A)
	  (MKEVAL 2)
	  (COND ((EQ TYPE 'FIXNUM) (SETQ WRD (+ WRD (ROT (BOOLE 1 SYM 17) -13.))))
		((NOT (EQ TYPE 'LIST)) (GO MKWERR))
		((NUMBERP (CAR SYM)) 
		 (SETQ NOGL () ACGL (CDR SYM))
		 (SETQ WRD (BOOLE 7 WRD (ROT (BOOLE 1 (CAR SYM) 17) -13.))))
		(T (GO MKWERR)))
      B	  (POPNCK@ B)
	  (MKEVAL 1)
	  (COND ((EQ TYPE 'FIXNUM) (SETQ NN SYM))
		((NOT (EQ TYPE 'LIST)) (GO MKWERR))
		((NUMBERP (CAR SYM)) (SETQ NOGL () ADDRGL (CDR SYM) NN (CAR SYM)))
		((PROG2 (SETQ SYL (CADR SYM)) (MEMQ (CAR SYM) '(QUOTE FUNCTION)))
		 (SETQ REL (COND ((OR (EQ (SETQ TYPE (TYPEP SYL)) 'LIST) 
				      (HUNKP SYL))
				  (SETQ ADDRGL SYL NN 0)
				  5)
				 ('T (SETQ NN (ATOMINDEX SYL TYPE))
				     4))))
		((COND ((EQ (CAR SYM) 'SPECIAL) (SETQ REL 2) T)
		       ((EQ (CAR SYM) 'ARRAY) (SETQ REL 10) T))
		 (AND (NOT (SYMBOLP SYL)) (GO MKWERR))
		 (SETQ NN (ATOMINDEX SYL 'SYMBOL)))
		((EQ (CAR SYM) 'RELOC)
		 (SETQ REL 1 NN (CADR SYM))
		 (AND (SETQ ADDRGL (CDDR SYM)) (SETQ NOGL ())))
		((COND ((EQ (CAR SYM) 'EVAL) 
			(SETQ ADDRGL (CONS SQUID (CDR SYM)))
			T)
		       ((EQ (CAR SYM) SQUID) (SETQ ADDRGL SYM) T))
		 (SETQ REL 5))
		(T (GO MKWERR)))
	  (SETQ WRD (BOOLE 7 (BOOLE 1 WRD -1_18.) (BOOLE 1 (+ WRD NN) 777777)))
      C	  (POPNCK@ C)
	  (MKEVAL 0)
	  (COND ((MEMQ TYPE '(FIXNUM FLONUM)) (SETQ WRD (+ WRD (ROT SYM 18.))))
		((NOT (EQ TYPE 'LIST)) (GO MKWERR))
		((NUMBERP (CAR SYM)) 
		 (SETQ NOGL () INDXGL (CDR SYM) WRD (+ WRD (ROT (CAR SYM) 18.))))
		(T (GO MKWERR)))
    DONE (AND (= REL 4) (MEMQ (CAR OL) '(CALL JCALL NCALL NJCALL)) (SETQ REL 3))
	  (SETQ *LOC (1+ *LOC))
	  (BUFFERBIN REL WRD (AND (= REL 5) (PROG2 () ADDRGL (SETQ ADDRGL ()))))
	  (COND ((NOT NOGL)
		 (AND OPGL (GLHAK OPGL 3))
		 (AND ACGL (GLHAK ACGL 2))
		 (AND ADDRGL (GLHAK ADDRGL 1) (GO MKWERR))
		 (AND INDXGL (GLHAK INDXGL 0))))
	  (RETURN ())
      MKWERR (PDERR OL |- Ill-formed expression - MAKEWORD|)
      	     (ERR 'FASLAP)))


(DEFUN GLHAK (GLITCH FIELD)
    (DECLARE (FIXNUM FIELD))
    (COND ((NULL (CAAR GLITCH))
	   (COND ((NOT (= FIELD 1)))	;RETURNS "true" IF LOSES
		 (T  (BUFFERBIN 6 
				(BOOLE 7 (COND ((CDDAR GLITCH) -4_33.) (0)) 
					 (BOOLE 1 (CADAR GLITCH) 777777))
				())
		     (AND (CDR GLITCH) (GLHAK (CDR GLITCH) FIELD)))))
	  (T (BUFFERBIN 7 
			(BOOLE 7 (COND ((CDDAR GLITCH) -4_33.) (0))		;PLUS OR MINUS?
				 (COND ((CADAR GLITCH) 2_33.) (0))		;VALUE KNOWN AT ASSEMBLY TIME?
				 (ROT FIELD -4)				;FIELD NUMBER
				 (CAAR GLITCH))					;SQUOZE REPRESENTATION
			(CADAR GLITCH))						;GUESS AT SYMVAL			
	     (AND (CDR GLITCH) (GLHAK (CDR GLITCH) FIELD)))))

(DEFUN BINOUT (X) (BUFFERBIN 0 X ()))


(DEFUN *DDTSYM (SYM)  (FASLDEFSYM SYM (LIST '0 (LIST (SQOZ/| (LIST SYM)) (GETDDTSYM SYM)))))


(DEFUN FASLOUT (X)  (OUT IMOSAR X))

