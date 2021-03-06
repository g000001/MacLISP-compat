;;;  MACAID    				-*-Mode:Lisp;Package:SI;Lowercase:T-*-
;;;  *************************************************************************
;;;  ***** MacLISP ******* MACro definition AIDs *****************************
;;;  *************************************************************************
;;;  ** (c) Copyright 1981 Massachusetts Institute of Technology *************
;;;  *************************************************************************

(herald MACAID /119)

#-NIL
(eval-when (eval compile)
  (or (get 'SUBLOAD 'VERSION)
      (load '((lisp) subload)))
  (subload SHARPCONDITIONALS)
  (subload LOOP)
)

;; For bootstrapping into LISPM, this could do
;; (defmacro HERALD (group-name &optional (version-number '?))
;;	     `(DEFPROP ,group-name ,version-number VERSION))


#+(or LISPM (and NIL (not MacLISP)))
  (progn 'compile 
	 (globalize "FLATTEN-SYMS") 	; Functions supplied
	 (globalize "BUT-TAIL")
	 (globalize "SYMBOLCONC")
	 (globalize "no-funp/|")
	 (globalize "side-effectsp/|")
	 (globalize "constant-p/|")
	 (globalize "+INTERNAL-DUP-P")
	 (globalize "+INTERNAL-PERMUTIBLE-P")
	 (globalize "defmacro-2/|")	;standardize macro-defining format
	 (globalize "DEFSIMPLEMAC") 	; Macros
	 (globalize "DEFCOMPLRMACRO") 
	 (globalize "DEFBOTHMACRO") 
	 (globalize "GENTEMP")
	 )


#-MacLISP (eval-when (eval compile load) (PACKAGE-DECLARE * SYSTEM 100) )

#-NIL  
(subload DEFMAX) 	;Get important functions and globalizations

#M 
(eval-when (eval load compile)
   (cond ((status feature COMPLR)
	  (*lexpr SYMBOLCONC GENTEMP)
	  (special GENTEMP)))
)

#+(local MacLISP)
  (declare (own-symbol FLATTEN-SYMS |carcdrp/|| |no-funp/|| |side-effectsp/|| 
		       +INTERNAL-DUP-P  DEFSIMPLEMAC  DEFCOMPLRMAC SYMBOLCONC 
		       DEFBOTHMACRO  |no-funp/||  |constant-p/||))

#-NIL 
(eval-when (eval compile)
   #-LISPM (*expr |carcdrp/||)
   (special |carcdrp/||)
   (defmacro TYPECASEQ (&rest w)
       `(CASEQ (TYPEP ,(car w)) 
	       ,.(mapcar '(lambda (x) 
			     (cons (sublis '((PAIR . LIST)) (car x)) 
				   (cdr x)))
			 (cdr w))))
   #+LISPM (defmacro PAIRP (x) `(NOT (ATOM ,x)))
)


;;;; GENTEMP and  SI:GEN-LOCAL-VAR


;;GENTEMP holds a list of three goodies
;; 1st: a string, or list of "chars", for the root "string" of the var names
;; 2nd: a number to be incremented with each usage, for a numerical suffix
;; 3rd: the marker used as a plist flag to signal "super-uninterned"

(or (and (boundp 'GENTEMP) GENTEMP)
    (setq GENTEMP (list ".." 0. '+INTERNAL-TEMP-MARKER)))

(eval-when (eval compile)
   (defmacro GENTEMP-prefix () `(CAR GENTEMP))
   (defmacro GENTEMP-time () `(CADR GENTEMP))
   (defmacro GENTEMP-marker () `(CADDR GENTEMP))
)


(defun GENTEMP (&optional (prefix () prefix-p) &aux (s0 (si:time-stamp)))
    "Generate a temporary symbol, which is guaranteed to have no
     'properties', even after compiling and fasloading."
   (setq s0 
	  #-NIL 
	   (maknam (nconc (if prefix-p (exploden prefix) (list 'T)) 
			  (exploden (GENTEMP-prefix))
			  s0))
	  #+NIL 	
	   (si:symbol-cons (string-append (if prefix-p (to-string prefix) "T") 
					  (GENTEMP-prefix)
					  s0))
	   )
   (putprop s0 'T (GENTEMP-marker))
   s0 ) 


(defmacro SI:GEN-LOCAL-VAR (&optional var (gentempper () gp))
    "Basically, a GENTEMP with a :LOCAL-VAR property, so that the
     COMPLR can check to see that it is never auto-SPECIALized."
   (setq gentempper (cond (gentempper `(GENTEMP ,gentempper))
			  ('(GENTEMP))))
   (cond ((and gp (null var)) gentempper)
	 (var `(PROG2 (PUTPROP (SETQ ,var ,gentempper) 'T ':LOCAL-VAR)
		      ,var)) 
	 ((let ((g (gentemp)))
	    (putprop g 'T ':LOCAL-VAR)
	    `((LAMBDA (,g) 
		(PUTPROP ,g 'T ':LOCAL-VAR)
		,g) 
	      ,gentempper)))))


(defun SI:TIME-STAMP () 
    "For now, this is just a sequence of numbers stored in a slot of GENTEMP.
     But we need a real 'time-stamp', such as the number of milliseconds since
     Jan 1, 1970."
   (setf (GENTEMP-time) (1+ (GENTEMP-time)))
  #-NIL 
   (let ((BASE 10.) (*NOPOINT 'T)) (exploden (GENTEMP-time)))
  #+NIL 
   (fill-digits-into-string (make-string 12.) 
			    (GENTEMP-time) 
			    0 
			    12. 
			    36.))



;;;; DEFSIMPLEMAC


;;; Many functions of one argument can be macro-expanded, providing
;;; 	that the argument-form can be multiplied.  If not, then we must
;;;	wrap a LAMBDA around it, and give it an argument-form of a symbol.


(defmacro DEFSIMPLEMAC (oname vars /&rest body &aux var name)
    (and (or (atom vars) (not (symbolp (car vars))) (cdr vars))
	 (error "Bad arglist for DEFSIMPLEMAC" `(,oname ,vars ,@body)))
    (setq var (car vars))
    (if (not (pairp oname))
	(setq oname `(,oname DEFMACRO-DISPLACE-CALL 'T)))
    (setq name (car oname)
	  body `(DEFMACRO ,oname ,vars
		  (COND ((and (|no-funp/|| (SETQ ,VAR (MACROEXPAND ,VAR)))
			      (+INTERNAL-DUP-P  ,VAR))
			 ,(cond ((cdr body) (cons 'PROGN body))
				((car body))))
			('T (|non-simple-x/|| ',name ,VAR)))))
    #-NIL (if (if (get 'SHARPCONDITIONALS 'VERSION) 
		  (nofeaturep 'NIL)
		  (status feature NIL))
	      (setq body `(PROGN 'COMPILE 
				 (DEF-OR-AUTOLOADABLE |non-simple-x/|| MACAID)
				 ,body)))
    body)

;; Presumes that argument is already macroexpanded
(defun |non-simple-x/|| (name callarg)
   (cond ((eq (car callarg) 'PROG2)
	  (let (( (() e1 e2 . rst) callarg))
	    `(PROG2 ,e1 (,name ,e2) ,. rst )))
	 ((eq (car callarg) 'PROGN)
	  (setq callarg (reverse (cdr callarg)))
	  `(PROGN ,.(nreverse (cdr callarg))
		  (,name ,(car callarg))))
	 ((memq (car callarg) '(SETQ PSETQ))
	  `(PROG2 ,callarg (,name ,(cond ((eq (car callarg) 'PSETQ)
					  (cadr callarg))
					 ((do ((l (cdr callarg) (cddr l)))
					      ((null (cddr l)) (car l))))))))
	 ((let (g decls)
	    (si:gen-local-var g)
	    (if (and (not (atom callarg)) 
		     (memq (car callarg) '(FIXNUM-IDENTITY FLONUM-IDENTITY)))
		(setq decls `((DECLARE 
			         (,(if (eq (car callarg) 'FIXNUM-IDENTITY)
				       'FIXNUM 
				       'FLONUM)
				  ,g)))
		      callarg (cadr callarg)))
	    `((LAMBDA (,g) ,.decls (,name ,g)) ,callarg)))))



;;;; DEFBOTHMACRO and DEFCOMPLRMAC


(defmacro DEFBOTHMACRO (fun &rest w &aux args body simplep)
    (setq args (car w) body (cdr w))	;Remember! LISPM is stupid
    (if (eq args 'SIMPLE)
	(setq args (car body) body (cdr body) simplep 'T))
    (setq body (progv args args (mapcar #'EVAL body)))	;like a macro expansion
    `(PROGN 'COMPILE
	    (DEFCOMPLRMAC ,fun ,.w)
	    (DEFUN ,fun ,args ,.body)))
		     
(defmacro DEFCOMPLRMAC (&rest w)
   (let (((fun args . body) w)
	 (defmac 'DEFMACRO))
     (or (symbolp fun) (error "Name not a symbol -- DEFCOMPLRMAC" fun))
     (cond ((eq args 'SIMPLE)
	    (pop body args)
	    (setq defmac 'DEFSIMPLEMAC)))
     (if (fboundp 'MACRO-EXPAND) 
	 (setq body (cdr (macro-expand `(PROGN ,.body)))))
      #-NIL 
       ;;In the MacLISP case, we don't use the SOURCE-TRANS feature, since
       ;; we dont want to clutter up the address space of a non-COMPLR 
       ;; environment with all those crufty expansion subrs
     (let ((definer `(,DEFMAC ,fun ,args ,.body)))
	`(PROGN 'COMPILE
	     (EVAL-WHEN (LOAD)
			(COND ((STATUS FEATURE COMPLR) 
			       (DEFPROP ,fun T DEFCOMPLRMAC)
			       (EVAL ',definer))))
	     (EVAL-WHEN (EVAL COMPILE)
			,definer)))
      #+NIL 
     (let ((expander-fun (si:gen-local-var))
	   DEFMACRO-CHECK-ARGS DEFMACRO-DISPLACE-CALL )
	 ;; process the &optional, &rest, and &aux of args for a LET list
	(desetq (() () args . body) (|defmacro-2/|| w))
    	 ;; 'args' should now be a list of one symbol.
	`(PROGN 'COMPILE
	     (DEFUN ,expander-fun ,args 
		(VALUES (PROGN ,.body) 'T))
	     (PUSH ',expander-fun (GET ',fun 'SOURCE-TRANS))))
     ))

#-MacLISP  
(defun |defmacro-2/|| (x &aux (y x) name) 
    "Will standardize a macro definition into the primitive (MACRO ...) form."
   (if (cond ((not (pairp x))) 
	     ((memq (car x) '(DEFUN MACRO)) () )
	     ((memq (car x) '(DEFMACRO DEFMACRO-DISPLACE))
	      (setq y (|defmacro-1/|| x ddc))
	      (if (eq (car y) 'PROGN) 
		  (or (setq y (assq 'MACRO (cdr x)))
		      (setq y (assq 'DEFUN (cdr x)))))
	      (if (and y (eq (car y) 'DEFUN))
		  (setq y (cond ((eq (setq name (caddr y)) 'MACRO) 
				   ;; (DEFUN <name> MACRO (ARG) ...)
				  `(MACRO ,(cadr y) ,.(cdddr y)))
				((memq name '(EXPR FEXPR)) () )
				(y))))
	      (null y)))
       (+internal-lossage 'DEFUN '|defmacro-2/|| x))
   (cond ((and (eq (car y) 'MACRO) (symbolp (cadr y)))
	   y)
	 ('T (setq name (cadr y)) 
	     (if (pairp name) (setq name (car name)))
	     `(MACRO ,name ,(cddr y)))))



;;;; |carcdrp/||

#-NIL (progn 'COMPILE 

;  +INTERNAL-CARCDRP returns a -1 if arg is not a carcdr symbol, else returns
;    a 13.-bit number encoding the three things of the old carcdr property.

(defun |carcdrp/|| (x) 
  (cond ((get x 'CARCDR))
	(|carcdrp/||				;|carcdrp/|| is non-null iff  
	 (let ((n (+INTERNAL-CARCDRP x)))	; +INTERNAL-CARCDRP exists
	   (declare (fixnum n nn))
	   (cond ((< n 0) () )
		 ((putprop x 		;"cache" result on plist
		     (list* (cond ((< n 1_12.) 'A) ('D))
			    (implode 
			     `(C ,.(nconc 
				    (do ((z ()
					    (cons (cond ((zerop (boole 1 nn 1))
							 'A) 
							('D))
						  z))
					 (nn (boole 1 (lsh n -6) 63.)
					     (lsh nn -1)))
					((< nn 2) z))
				    '(R)))) 
			    (boole 1 n 63.))
		     'CARCDR)))))))


(and (not (boundp '|carcdrp/||))
     (not (setq |carcdrp/|| (fboundp '+INTERNAL-CARCDRP)))
     (mapc '(lambda (x) (putprop (car x) (cdr x) 'CARCDR))
	   '((CAR (A NIL . 6.))
	     (CAAR (A CAR . 5.))
	     (CAAAR (A CAAR . 19.))
	     (CAAAAR (A CAAAR . 27.))
	     (CAAADR (A CAADR . 26.))
	     (CAADR (A CADR . 18.))
	     (CAADAR (A CADAR . 17.))
	     (CAADDR (A CADDR . 16.))
	     (CADR (A CDR . 4.))
	     (CADAR (A CDAR . 3.))
	     (CADAAR (A CDAAR . 36.))
	     (CADADR (A CDADR . 35.))
	     (CADDR (A CDDR . 2.))
	     (CADDAR (A CDDAR . 1.))
	     (CADDDR (A CDDDR . 0.))
	     (CDR (D NIL . 14.)) 
	     (CDAR (D CAR . 13.)) 
	     (CDAAR (D CAAR . 24.)) 
	     (CDAAAR (D CAAAR . 33.)) 
	     (CDAADR (D CAADR . 32.)) 
	     (CDADR (D CADR . 23.)) 
	     (CDADAR (D CADAR . 22.)) 
	     (CDADDR (D CADDR . 21.)) 
	     (CDDR (D CDR . 12.)) 
	     (CDDAR (D CDAR . 11.)) 
	     (CDDAAR (D CDAAR . 30.)) 
	     (CDDADR (D CDADR . 29.)) 
	     (CDDDR (D CDDR . 10.)) 
	     (CDDDAR (D CDDAR . 9.)) 
	     (CDDDDR (D CDDDR . 8.)) )
	     ))
)

#+NIL 
(defun |carcdrp/|| (x) 
   (let* ((pn (get-pname x))
	  (len (string-length pn)))
      (and (> len 2)
	   (eq (char pn 0) ~C)
	   (eq (char pn (1- len)) ~R)
	   (LOOP FOR i FROM (- len 2) DOWNTO 1
		 UNLESS (memq (char pn i) '(~A ~D)) RETURN () 
		 FINALLY (return 1000.)))))


;;;; |constant-p/||, |no-funp/||, and +INTERNAL-DUP-P,

;; Presumes that argument is already macroexpanded
(defun |constant-p/|| (x) 
   (or (null x)
       (typecaseq x 
	      (SYMBOL () )
	      (PAIR (memq (car x) '(QUOTE FUNCTION)))
	      (T 'T))))


;; Presumes that argument is already macroexpanded
(defun |no-funp/|| (x)
  (cond ((or (atom x) (memq (car x) '(QUOTE FUNCTION DECLARE))))
	((not (symbolp (car x)))  () )
	((|carcdrp/|| (car x)) (|no-funp/|| (cadr x)))
	((memq (car x) '(NTH FIXNUM-IDENTITY FLONUM-IDENTITY 
			     +INTERNAL-CHAR-N CHAR-N CHAR VREF BIT 
			     SI:XREF CXR ELT AR-1 AREF))
	 (and (|no-funp/|| (cadr x)) (|no-funp/|| (caddr x))))
	((memq (car x) '(+ - * // \ 1+ 1- +$ -$ *$ //$ 1+$ 1-$))
	  (LOOP FOR y IN (cdr x) 
		ALWAYS (|constant-p/|| y)))))


;; Presumes that argument is already macroexpanded
(defun +INTERNAL-DUP-P (x)
    "Non-null if it is 'cheaper' to duplicate the permissibly-duplicatable 
     code rather than do a lambda-binding."
  (cond ((or (atom x) (memq (car x) '(QUOTE FUNCTION DECLARE)))
	  ;; These, of course, do nothing
	 'T)
	((not (symbolp (car x)))  () )
   	((|carcdrp/|| (car x))
	  ;; any carcdr of length 2 or less -- '(CAR CDR CAAR CADR CDAR CDDR)
	 (and (< (flatc (car x)) #-NIL 4 #+NIL 3)
	      (or (atom (cadr x))
		  (|constant-p/|| (cadr x)))))
	 #M 
	((eq 'CXR (car x))  
	 (and (|constant-p/|| (cadr x))
	      (or (atom (caddr x)) (|constant-p/|| (caddr x)))))
	((memq (car x) '(+ - * // \ 1+ 1- +$ -$ *$ //$ 1+$ 1-$))
	  (LOOP FOR y IN (cdr x) 
		ALWAYS (|constant-p/|| y)))
	((or (memq (car x) '(FIXNUM-IDENTITY FLONUM-IDENTITY))
	     (and (null (cddr x)) (memq (car x) '(PROG2 PROGN))))
	 (+internal-dup-p (cadr x)) )))

(defun +INTERNAL-PERMUTIBLE-P (forms)
    "Non-null if it is permissible to change the ordering of the
     evaluations on the list 'forms'."
   (do ((l forms (cdr l)) 			;Either all constans
	(non-constantsp)			; or no side-effects
	(haumany-sides 0)
	x)
       ((null l) 'T)	
     (declare (fixnum haumany-sides))
     (setq x (macroexpand (car l)))
     (cond ((|constant-p/|| x) () ) 
	   ((|side-effectsp/|| x)
	     (if (or non-constantsp (> haumany-sides 0))
		 (return () ))
	     (setq non-constantsp 'T 
		   haumany-sides (1+ haumany-sides))
	     (if (> haumany-sides 1) (return () )))
	   ('T (setq non-constantsp 'T)))))



;;;;  |side-effectsp/||

(defun |side-effectsp/|| (x)
  (cond ((atom x) () )
	((memq (car x) '(QUOTE FUNCTION DECLARE)) () )
	((and (pairp (car x)) (eq (caar x) 'LAMBDA))
	 (or (|mmcdrside/|| (cdar x)) (|mmcdrside/|| x)))
	((or (not (symbolp (car x))) (not (fboundp (car x))))   
	 'T)
    #+LISPM 
	((let (ocarx ocdrx nx)
	      (setq ocarx (car x) ocdrx (cdr x))
	      (setq nx (macroexpand-1 x))
	      (cond ((or (not (eq nx x))
			 (not (eq ocarx (car x)))
			 (not (eq ocdrx (cdr x))))
		     (setq x nx)
		     'T)))
	 (|side-effectsp/|| x))
    #-LISPM 
	((let ((nx (macroexpand-1* x))) (and nx (setq x nx)))
	 (|side-effectsp/|| (car x)))
	((get (car x) '|side-effectsp/||) (|mmcdrside/|| x))
	((|carcdrp/|| (car x)) (|side-effectsp/|| (cadr x))) 
        ((eq (car x) 'COND) 
	  (LOOP FOR clause IN (cdr x)
		THEREIS (|mmcdrside/|| (cons () clause))))
	((memq (car x) '(CASEQ SELECTQ))
	 (or (|side-effectsp/|| (cadr x)) 
	     (LOOP FOR z IN (cddr x)
		   THEREIS (|mmcdrside/|| z))))
        ((eq (car x) 'PROG) (|mmcdrside/|| (cdr x)))
	('T 'T)))

(defun |mmcdrside/|| (y) 
  (LOOP FOR x IN (cdr y)
	THEREIS (|side-effectsp/|| x)))


;; This property does not mean that the function has side effects!  It
;;  means that the function itself has none, but that it's arguments should
;;  be inspected by means of the function |mmcdrside/||
(mapc '(lambda (x) (putprop x '|mmcdrside/|| '|side-effectsp/||))
      (append 
       #M '(ARRAYCALL ARRAY LISTARRAY HUNK MAKHUNK CXR  
	    SIGNP *LDB *LOAD-BYTE ROT FSC |&restv-ify/|| )
       #N '( <$  >$ <=$ >=$ =$ MAX& MIN& MAX$ MIN$  ELT)
        '(SI:MAKE-EXTEND SI:EXTEND-LENGTH SI:EXTENDP EXTENDP EXTEND-LENGTH 
	  SI:XREF SI:EXTEND  PTR-TYPEP  +INTERNAL-CHAR-N)
	'(STRINGP VECTORP BITSP CHARACTERP 
	  SUBSEQ  TO-LIST  TO-VECTOR  TO-STRING  TO-BITS 
	  VECTOR-LENGTH  STRING-LENGTH  BITS-LENGTH 
	  BIT VREF CHAR CHAR-N +INTERNAL-CHAR-N 
	  VECTOR MAKE-VECTOR MAKE-STRING MAKE-BITS
	  *:FIXNUM-TO-CHARACTER  STRING-PNPUT 
	  |defvst-construction/||  |defvst-construction-1/||
	  |defvst-selection-1/||  |defvst-xref/|| 
	   ;; above are for NILCOM stuff
	  AND OR MAKNAM MAKE-LIST PAIRP FBOUNDP PLIST
	  CONS NCONS XCONS ASSQ ASSOC COPYSYMBOL GET GETL 
	  GETCHAR GETCHARN IMPLODE LAST LIST LISTIFY PNGET 
	  EXPLODE EXPLODEC EXPLODEN FLATC FLATSIZE BUT-TAIL 
	  MEMQ MEMBER SUBLIS SUBST REVERSE APPEND SYMBOLCONC 
	  BIGP EQUAL EQ FIXP FLOATP NUMBERP SYMBOLP TYPEP
	  NOT NULL ODDP GREATERP LESSP PLUSP MINUSP ZEROP 
	  FILEP FASLP PROBEF NAMELIST NAMESTRING TRUENAME  
	  PLUS DIFFERENCE TIMES QUOTIENT ADD1 SUB1 ABS 
	  + - * // 1+ 1- ^ +$ -$ *$ //$ 1+$ 1-$ ^$  \  \\ 
	  REMAINDER GCD EXP EXPT BOOLE > <  =  >= <= 
	  IFIX FIX LOG SQRT SIN COS LSH ASH LDB LOAD-BYTE 
	  HAIPART HAULONG HUNKSIZE LENGTH SXHASH 
	  FIXNUM-IDENTITY FLONUM-IDENTITY)
	))

;;;; SYMBOLCONC, BUT-TAIL, FLATTEN-SYMS 

(eval-when (eval compile)
(defmacro iterate-over-args (varsl &rest body &aux item seq index tail)
   (pop varsl item) 		;Damnd LISPM! really want
   (pop varsl seq) 		; (desetq (item seq index tail) varsl)
   (pop varsl index)
   (pop varsl tail)
 #-NIL  `(LOOP FOR ,tail ON ,seq 
	       AS  ,item = (car ,tail)
	       FOR ,index FROM 0 
	       DO ,.body)
 #+NIL  `(LOOP FOR ,item BEING THE VECTOR-ELEMENTS OF ,seq USING (INDEX ,index)
	       DO ,.body) ))

(defun SYMBOLCONC #M w #-MacLISP (&rest w &aux (nchars 0))  
    "Concatenate together the PNAMEs of some SYMBOLs, and INTERN that
     string to get a SYMBOL."
    #M (setq w (listify w)) 
   (iterate-over-args (s w i l)
	(do ()
	    ((cond 
	       ((null s) 
		 (setq s #-MacLISP (make-string 0)     ;FOo! () means different
			 #+MacLISP (list #/N #/I #/L)  ; things at times!
			 )
		 'T)
	       ((typecaseq s 
		   (SYMBOL (setq s #M (exploden s) #-MacLISP (get-pname s)) 'T)
		   (FIXNUM #-LISPM 
			   (let ((BASE 10.) 
				 (*NOPOINT 'T))  
			     (setq s (exploden s))
			      #-MacLISP 
			     (setq s (to-string s)))
			   #+LISPM (setq s (string s)))
		   (PAIR (setq s #N (to-string s) 
			 	 #M (append s () )
				 #Q (apply #'STRING-APPEND s)
			     )
		      'T)
	 #-MacLISP (STRING 'T)
	 #N 	   (VECTOR (setq s (to-list s)))
		   (T #M (cond ((not (hunkp s)) () )
			       ((and (fboundp 'STRINGP) (stringp s))
				 (setq s (exploden s))
				 'T)
			       ((and (fboundp 'VECTORP) (vectorp s))
				 (setq s (to-list s))
				 'T))
		      #-MacLISP () )))))
	  (setq s (cerror 'T () ':WRONG-TYPE-ARGUMENT 
			  "~1G~S Bizarre arg -- SYMBOLCONC" 
			  'SYMBOL s)))
      #N (prog2 (setq nchars (+ nchars (string-length s)))
		(vset w i s))
      #-NIL (rplaca l s)
       )
#Q (intern (apply #'STRING-APPEND w))
#M (implode (apply 'NCONC w))
#N (let ((result (make-string nchars))		    ;Since this file is early
	 (newi 0))				    ; in the bootstrapping of
      (iterate-over-args (s w i () )		    ; the system, make sure it
	  (string-replace result s newi)	    ; uses only simple things.
	  (setq newi (+ newi (string-length s))))
      (intern result))
)



(defun BUT-TAIL (list tail)
    "Copy top level of list l down to the tail of l that is EQ to tail."
 #+Lispm (ldiff list tail) 
 #-Lispm 
   (do ((l list (cdr l))
	(copy () (cons (car l) copy)))
       ((or (atom l) (eq l tail)) (nreverse copy)))
  )


(defun FLATTEN-SYMS (x l)
    "Descend a pair tree, collecting a list of all SYMBOLs seen."
   (cond ((pairp x) (FLATTEN-SYMS (car x) (FLATTEN-SYMS (cdr x) l)))
	 ((null x) l)
	 ((symbolp x) (cons x l))
	 ('T l)))

