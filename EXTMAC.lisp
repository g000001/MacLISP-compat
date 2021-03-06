;;;   EXTMAC			-*-Mode:Lisp;Package:SI;Lowercase:T-*-
;;;   ****************************************************************
;;;   *** MacLISP **** EXTended datatype scheme, MACros **************
;;;   ****************************************************************
;;;   ** (c) Copyright 1981 Massachusetts Institute of Technology ****
;;;   ****************************************************************

(herald EXTMAC /187)

(include ((lisp) subload lsp))

(eval-when (eval load compile)
    (subload MACAID)		;Also down-loads DEFMAX
    (subload ERRCK)
)


(eval-when (compile)
  (setq DEFMACRO-FOR-COMPILING 'T DEFMACRO-DISPLACE-CALL MACROEXPANDED)
  (own-symbol DEFCLASS* DEFMETHOD*)
 )


(defvar SI:EXTSTR-SETUP-CLASSES 
  '(OBJECT CLASS VECTOR STRUCT STRUCT=INFO SEQUENCE)
  "This list corresponds to what is set up in EXTSTR")

(defvar SI:EXTEND-Q-OVERHEAD 2 
  "Number of slots taken out of a hunk for EXTEND overhead.")

(defmacro SI:EXTEND-CLASS-OF (x) `(CXR 0 ,x))
(defmacro SI:EXTEND-MARKER-OF (x) `(CXR 1 ,x))




;; Be careful about circular dependencies!  Luckily this one is minor,
;;  and can be patched, if necessary.  (EXTEND has some SETFs in it.)
;; DEFSETF -> DEFVST -> EXTEND -> EXTMAC -> DEFSETF

(eval-when (eval compile)
  (subload DEFSETF)
)

(defsetf SI:XREF ((() h n) val) () `(SI:XSET ,h ,n ,val))


;;;; Initial CLASS object structure, and DEFCLASS* 

;; Leave around for benefit of NILSIM;PACKAGE
(defmacro SI:DEF-INITIAL-EXTEND-STRUCT 
	  (package prefix &rest rest
			  &aux (count 0)
			       (sizym (symbolconc package
						  '/: PREFIX
						  '-INSTANCE-SIZE))
			       access-sym)
  `(PROGN 'COMPILE
     ,.(mapcan 
	#'(lambda (frob)
	    (if (not (atom frob)) (setq frob (car frob)))
	    (setq access-sym (symbolconc package '/: prefix '- frob))
	     ;;; Use one function for macro-expanding all accessor macros
	    `( (DEFPROP ,access-sym
			,(prog1 count (setq count (1+ count)))
			SI:CLASS-SLOT-ACCESSOR)
	       (DEFPROP ,access-sym SI:CLASS-SLOT-ACCESSOR MACRO)))
	rest)
     (DECLARE (SPECIAL ,sizym)) ;|Number of Q's in instances of this class|
     (EVAL-WHEN (EVAL LOAD COMPILE) (SETQ ,sizym ,count))))
  

(SI:DEF-INITIAL-EXTEND-STRUCT SI CLASS
    SENDI	   ;; LSUBR-like function to jump to for SENDs to instances.
    SENDI-SYM	   ;; SYMBOL or LAMBDA the SENDI LSUBR came from
    CALLI	   ;; Similarly, for FUNCALLs.
    CALLI-SYM
    MAP-METHODS-I  ;; Interpreter for MAP-OVER-METHODS
    MAP-METHODS-SYM
    MAP-CLASSES-I  ;; Interpreter for MAP-OVER-CLASSES
    MAP-CLASSES-SYM
    ADD-METHOD-FUN ;; SUBRCALLed to add a method to a class
    TYPEP	   ;; Symbol returned by TYPEP.
    SUPERIORS	   ;; NCONS of superior class.
    NAME	   ;; Name of this class
    METHODS	   ;; An a-list of (KEY . <function>).
    PLIST	   ;; PLIST of random information
)

(defun SI:CLASS-SLOT-ACCESSOR ((fun val))
  (let ((slot (get fun 'SI:CLASS-SLOT-ACCESSOR)))
    (if (null slot) (+internal-lossage 'NULL 'SI:CLASS-SLOT-ACCESSOR fun))
    (if (memq compiler-state '(COMPILE MAKLAP))
	`(SI:XREF ,val ,slot) 
	`(SI:XREF
	   (LET ((VAL ,val))
	     ;;When EXTMAC is loaded, so will be ERRCK and SENDI
	     ;;If  this macro writes out expr code to a file, rather
	     ;; than having it compiled, then the loser will just have
	     ;; to run such expr code in a lisp with ERRCK and SENDI
	     (IF *RSET (CHECK-TYPE VAL #'CLASSP ',fun))
	     VAL)
	   ,slot))))

(defmacro SI:CLASS-ATTRIBUTES (class)
  `(si:class-plist ,class))
 
(defmacro SI:CLASS-VAR (class)
  `(get (si:class-plist ,class) ':VARIABLE))

(defmacro SI:CLASS-DOCUMENTATION (class)
  `(get (si:class-plist ,class) ':DOCUMENTATION))

;;Someday this should just turn into SI:CLASS-NAME -- when all those old
;;  classes composed out of HUNK16's go away.   July 4, 1981  - JonL -
(defmacro SI:CLASS-NAME-CAREFUL (class)
  `(let ((class ,class))
    (if (eq (typep class) 'HUNK32)
	(SI:XREF CLASS 16.)
	(si:class-name class))))

;; (DEFCLASS* name variable superior . options)
;;   creates a new CLASS object, assigning it to the variable
;;   VARIABLE.

(defmacro DEFCLASS* (name var supr &rest options &aux (typep name))	
  (and supr (symbolp supr) (setq supr (list supr)))
  (do ((l options (cddr L)))
      ((null l))
    (caseq (car l)
	   (TYPEP (setq typep (cadr l)))
	   (T (error "unknown option - DEFCLASS*"
		     (list (car l) (cadr l))))))
  `(PROGN 'COMPILE
	  ,@(if var `((DEFVAR ,var)))
	  (SI:DEFCLASS*-1 ',typep 
			  ',var 
			  (LIST ,@supr) 
			  ',name
			  ,@(if (filep infile)
				(list `',(namestring (truename infile)))))))

;;;; DEFMETHOD*, and MAKE-A-METHOD

;; (DEFMETHOD* (KEY FOO-CLASS) (FROB . ARGS) . BODY)
;; defines a KEY method for instances of class FOO.
;; When someone does a (SEND BAR 'KEY ARG1 ARG2), FROB is bound to
;; BAR, the ARGS are bound to ARG1 and ARG2, and the BODY is run.
;; KEY can be a list of keys instead of a single key

(defmacro DEFMETHOD* ((msg-key class-var) (obj . arglist) &rest body)
  (let* ((keys (if (atom msg-key) (ncons msg-key)
		  msg-key))
	 (method-fun (symbolconc (car keys) '-> class-var)))
    `(PROGN 'COMPILE
	    (DECLARE (**LEXPR ,method-fun))
	    (DEFUN ,method-fun (,obj () ,.arglist) ,.body)
	    ,.(mapcar #'(lambda (key)
			 `(ADD-METHOD ',key ',method-fun ,class-var))
		    keys))))


(defmacro MAKE-A-METHOD (&rest keywords &aux
			       (keyplist (cons 'keyplist keywords)))
  (let ((key (get keyplist 'key))
	(fun (get keyplist 'fun))
	(next (get keyplist 'next)))
    `(hunk ,key (and (symbolp ,fun)
		     (get ,fun 'lsubr))
	    ,fun ,next)))



;;;; TYPECASEQ

;; Temporary definition for ERROR-OUTPUT, unless CERROR is loaded
(defvar ERROR-OUTPUT 'T)

(defvar *:TRUTH 'T)
(defvar *:VAX-PRIMITIVE-TYPES
	'(PAIR VECTOR EXTEND FIXNUM FLONUM CONSTANT STRING BITS
	       CHARACTER SYMBOL VECTOR-S SUBR MSUBR FLONUM-S SMALL-FLONUM))

;; This definition of TYPECASEQ warns of LIST instead of PAIR, and
;; also of use of the extended TYPECASEQ syntax.  It also warns of
;; the use of T to denote an OTHERWISE clause, iff running in NIL.

(defmacro TYPECASEQ (typ &rest clauses)
  (setq clauses
   (mapcar			   ;Clobber LIST to PAIR, an warn of EXTENDs
    #'(lambda (clause)
	(setq clause (append clause ()))
        (if   (and (status feature NIL)
		   (not (eq *:TRUTH 'T))
		   (eq (car clause) *:TRUTH))
	      (rplaca clause 'T))  ;Fix loser's code. ######## Dangerous!!! 
	(if (eq (car clause) 'T)
	    clause
	    (let ((types (if (atom (car clause))
			     (ncons (car clause))
			     (append (car clause) ()))))
	      (map #'(lambda (types)			;Side effect if LIST
		       (cond 
			 ((eq (car types) 'LIST)
			   (format 
			     error-output
			     "~&;Warning: LIST keyword in TYPECASEQ clause -- ~
Converting to PAIR~%")
			   (rplaca types 'PAIR)))
		       (cond 
			 ((not (memq (car types) *:VAX-primitive-types))
			   (format 
			     error-output
			     "~&;Warning: ~S non-primitive type in TYPECASEQ~%"
			     (car types)))))
		   types)
	      (rplaca clause types))))
    clauses))
  `(CASEQ (PTR-TYPEP ,typ)
	  ,.clauses)) 


;; So a "method" is just a 4-hunk
(defmacro METHOD-NEXT (x) `(CXR 0 ,x))
(defmacro METHOD-SYMBOL (x) `(CXR 1 ,x))
(defmacro METHOD-FUN (x) `(CXR 2 ,x))
(defmacro METHOD-FUN-SYM (x) `(CXR 3 ,x))



;;;; DEFSFA 

(defmacro DEFSFA (name (sfa operation) vars options &rest ops)
  (let ((constructor-name (symbolconc 'cons-a- name))
	(handler-name (symbolconc name '-sfa-handler))
	(wops (nconc (delq ':SEND (mapcar #'CAR ops)) '(:SEND)))
	(data (si:gen-local-var () "SFA-DATA"))
	(idx -1)
	(initter (memq ':INIT options))
	accessor )
    (declare (fixnum idx))
    `(PROGN 'COMPILE
       (EVAL-WHEN (EVAL LOAD COMPILE)
	 (DECLARE (SPECIAL MACLISP-PRIMITIVE-CLASS))
	 (def-or-autoloadable SEND-AS EXTEND)
	 (def-or-autoloadable SFA-UNCLAIMED-MESSAGE EXTSFA)
	 (def-or-autoloadable SI:DEFSFA-ACCESSOR EXTSFA)
	 (def-or-autoloadable SI:DEFSFA-CREATOR EXTSFA)
	 (def-or-autoloadable SI:INIT-SFA EXTSFA)
	 (DEFPROP ,constructor-name SI:DEFSFA-CREATOR MACRO)
	 (DEFPROP ,constructor-name ,name DEFSFA-NAME)
	 ,(if initter 
	      `(PUTPROP ',name 
			,(cadr initter)
			'DEFSFA-INITP)
	      `(DEFPROP ,name T DEFSFA-INITP)) 
	 (DEFPROP ,name ,(length vars) DEFSFA-SIZE)
	 (DEFPROP ,name ,handler-name DEFSFA-HANDLER)
	 (DEFPROP ,name ,vars DEFSFA-INITS)
	 ,.(mapcan #'(lambda (var)
		       (if (pairp var) (setq var (car var)))
		       (setq accessor (symbolconc name '- var)
			     idx (1+ idx))
		       `( (DEFPROP ,accessor ,idx DEFSFA-IDX)
			  (DEFPROP ,accessor SI:DEFSFA-ACCESSOR MACRO)))
		   vars))
       (DEFUN ,handler-name (,sfa ,operation ,data)
	 (CASEQ ,operation
	  ,@(mapcan #'(lambda (clause)
			(if (atom (cadr clause))
			    `((,(car clause)
			       (LET ((,(cadr clause) ,data))
				 ,@(cddr clause))))))
		    ops)
	  (:SEND (DESETQ (,operation ,data) ,data)
		 (CASEQ ,operation
		   ,@(mapcan #'(lambda (clause)
				 (if (not (atom (cadr clause)))
				     `((,(car clause)
					(LET ((,(cadr clause) ,data))
					  ,@(cddr clause))))))
			     ops)
		   (T (SFA-CALL ,sfa ,operation ,data))))
	  (WHICH-OPERATIONS
	   (IF (FBOUNDP 'SEND-AS)
	       (APPEND ',wops
		       (DELETE 'PRINT	;Temporary, has :PRINT-SELF meaning too
			       (SEND-AS MACLISP-PRIMITIVE-CLASS
					,sfa 
					'WHICH-OPERATIONS)))
	       ',wops))
	  (SI:WHICH-OPERATIONS-INTERNAL ',wops)
	  (:INIT (SI:INIT-SFA ,sfa ',name ,data))
	  (T (SFA-UNCLAIMED-MESSAGE ,sfa ,operation ,data)))))))


(def-or-autoloadable SI:DEFSFA-CREATOR EXTSFA)
(def-or-autoloadable SI:DEFSFA-ACCESSOR EXTSFA)


