;;;  MLMAC    				-*-Mode:Lisp;Package:SI;Lowercase:T-*-
;;;  *************************************************************************
;;;  ***** MacLISP ******* MacLisp-only system MACros ************************
;;;  *************************************************************************
;;;  ** (c) Copyright 1981 Massachusetts Institute of Technology *************
;;;  *************************************************************************

;; Herald is on next page, since it is defined in this file
;; (herald MLMAC /91)

(include ((lisp) subload lsp))


;; Warning! following lines must come before the subload of DEFMAX
;; ######## Glaaag! Remove these crocks after Nov 1981 -- JonL 11/12/80
#+PDP10  (progn 'compile
(and (not (getl 'FBOUNDP '(SUBR EXPR)))
     (defun FBOUNDP (x) (getl x '(SUBR FSUBR LSUBR EXPR FEXPR MACRO))))
(and (not (getl 'PAIRP '(SUBR EXPR)))
     (defun PAIRP (x) (eq (typep x) 'LIST)))
)


(def-or-autoloadable VALUES-LIST MLSUB)
(def-or-autoloadable MULTIPLE-VALUE-LIST/| MLSUB)
(def-or-autoloadable MSETQ-CALL UMLMAC)
(def-or-autoloadable MSETQ-RETURN UMLMAC)
(def-or-autoloadable GENTEMP MACAID)
(def-or-autoloadable +INTERNAL-PERMUTIBLE-P MACAID)


;;Might as well - output of defmacro for HERALD will require it anyway
(subload DEFMAX)


(declare (genprefix |mmac|) )

(declare (own-symbol HERALD)
	 (mapex () )
	 (mapc '(lambda (x) (putprop x 'T 'SKIP-WARNING))
	       '(IF SETQ-IF-UNBOUND CATCH THROW DEFVAR PSETQ SELECTQ
		    WITHOUT-INTERRUPTS WITH-INTERRUPTS WITHOUT-TTY-INTERRUPTS
		    MULTIPLE-VALUE VALUES MULTIPLE-VALUE-BIND
		    MULTIPLE-VALUE-LIST  MULTIPLE-VALUE-RETURN  RETURN-LIST))
	 (muzzled T))


;;;; HERALD

(defmacro HERALD (group-name &optional (version-number '||) (ofile 'MSGFILES))
   (or (symbolp group-name)
       (check-type group-name #'SYMBOLP 'HERALD))
   (let* ((ifile (and (filep infile)
		      (car (last (truename infile)))))
	  (v (cond ((and ifile
			 (fixp (car (errset (readlist (exploden ifile)) () ))))
		    ifile)
		   ((symbolp version-number) version-number)
		   ('||)))
	  (putpropper `(PUTPROP ',group-name ',v 'VERSION))
	  (text (symbolconc '|;Loading | group-name '| | v)) )
      ;; Remember, this is a maclisp-only file!
     (cond ((if (get 'SHARPCONDITIONALS 'VERSION)
		(featurep '(and NIL (not MacLISP)))
		(status feature For-NIL))
	     ;;This clause should be selected only when cross-compiling
	     ;; NILAID feature set has *both* NIL and MacLISP
	    (setq text (get-pname text)))
	   ('T (setq text (copysymbol text () ))
	       (set text text)
	       (putprop text 'T '+INTERNAL-STRING-MARKER)
	       (if (status feature COMPLR)
		   (putprop text `(SPECIAL ,text) 'SPECIAL))
		 ;;In older lisps, or for cross-compilation, we simply forget
	         ;;  about delaying-until-exit the putprop of version number.
	       (setq putpropper
		     `(COND ((ALPHALESSP (STATUS LISPV) '/2071) ,putpropper)
			    ('T (PUSH `(LAMBDA (X)
					 (OR X (DEFPROP ,',group-name
							,',v
							VERSION)))
				      FILE-EXIT-FUNCTIONS))))))
     `(PROGN
	(COND ((STATUS NOFEATURE NOLDMSG)
	        (TERPRI ,ofile)
		(PRINC ,text ,ofile)))
	,putpropper
	',v)))


(herald MLMAC /91)




;; Basically, most of these "FSUBR" macros only need to be "un-cached"
;;  if they are redefined.
(eval-when (eval compile)
    (setq defmacro-displace-call MACROEXPANDED)
)

(defmacro IF (p c &rest a)
    (cond (a `(COND (,p ,c) (T ,.a)))
	  (T `(AND ,p ,c))))


(defmacro SETQ-IF-UNBOUND (&rest args)
  (do ((a args (cddr a))
       (l () (cons `(OR (BOUNDP ',(car a)) (SETQ ,(car a) ,(cadr a))) l)))
      ((null a)
       (cond ((null (cdr l)) (car l))
	     (`(PROGN ,.(nreverse l)))))))


(defmacro CATCH (&whole f comp &optional tag)
   (si:catch-throw-service f comp tag '*CATCH))

(defmacro THROW (&whole f comp &optional tag)
    (si:catch-throw-service f comp tag '*THROW))

(defun SI:CATCH-THROW-SERVICE (f comp tag newfun)
   (si:obsolete-form-msg f newfun)
   (cond ((and tag (not (symbolp tag)))
	  (terpri msgfiles)
	  (princ '|Warning! Possible syntax error -- the 'tag' part of your old  | msgfiles)
	  (princ (car f) msgfiles)
	  (terpri msgfiles)
	  (princ '|    | msgfiles)
	  (princ tag msgfiles)
	  (princ '| is not a symbol. | msgfiles)))
   `(,newfun ',tag ,comp))


(defun SI:OBSOLETE-FORM-MSG (f fun)
   (terpri msgfiles)
   (princ '|WARNING! An obsolete form, | msgfiles)
   (prin1 f msgfiles)
   (terpri msgfiles)
   (princ '|    is being expanded - please use | msgfiles)
   (princ fun msgfiles)
   (princ '| instead.| msgfiles))



;; (DEFVAR sym value documentation)
;; SETQ-IF-UNBOUND so can initialize a var before loading the file which
;; DEFVARs it.  "documentation" is optional
;; Consider: (SETQ GRINDEF-MAGIC-PARAMETER 34.)


(defmacro DEFVAR (var &optional (val () valp) () )	;3rd = documentation
  `(PROGN 'COMPILE
	  (EVAL-WHEN (EVAL LOAD COMPILE)
	     (AND (STATUS FEATURE COMPLR) (SPECIAL ,var)))
	  ,.(if valp `((SETQ-IF-UNBOUND ,var ,val)))
	  ',var))

(defmacro DEFCONST (var &optional (val () valp) () )	;3rd = documentation
  `(PROGN 'COMPILE
	  (EVAL-WHEN (EVAL LOAD COMPILE)
	     (AND (STATUS FEATURE COMPLR) (SPECIAL ,var)))
	  ,.(if valp `((SETQ ,var ,val)))
	  ',var))


;; PSETQ looks like SETQ but does its work in parallel.
(defmacro PSETQ (&rest rest)
     (cond ((cddr rest)
	    `(SETQ ,(car rest)
		   (PROG1 ,(cadr rest) (PSETQ . ,(cddr rest)))))
	   ((cdr rest) `(SETQ  ,.rest))
	   ('T (error '|Odd number of args| (cons 'PSETQ rest)))))



;;; Interrupt hackers
(defmacro WITHOUT-INTERRUPTS (&body forms)
  `(LET ((+INTERNAL-WITHOUT-INTERRUPTS T))
     . ,forms))

(defmacro WITH-INTERRUPTS (&body forms)
  `(LET ((+INTERNAL-WITHOUT-INTERRUPTS ()))
     . ,forms))

(defmacro WITHOUT-TTY-INTERRUPTS (&body forms)
  `(LET ((+INTERNAL-WITHOUT-INTERRUPTS 'TTY))
     . ,forms))

(or (fboundp 'SELECTQ)
    (equal (get 'SELECTQ 'AUTOLOAD) #%(autoload-filename UMLMAC))
    (defun SELECTQ macro (x)
      ((lambda (n FASLOAD)
	 (cond ((null n))
	       ((alphalessp n "26")
		 (remprop 'UMLMAC 'VERSION))
	       ((+internal-lossage 'UMLMAC 'SELECTQ n)))
	 (load #%(autoload-filename UMLMAC))
	 (macroexpand x))
       (get 'UMLMAC 'VERSION)
       () )))



;;;; Multiple-value stuff

;;; Variables   *:AR2 *:AR3 *:AR4 *:AR5 *:AR6 *:AR7 *:AR8 *:ARn *:ARlist
;;;  are automatically specialized by the COMPLR

(eval-when (eval compile)
  (setq retvec-vars '(*:AR2 *:AR3 *:AR4 *:AR5 *:AR6 *:AR7 *:AR8)
	max-retvec (length retvec-vars))
)

(let ((x '#.`(*:ARlist *:ARn ,.retvec-vars)))
  (if (boundp '+INTERNAL-INTERRUPT-BOUND-VARIABLES)
      (or (memq '*:AR2 +INTERNAL-INTERRUPT-BOUND-VARIABLES)
	  (memq '*:ARlist +INTERNAL-INTERRUPT-BOUND-VARIABLES)
	  (setq +INTERNAL-INTERRUPT-BOUND-VARIABLES
		(append x +INTERNAL-INTERRUPT-BOUND-VARIABLES)))
      (setq +INTERNAL-INTERRUPT-BOUND-VARIABLES x)))


;;; *:ARn holds the number of extra-return values -- 0 at top level
;;; *:ARlist holds a list of all return values beyond the 8'th
(setq *:ARn 0 *:ARlist () )



(defun MULTIPLE-VALUE-tester/| (val)
    ;; Returns () if it is likely that evaluation of 'val' cannot
    ;;  produce multiple values.  T says it probably can produce them.
   (cond
     ((atom val) () )
     ((eq (car val) 'VALUES) 'T)
     ((atom (setq val (macroexpand val))) () )
     ((not (atom (car val)))
       (and (eq (caar val) 'LAMBDA)
	    (MULTIPLE-VALUE-tester/| (car (last (cddar val))))))
     ((not (symbolp (car val))) () )
     ((let ((type (sysp (car val))))
	(cond
	  ((not (memq type '(SUBR LSUBR FSUBR)))) ;;Non-system subrs might!
	  ((eq type 'LSUBR)
	    (cond ((cond ((eq (car val) 'PROG2) (pop val) 'T)
			 ((eq (car val) 'PROG1) 'T))
		    (and (null (cddr val))
			 (MULTIPLE-VALUE-tester/| (cadr val))))
		  ((eq (car val) 'PROGN)
		    (MULTIPLE-VALUE-tester/| (car (last val))))
		  ((memq (car val) '(FUNCALL APPLY EVAL LEXPR-FUNCALL SEND
				     SEND-AS LEXPR-SEND LEXPR-SEND-AS)))))
	  ((eq type 'FSUBR)
	    (cond ((cond ((eq (car val) 'COND))
			 ((eq (car val) 'CASEQ) (pop val) 'T))
		     ;; COND's are permissible only if each clause is OK
		    (MULTIPLE-VALUE-tester-aux/|
		       (mapcar #'CAR (mapcar #'LAST (cdr val)))))
		  ((eq (car val) 'OR)
		    (MULTIPLE-VALUE-tester-aux/| (cdr val)))
		  ((eq (car val) 'AND)
		    (MULTIPLE-VALUE-tester/| (car (last val))))
		  ((memq (car val) '(SUBRCALL LSUBRCALL))))))))))


(defun MULTIPLE-VALUE-tester-aux/| (l)
    ;; Check to see that each item on list l is permissible for MULTIPLE-VALUEs
  (do ()
      ((null l) 'T)
    (if (null (MULTIPLE-VALUE-tester/| (car l)))
	(return () ))
    (pop l)))



;;FOO! this definition must be compiled before subsequent re-definitions for
;; MULTIPLE-VALUE, so that MACROEXPAND-1*M can be properly called.

(defun MULTIPLE-VALUE-expander/|
       (varlist val original &aux (nxvals 0)  stql  ck-no-retvals  atomic-valp)
  (declare (fixnum nxvals))
   ;;First value is normal return value -- rest are passed thru
   ;;  the special variables *:AR2, *:AR3, ... *:AR8
   ;;The number of extra return values is stored in *:ARn.
   ;; In addition to checking everything, Returns a list of the expanded
   ;; 'val' form, a form to check the number of return-values, and a
   ;; list of pairs of things to be setq'd
  (do ((ex? 'T))
      ((cond ((null ex?))
	     ((not (pairp val)) (setq atomic-valp 'T) 'T)
	     ((memq (car val) '(VALUES VALUES-LIST)))))
      ;;Say, there a circularity here -- omit the macroexpand for "bootstrap"
    (multiple-value (val ex?) (macroexpand-1*m val)))
  (cond ((null varlist) () )
	  ;;() means no vars to be SETQ'd -- an OK case.
	((or (atom varlist)
	     (> (setq nxvals (length (cdr varlist))) 255.)
	      ;;Atomic "computation" can't produce "extra" multiple values
	     (and (> nxvals 0) atomic-valp))
	 (error '|Bad varlist for MULTIPLE-VALUEing macro| original))
	((and (> nxvals 0)
	      (cond (atomic-valp 'T)
		    ((memq (car val) '(VALUES VALUES-LIST)) () )
		    ((not (MULTIPLE-VALUE-tester/| val)))))
	  ;;No system function currently returns multiple values -- 2/4/81
	 (error "This form does not guarantee multiple return values"
		original)))
  (setq stql (mapcan #'(lambda (var slot) (if var (list `(,var ,slot))))
		     (cdr varlist)
		     '#.retvec-vars))
  (cond ((> nxvals 0)
	 (setq ck-no-retvals `(AND (< *:ARn ,nxvals)
				   (SI:CHECK-MULTIPLICITIES ,nxvals)))
	 (if (> nxvals #.max-retvec)
	     (setq stql (nconc stql
			       `((,(nthcdr #.max-retvec (cdr varlist))
				   (PROG1 *:ARlist (SETQ *:ARlist () )))))))))
  (if (not atomic-valp)
       ;; Shield against spurious propogation of "values" in *:ARn
      (setq val `(PROG2 (SETQ *:ARn 0) ,val ,ck-no-retvals (SETQ *:ARn 0))))
  (list val stql))



;;;; MULTIPLE-VALUE-LIST,  MULTIPLE-VALUE,  MULTIPLE-VALUE-BIND,  VALUES,
;;;; RETURN-LIST,  MULTIPLE-VALUE-RETURN


(defmacro MULTIPLE-VALUE-LIST (form)
  `(PROG2 (SETQ *:ARn 0) (MULTIPLE-VALUE-LIST/| ,form)))


(defmacro MULTIPLE-VALUE (varlist val &whole original
			  &aux (first-var (car varlist)) (SETQer 'SETQ))
   (let (((val stql)
	 (MULTIPLE-VALUE-expander/| varlist val original)))
      ;;First, see if any destructuring has to take place
     (mapc #'(lambda (bind-spec)
	       (cond ((null bind-spec) )
		     ((or (not (symbolp (setq  bind-spec (car bind-spec))))
			  (memq bind-spec '(() T)))
		      (setq SETQer 'DESETQ))))
	   stql)
     (if stql (setq stql `(,SETQer ,.(apply #'NCONC stql))))
     (cond ((null first-var)
	    `(PROG1 ,val ,stql))
	   ((symbolp first-var)
	    `(PROGN (SETQ ,first-var ,val)
		    ,stql
		    ,first-var))
	   ((let (g)
	      (si:gen-local-var g)
	      `(LET ((,g ,val))
		 (DESETQ ,first-var ,g)
		 ,stql
		 ,g))))))


(defmacro MULTIPLE-VALUE-BIND (varsl form &body body &whole original)
   ;; Even though there is an explicit call macroexpand here, it should
   ;;  only really call the macro MULTIPLE-VALUE, which probably will be
   ;;  be redefined only when this macro is redefined too.
  (let (((val letlist)
	 (MULTIPLE-VALUE-expander/| `(() ,.(cdr varsl)) form original)))
    `(LET ((,(car varsl) ,val)
	   ,.letlist )
	,.body)))



(defmacro (VALUES defmacro-displace-call 'T)  (&rest w)
  (let ((nxvals (1- (length w))) (first-value (car w)) stql)
    (declare (fixnum nxvals))
    (if (< nxvals 1) (setq nxvals 0))
    (setq stql (mapcan #'LIST '#.retvec-vars (cdr w)))
    (if (> nxvals #.max-retvec)
	(setq stql (nconc stql
			  `(*:ARlist (LIST ,.(nthcdr #.(1+ max-retvec) w))))))
    (if stql (setq stql `((PSETQ ,.stql))))
    `(PROG1 ,first-value
	    ,.stql
	    (SETQ *:ARn ,nxvals))))


(defmacro RETURN-LIST (l)
    ;; Return from a PROG, spreading out the elements of the argument
    ;; (which should be a non-null list) into the m-v state
   `(RETURN (VALUES-LIST ,l)))

(defmacro MULTIPLE-VALUE-RETURN (form) `(RETURN ,form))




