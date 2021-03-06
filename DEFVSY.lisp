;;; DEFVSY    				-*-Mode:Lisp;Package:SI;Lowercase:T-*-
;;; **************************************************************************
;;; ***** NIL ****** NIL/MACLISP/LISPM Structure Definer Aux, Part 2 *********
;;; **************************************************************************
;;; ******** (c) Copyright 1981 Massachusetts Institute of Technology ********
;;; **************************************************************************

;;; Auxillary file for DEFVST -- can stand alone in runtime environment.
;;; In MacLISP, this file is INCLUDE'd in DEFVST for NADEFVST

(herald DEFVSY /83)

;; Some of the following will have already been done by DEFVST when
;;  targeting for some kind of NIL  (cross-compilation, or NILAID).

#-NIL (include ((lisp) subload lsp))

#-NIL 
(eval-when (eval compile)
  (subload SHARPCONDITIONALS)
  (subload EXTEND)
  (subload EXTMAC)
  (subload VECTOR)
  )

#+(or LISPM (and NIL (not MacLISP)))
(progn (globalize "defvst-initialize/|")
       (globalize "STRUCT-TYPEP")
       (globalize "defvst-typchk/|")
       )




#+(and MacLISP (not NIL))  (progn 'compile 

(subload EXTSTR)
(def-or-autoloadable |defvst-construction-1/|| DEFVSX)

)

 
#+(local MACLISP)
(declare (mapc '(lambda (x) (putprop x T 'SKIP-WARNING))
	       '(STRUCT-TYPEP)))

(eval-when (eval compile)
  (cond ((status feature COMPLR)
	  (special STRUCT-CLASS STRUCT=INFO-CLASS |defvst-construction/||))
       #M (*lexpr SI:DEFVST-BARE-INIT SI:DEFCLASS*-1 |defvst-initialize/||))
 #+(local MacLISP) 
  (do ((i 0 (1+ i))
       (l '(VERS NAME CNSN SIZE INIS CLSS) (cdr l))
       (z))
      ((null l))
    (setq z (symbolconc 'STRUCT=INFO- (car l)))
    (eval `(DEFMACRO ,z (X) `(SI:XREF ,X ,,i))))
  (defmacro DEFVST-MACROIFY* (name fun)
    #+MacLISP `(PUTPROP ,name ',fun 'MACRO)
    #-MacLISP `(FSET ,name (CONS 'MACRO #',fun)) 
    )
  )

;(defvar SI:STRUCT=INFO-VERSION 2
;	"Version # of STRUCT=INFO guys to allow automatic compatibility")

(eval-when (eval compile load)
  (and (status feature COMPLR) (special SI:STRUCT=INFO-VERSION))
  (setq SI:STRUCT=INFO-VERSION 2)
)



;;;; STRUCT-TYPEP, |defvst-typchk/|| 

(defun STRUCT-TYPEP (x) 
   (and 
		 ;;Note that in the #+FM case, the object time environment
		 ;;  is not a priori required to have the CLASS system.
      #+(and (local PDP10) (not NIL))
     (hunkp x) 
      #N 
     (si:extendp x)
     (setq x (si:extend-class-of x))
      #+(and (local PDP10) (not NIL))
     (and (hunkp x) (eq (si:extend-marker-of x) '#.si:class-marker))
     (get (setq x (si:class-typep x)) 'STRUCT=INFO)
     x))


(declare (own-symbol |defvst-initialize/|| |defvst-typchk/||))

(defun |defvst-typchk/|| (val typl accessor-mac)
       ;;Accessor-macro name has a SELECTOR property of "(<sname> <index>)"
       ;; where <sname> is the structure name, and <index> is the vector
       ;; index corresponding to the key-name
       ;;For now, the first slot of a structure-vector is taken up by the 
       ;; &STRUCT marker, so the access of the initializations list(vector)
       ;; must be made to correspond.
   (do ()
       ((memq (typep val) typl) val)
     (let* ((selprop (get accessor-mac 'SELECTOR))
	    (sname (car selprop))
	    (key (car (si:xref (struct=info-inis (get sname 'STRUCT=INFO))
			      (cond ((eq (caddr selprop) '&REST) 0)
				    ((1+ (cadr selprop))))))))
	 (setq val (cerror 'T () ':WRONG-TYPE-ARGUMENT 
			   "~%Restriction Violation while creating a structure.  The ~2G~S component of ~S is being set to ~1G~S, which is supposed to be of type ~0G~S"
			   (if (cdr typl) typl (car typl)) val key sname)))))

(defun SI:VERIFY-DEFVST-VERSION (sname version)
  (if (= version 1)				;Version 1 and 2 are almost
      (setq version 2))				;identical
  (if (not (= version SI:STRUCT=INFO-VERSION))
      (ferror ':WRONG-TYPE-ARGUMENT
	      "~A is an unknown version of structure definition, current version = ~A"
	      sname SI:STRUCT=INFO-VERSION)))



;;;; |defvst-initialize/||

;;;Move &OPTIONAL to after VERSION once old files are flushed (after
;;;  defvst-version 1 is gone).   July 4, 1981  -- JonL --

(defun |defvst-initialize/|| (sname cnsn nkeys inis
			      &optional (version 1) source-file sname-class-var
			      &rest ignore	
			      &aux sname-class sinfo inivec? (inislength 0) )
  (declare (fixnum inislength))
  (si:verify-defvst-version sname version)
  (setq inislength (cond ((or (null inis) (pairp inis)) (length inis))
			 ('T (setq inivec? 'T)
			     (vector-length inis))))
   ;; Get STRUCT=INFO, the class, and the class variable.  The class variable
   ;; is not needed if we already have a STRUCT=INFO frob.  There can be a
   ;; class object before a STRUCT=INFO object, by loading a file with an
   ;; instance of an object before its DEFVST, thanks to USERATOMS-HOOK hackery
   (cond ((setq sinfo (get sname 'STRUCT=INFO)) 
	  (setq sname-class (STRUCT=INFO-clss sinfo))
	  (if (null sname-class)
	      (+internal-lossage 'STRUCT=INFO-clss '|defvst-initialize/||
				 sname)))
	 ((setq sname-class (get sname 'CLASS))
	  (setq sname-class-var (si:class-var sname-class)))
	 ((not (null sname-class-var)))
;;Next line a temporary hack until version 1 goes away
;; --RWK  Sunday the twenty-first of June, 1981; 4:51:26 am
;;See also the dated comment in the EXTSTR file near SI:DEFVST-BARE-INIT
	 ((setq sname-class-var (get sname 'CLASS-VAR)))
	 (T (setq sname-class-var (symbolconc sname "-CLASS"))))
   (cond 
     ((null sname-class) () )
     ((and sinfo 
	    ;;If re-defining to be the same thing, then nothing to do
	    ;; Maybe should ignore the initializations as not incompatible if
	    ;; changed?
	   (= nkeys (STRUCT=INFO-size sinfo))
	   (eq cnsn (STRUCT=INFO-cnsn sinfo))
	   (let* ((prev-inis (STRUCT=INFO-inis sinfo))
		  (ln (vector-length prev-inis)))
	     (and (= inislength ln)
		   ;;Determine whether the two 'inis' are component-wise equal.
		  (do ((i 0 (1+ i))
		       (l-v inis))
		      ((>= i ln) 'T)
		    (declare (fixnum i))
		    (if (not (equal (vref prev-inis i)
				    (if inivec? (vref l-v i) (pop l-v))))
			(return () ))))))
       () )
     ;;First defining of a class can happen via USERATOMS-HOOK, so 
     ;;  we add STRUCT=INFO
     ((null sinfo) () )
     ('T 
        #+(and MacLISP (not NIL))
       (progn (terpri msgfiles)
	      (princ ";Warning!  Incompatibly redefining the structure " msgfiles)
	      (prin1 sname msgfiles)
	      (terpri msgfiles)
	      (princ ";  Methods will not be preserved in the newly-created class." msgfiles)
	      )
        #-(and MacLISP (not NIL))
       (format ERROR-OUTPUT "~%;Warning!  Incompatibly redefining the structure ~S~%;  Methods will not be preserved in the newly-created class." sname)
	  ;;Cause new class to be used
	 (setq |defvst-construction/|| (1+ |defvst-construction/||)
	       sname-class () )))
   (cond ((or (null sname-class) (null sinfo))
	    ;;For MacLISP, following fun is defined in EXTSTR, and does the
	    ;; puptrop of the  STRUCT=INFO property, and a "si:defclass*-2"
	    ;; if needed.
	   (si:DEFVST-bare-init sname 
				sname-class-var 
				cnsn 
				nkeys 
				inis
				version
				source-file)
	    ;; Be sure to get everything up-to-date.
	   (setq sinfo       (get sname 'STRUCT=INFO)
		 sname-class (STRUCT=INFO-clss sinfo)
		 inis        (STRUCT=INFO-inis sinfo))))
   (flush-macromemos cnsn MACROEXPANDED)
    ;; Now we vivify the macros.
   (defvst-macroify* cnsn |defvst-construction-1/||)
   (putprop cnsn sname 'CONSTRUCTOR)
   (do ((i 0 (1+ i))
	(n-inis (1- (vector-length inis)))
	(selnm))
       ((= i n-inis))
     (declare (fixnum i n-inis))
     (cond ((setq selnm (cadr (vref inis (1+ i))))  ;Each inis slot is a list,
	    (flush-macromemos selnm MACROEXPANDED)  ;of KEYNAME, SELECTOR-NAME
	    (putprop selnm `(,sname ,i) 'SELECTOR)
	    (defvst-macroify* selnm |defvst-selection-1/||)))))




(eval-when (eval compile)
  (defmacro initial-STRUCT=INFO-inis-list ()
     ;; Key-names with info for default initial forms.
     ''(()					         ;&REST info
	(VERS STRUCT=INFO-VERS SI:STRUCT=INFO-VERSION )  ;1st key
	(NAME STRUCT=INFO-NAME () )		         ;2st key
	(CNSN STRUCT=INFO-CNSN () )		         ;3nd
	(SIZE STRUCT=INFO-SIZE 0 )		         ;4rd
	(INIS STRUCT=INFO-INIS () )		         ;5th
	(CLSS STRUCT=INFO-CLSS STRUCT=INFO-CLASS)) )     ;6th
  (defmacro make-initial-STRUCT=INFO-inis () 
	;;Ha!  The following code for MacLISP makes up an "initializations"
	;;  vector for a STRUCT=INFO without having VECTOR or EXTBAS loaded
     #+(and MacLISP (not NIL))
    `(SI:EXTEND ,vector-class ,.(mapcar '(lambda (x) `',x)
					     (initial-STRUCT=INFO-inis-list))) 
     #-(and MacLISP (not NIL))
    (to-vector (initial-STRUCT=INFO-inis-list)))
  )


(|defvst-initialize/|| 
	'STRUCT=INFO 
	'CONS-A-STRUCT=INFO 
	6
	(make-initial-STRUCT=INFO-inis)
	2
	(and (filep infile) (truename infile))
	'STRUCT=INFO-CLASS)


#+(and MacLISP (not NIL))  (progn 'compile 

(defun gen-autoloadables macro (x)
  `(OR (BOUNDP 'SI:XREF)
       ,.(mapcan #'(lambda (y)
		     (mapcar #'(lambda (x) 
				 `(DEF-OR-AUTOLOADABLE ,x ,(car y)))
			     (cadr y)))
		 '((EXTBAS (SI:XREF SI:XSET SI:EXTEND SI:MAKE-EXTEND 
			    SI:EXTEND-LENGTH EXTEND-LENGTH ))
		   (SENDI (EXTENDP SI:EXTENDP))))))

(gen-autoloadables)

 )

