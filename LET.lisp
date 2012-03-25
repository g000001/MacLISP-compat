;;; LET  -*-mode:lisp;package:si-*-				    -*-LISP-*-
;;; **************************************************************************
;;; ******** NIL ******** LET With Destructuring  ****************************
;;; **************************************************************************
;;; ******** (C) Copyright 1981 Massachusetts Institute of Technology ********
;;; ************ THIS is a read-only file! (all writes reserved) *************
;;; **************************************************************************

;;; For MacLISP, to compile NALET (version which destructures over vectors),
;;;  just load the SHARPC module, and set TARGET-FEATURES to 'NILAID

(in-package :maclisp)


;;;; Temporary macros
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar |LET.dcmp-tempvars|)
  (defvar |LET.gensym-tempvars?| 'T))

(eval-when (:compile-toplevel :execute)

;;; Leave these as defined by "macro" rather than "defmacro", so that
;;;  one has a ghost of a chance of interpreting this file.
;;; Leave inside the eval-when so that the fool LISPM can win


  (deftype pair () 'cons)

  (deftype vector-s () 'vector)

  (deftype extend () 'vector)

  (deftype constant (&rest args)
    (declare (ignore args))
    `(satisfies constantp))

  (defun vector-length (vec)
    (declare (vector vec))
    (length vec))

  (defun pairp (obj)
    (typep obj 'pair))

  (defmacro TYPECASEQ (item &body clauses)
    `(typecase ,item
       ,@(mapcar (lambda (c)
                   (if (consp (car c))
                       `((OR ,@(car c)) ,@(cdr c))
                       c))
                 clauses)))

  (defmacro FIXNUMP (w)
    `(TYPEP ,w 'FIXNUM))

  (defun vref (vector index)
    (declare (vector vector))
    (aref vector index))

  (defmacro TRUTHITY (&rest x)
    (declare (ignore x))
    ''T
                                        ;#N    *:TRUTH
    )

  (defmacro NON-NULL-SYMBOL (x)
                                        ;#+BadNULL
    `(AND ,x (SYMBOLP ,x))
                                        ;#-BadNULL  `(SYMBOLP ,(cadr x))
    )

  (defmacro QSEQUENCEP (x)
    `(TYPECASEQ ,x
       ((PAIR VECTOR VECTOR-S) 'T)
       (T () )))

;;; Here is the non-destructuring version of LET!
  (defmacro BIND-LET (&body body)
    ((lambda (ll w vars vals)
       (do ((l ll (cdr l)))
           ((null l))
         (push (cond ((atom (car l)) (push () vals) (car l))
                     ('T (push (cadar l) vals) (caar l)))
               vars))
       `((LAMBDA (,.(nreverse vars)) ,.w) ,.(nreverse vals)))
     (car body) (cdr body) () () ))

;;; DOMAP-AND evaluates a form, on successive tails of a list, returning ()
;;;  if any of the evaluations if (), and returning the last one if not.
;;; DOMAP-OR returns the first non-() one, or () if all are ().
;;; Syntax is (DOMAP-and/or (VAR1 <first-form>) ... (VARn <last-form>) <pred>)
;;;   Items in angle-brackets are evaluated, and the names "VARi" are used
;;;   as the stepping variables to use;  <pred> is a "predicate" form.
;;;   Typical use -  (DOMAP-AND (TEMP DATA-LIST) (NOT (LOSEP (CAR TEMP))))
  (defmacro DOMAP-AND (&body x)
    (bind-let ((forms x) pred (g (gensym)))
              (setq pred (car (setq forms (reverse forms)))
                    forms (nreverse (cdr forms)))
              `(DO ((,g)
                    ,.(mapcar #'(lambda (x) `(,(car x) ,(cadr x) (CDR ,(car x))))
                              forms))
                   ((NOT (AND ,.(mapcar #'CAR forms))) ,g)
                 (OR (setq ,g ,pred) (RETURN () )))))

  #|(domap-and (temp '(1 2 3 4))
  (t2 '(1 2 3 4))
  (list temp t2))|#


  (defmacro DOMAP-OR (&body x)
    (bind-let ((forms x) pred (g (gensym)))
              (setq pred (car (setq forms (reverse forms)))
                    forms (nreverse (cdr forms)))
              `(DO ((,g)
                    ,.(mapcar #'(lambda (x) `(,(car x) ,(cadr x) (CDR ,(car x))))
                              forms))
                   ((NOT (AND ,.(mapcar #'CAR forms))) () )
                 (AND (setq ,g ,pred) (RETURN ,g)))))

  
;;;???
  (defmacro |LET.repeated?| (x)
    (bind-let ((l (gensym)))
              `(DO ((,l ,x))
                   ((NULL ,l) () )
                 (AND (MEMQ (CAR ,l) (CDR ,l)) (RETURN 'T))
                 (POP ,l))))

  (defmacro PUSHNRL (item lname)
    `(SETQ ,lname (NRECONC ,item ,lname)))

;;; Renamings!  Due to certain symbols already being in pure LISP etc.,
;;;   so its cheaper to use them, but these names are more descriptive.
  (defmacro |LET.do-a-subform| (&body x) `(|LET.step&decompose| ,.x))
  (defmacro |LET.find-rightmost| (x)
    `(|LET.match-vars| () ,x -1 () ))
  (defmacro |LET.in-pattern?| (x y)
    `(|LET.match-vars| ,x ,y +1 () ))
  (defmacro NOVARS? (x) `(NOT (|LET.match-vars| () ,x +1 () )))
  (defmacro |LET.listallvars| (x y)
    `(|LET.match-vars| (truthity) ,x +1 ,y))

  )	;end of temporary macros


;;;; LET decomposer

;;; Following function produces code to perform the decomposition
;;;  indicated by the pattern.

(DEFUN |LET.decompose| (PAT VAR USEP)
  (AND
   PAT
   (TYPECASEQ PAT
     (SYMBOL `((SETQ ,pat ,var)) )		;What could be simpler!
     (PAIR
      (COND  ;Here are the simple cases, do one binding to an atom and go on
                                        ;  destructuring other one.  Case of pattern ((...) . <atom>)
        ((NOT (QSEQUENCEP (CAR PAT)))
         (|LET.do-1-atom| 'CAR (CAR PAT) (CDR PAT) VAR USEP))
        ((NOT (QSEQUENCEP (CDR PAT)))
         (|LET.do-1-atom| 'CDR (CDR PAT) (CAR PAT) VAR USEP))
        ('T ;Complex case, both car,cdr of pattern are non-atomic
                                        ;First, see if some non-atomic subform is fake (no vars)
         (BIND-LET ((OP))
                   (COND ((COND ((NOVARS? (CAR PAT))
                                 (SETQ OP 'CAR PAT (CDR PAT))
                                 'T)
                                ((NOVARS? (CDR PAT))
                                 (SETQ OP 'CDR PAT (CAR PAT))
                                 'T))
                          (|LET.do-1-atom| OP () PAT VAR USEP))
                         ((NCONC (|LET.do-a-subform| 'CAR
                                                     (CAR PAT)
                                                     VAR
                                                     (truthity))
                                 (|LET.do-a-subform| 'CDR
                                                     (CDR PAT)
                                                     VAR
                                                     USEP))))))))
     ((VECTOR VECTOR-S EXTEND)
      (DO ((I 0 (1+ I))
	   (LN (VECTOR-LENGTH PAT))
	   (VDCMPL () ) (SUBPAT () ))
	  ((NOT (< I LN)) (NREVERSE VDCMPL))
	(AND (SETQ SUBPAT (VREF PAT I))
	     (TYPECASEQ SUBPAT
	       (SYMBOL (PUSH `(SETQ ,subpat (VREF ,var ,i))  VDCMPL))
	       ((PAIR VECTOR)
		(PUSHNRL (|LET.do-a-subform| I SUBPAT VAR (truthity)) VDCMPL))
               (T () )))))
     (T () ))))



;;; Come here with an atomic "APAT" (A-pattern), and output a SETQ
;;;   corresponding to having taken the "CARCDR" operation over "VAR".
;;;   (but no code unless APAT is actually a symbol).   Then continue
;;;   the decomposing on "DPAT".  If DPAT is actually decomposable,
;;;   then it corresponds to taking the other "carcdr" operation  on "PAT".
;;; "VAR" is the code over which we are taking the car/cdrs, and generally
;;;   is some temp variable; but for LISPM style, it *** may someday ** be
;;;   compositions like  "(CAR (CDR Z))" instead of merely "G0012".
;;; "USEP" non-null means that "VAR" may be used as a temporary variable
;;;   during the destructuring of the DPAT part.

(DEFUN |LET.do-1-atom| (CARCDR APAT DPAT VAR USEP)
                                        ;Should we think a bit more about selecting a better choice for
                                        ;  the sub-recursive "VAR" to use as a temp var?
  (BIND-LET
    ((SET-1-VAR (AND (NON-NULL-SYMBOL APAT) `(SETQ ,apat (,carcdr ,var))) )
     DCMPL DSYM?)
    (COND ((NULL DPAT) () )
          ((TYPECASEQ DPAT
             (SYMBOL (SETQ DSYM? 'T) 'T)
             ((PAIR VECTOR VECTOR-S)
              (NOT (NOVARS? DPAT))))
                                        ;Switch the "carcdr" sense, to do the other half
           (SETQ CARCDR (COND ((EQ CARCDR 'CAR) 'CDR) ('CAR)))
           (COND ((EQ APAT VAR)
                                        ;Lousy case when the variable assignment must be done
                                        ; last, due to it being the same as the destructure base
                  (PUSH SET-1-VAR DCMPL)
                  (SETQ SET-1-VAR () )
                  (AND (EQ USEP VAR) (SETQ USEP (truthity)))))
           (COND (DSYM? (PUSH `(SETQ ,dpat (,carcdr ,var)) DCMPL))
                 ('T (SETQ DSYM? (|LET.do-a-subform| CARCDR DPAT VAR USEP))
                     (SETQ DCMPL (NCONC DSYM? DCMPL))))))
    (AND SET-1-VAR (PUSH SET-1-VAR DCMPL))
    DCMPL))




;;; Only come here when PAT is either a PAIR or VECTOR.
;;; USEP null means we can't use the variable VAR for intermediate temps, and
;;;   must get a temporary variable for "optimize"-style destructuring.
;;;   these temp vars are in a list, pointed to by the cdr of
;;;   |LET.dcmp-tempvars|, so that lambda-binding may shield parts
;;;   of the list;  we shield over a piece of code in which we don't want
;;;   certain variables to be used.
;;; USEP = #T is similar to (), but means test out |LET.gensym-empvars?|
;;;   to determine whether to gensym a new var, or get one from the pattern.
;;; USEP = <symbol> means use that symbol for a temp var.

                                        ;This function should really be called |LET.do-a-subform|
(DEFUN |LET.step&decompose| (CARCDR PAT VAR USEP)
  (AND (NOT (NOVARS? PAT))
       (BIND-LET ((ACCESSOR (COND ((FIXNUMP CARCDR)
                                   `(VREF ,var ,carcdr))
                                  (`(,CARCDR ,var)))
                            ))
                 (COND ((OR (NULL USEP)
                            (AND (EQ USEP (truthity)) |LET.gensym-tempvars?|))
                        (COND ((NULL |LET.dcmp-tempvars|) (ERROR "LET.do-a-subform"))
                              ((NULL (CDR |LET.dcmp-tempvars|))
                               (RPLACD |LET.dcmp-tempvars| (LIST (GENSYM)))))
                        (BIND-LET ((|LET.dcmp-tempvars| |LET.dcmp-tempvars|))
                                  (SETQ VAR (CADR |LET.dcmp-tempvars|))
                                  (POP |LET.dcmp-tempvars|)
                                  `((SETQ ,var ,accessor)
                                    ,. (|LET.decompose| pat var var))))
                       ((COND ((EQ USEP (truthity))
                               (NULL (SETQ VAR (|LET.find-rightmost| PAT))))
                              (USEP (NOT (EQ USEP VAR)))
                              ('T))
                        (ERROR "~A LET.do-a-subform" PAT))
                       ('T `((SETQ ,var ,accessor)
                             ,. (|LET.decompose| pat var usep)) )))))


;;;; |LET.match-vars|

;;; This foolish function ought to be in the system!

;;; If "|LET.matchp|" is null, then simply search for any variable
;;;   going in the specified direction.  If |LET.direction| is +1,
;;;   then go in the "CAR" direction, ie left-to-right in print order;
;;;   if -1, then in the "CAR", or right-to-left, direciton.
;;; If "|LET.matchp|" is #T, then list all variables in the pattern, by
;;;   pushing onto the fourth argument;  otherwise,
;;; If "|LET.matchp|" is non-null, then search for occurrence
;;;   of that particular variable.
;;; Returns null if there aren't any variables in the pattern;
;;;    otherwise, returns variable which satisfies "|LET.matchp|".

(defvar |LET.direction|)
(declaim (fixnum |LET.direction|))

(defvar |LET.matchp|)
(defvar |LET.direction|)
(defvar |LET.listallvars|)

(DEFUN |LET.match-vars| (|LET.matchp| PAT |LET.direction| |LET.listallvars|)
  (|LET.anyvarsp| PAT))

(DEFUN |LET.anyvarsp| (PAT)
  (AND PAT
       (TYPECASEQ PAT
         (SYMBOL (COND ((OR (NULL |LET.matchp|) (EQ |LET.matchp| PAT))
                        PAT)
                       ((EQ |LET.matchp| (truthity))
                        (PUSH PAT |LET.listallvars|)
                        |LET.listallvars|)))
         (VECTOR
          (PROG (LN IX TMP)
                                        ;    (DECLARE (FIXNUM LN IX))
                (SETQ LN (LENGTH PAT)
                      IX (COND ((= |LET.direction| -1) (1- LN))
                               ('T  0)))
             TG (AND (= 0 LN)
                     (RETURN (AND (EQ |LET.matchp| (truthity))
                                  |LET.listallvars|)))
                (AND (SETQ TMP (|LET.anyvarsp| (VREF PAT IX)))
                     (NOT (EQ |LET.matchp| (truthity)))
                     (RETURN TMP))
                (SETQ IX (+ |LET.direction| IX) LN (1- LN))
                (GO TG)))
         (PAIR (COND ((EQ |LET.matchp| (truthity))
                      (COND ((= |LET.direction| -1)
                             (|LET.anyvarsp| (CDR PAT))
                             (|LET.anyvarsp| (CAR PAT)))
                            ('T (|LET.anyvarsp| (CAR PAT))
                                (|LET.anyvarsp| (CDR PAT))))
                      |LET.listallvars|)
                     ((= |LET.direction| -1)
                      (OR (|LET.anyvarsp| (CDR PAT))
                          (|LET.anyvarsp| (CAR PAT))))
                     ((OR (|LET.anyvarsp| (CAR PAT))
                          (|LET.anyvarsp| (CDR PAT) )))))
         (T () ))))


;;;; LET and LET* Expanders

(DEFUN LET-expander-1 (L)
  (PROG (LETL LMBODY |LET.dcmp-tempvars| VARS VALS EXCEPTIONS
              GVAR DECLP DCMPL LL OK-FL ALLFLATS  NVAR NVAL)
        (SETQ LETL (CAR L) LMBODY (CDR L))
        (SETQ |LET.dcmp-tempvars| (LIST () ) OK-FL 'T)
        (COND ((AND (NOT (ATOM (CAR LMBODY))) (EQ (CAAR LMBODY) 'DECLARE))
               (SETQ DECLP (LIST (CAR LMBODY)))
               (POP LMBODY)))
        (IF (NULL LMBODY) 		;If you ask me [JonL - 12/1/80]
            (PUSH () LMBODY))		; (LAMBDA (...)) should be a bug
        (MAPC
         #'(LAMBDA (IL)
	     (SETQ NVAR () NVAL () LL () )
	     (COND
	       ((NOT OK-FL))
	       ((NULL IL) (SETQ OK-FL () ))
	       ((TYPECASEQ IL
		  (SYMBOL (SETQ NVAR IL))
		  (PAIR
                   (COND
                     ((AND (NOT (ATOM (CDR IL)))
                           (CDDR IL))
                      (SETQ OK-FL () ))
                     ((NULL (CAR IL))
                      (setq nvar ()
                            nval (macroexpand (cadr il)))
                      (and (or (not (pairp nval))
                               (eq (car nval) 'QUOTE))
                           (setq nval () )))
                     ((TYPECASEQ (CAR IL)
                        (SYMBOL (SETQ NVAR (CAR IL) NVAL (CADR IL)) )
                        ((PAIR VECTOR VECTOR-S)
                         (SETQ ALLFLATS (|LET.listallvars| (CAR IL) ALLFLATS))
                         (COND ((COND ((NULL (CADR IL)) () )
                                      ((NULL |LET.gensym-tempvars?|)
                                       (SETQ GVAR (|LET.find-rightmost| (CAR IL)))
                                       (PUSH GVAR EXCEPTIONS)
                                       'T)
                                      ((AND (PAIRP (CAR IL))
                                            (SYMBOLP (CAAR IL))
                                            (NOVARS? (CDAR IL)))
                                       (PUSH (SETQ GVAR (CAAR IL)) VARS)
                                       (PUSH `(CAR ,(cadr il)) VALS)
                                       (PUSH GVAR EXCEPTIONS)
                                       () )
                                      ('T (SETQ GVAR (GENSYM)) 'T))
                                (SETQ LL (|LET.decompose| (CAR IL) GVAR GVAR))
                                (SETQ NVAR (AND LL GVAR) NVAL (CADR IL)))))
                        (T (SETQ OK-FL () ))))))
		  (T (SETQ OK-FL () )))))
	     (COND (OK-FL (PUSH NVAR VARS)
			  (PUSH NVAL VALS)
			  (AND LL (SETQ DCMPL (NCONC LL DCMPL))))))
         LETL)
        (AND (OR (NOT OK-FL) (|LET.repeated?| ALLFLATS))
             (ERROR "~A Bad variable list in LET" LETL))
        (SETQ DCMPL (|LET.optimize| DCMPL ALLFLATS))	    ;POPs tempvars also
        (AND EXCEPTIONS
             (MAPC #'(LAMBDA (X) (SETQ ALLFLATS (DELQ X ALLFLATS)))
                   EXCEPTIONS))
        (SETQ ALLFLATS (NCONC |LET.dcmp-tempvars| ALLFLATS))
        (SETQ VARS (NRECONC VARS ALLFLATS)
              VALS (NRECONC VALS (make-list (LENGTH ALLFLATS))))
        (RETURN `((LAMBDA ,vars
                    ,.declp
                    ,.(nconc dcmpl lmbody))
                  ,.vals))))

(DEFUN LET*-expander-1 (L)
  (LET-expander-1
   (COND ((OR (ATOM (CAR L)) (ATOM (CDAR L))) L)
         ((BIND-LET ((LMBODY (CDR L)) DECLP)
		    (COND ((AND (NOT (ATOM (CAR LMBODY)))
				(EQ (CAAR LMBODY) 'DECLARE))
			   (SETQ DECLP (CAR LMBODY))
			   (SETQ LMBODY (CDR LMBODY))))
		    (IF (NULL LMBODY)	  ;If you ask me [JonL - 12/1/80]
			(PUSH () LMBODY)) ; (LAMBDA (...)) should be a bug
		    (PUSH 'PROGN LMBODY)
		    (MAPC #'(LAMBDA (BND) (SETQ LMBODY `(LET (,bnd) ,lmbody)))
			  (REVERSE (CAR L)))
		    (COND (DECLP `(,(cadr lmbody) ,declp ,. (cddr lmbody)))
			  ('T (CDR LMBODY))))))))

;;;; DESETQ Expander

(DEFUN DESETQ-expander-1 (LL)
  (PROG (L DCMPL GVAR GVAR-INIT ITEM PAT DS-VAR ALLFLATS VARS
           |LET.dcmp-tempvars| TMP-VAR)
	(SETQ L LL |LET.dcmp-tempvars| (LIST () ))
     LOOP-START
	(AND (NOT (PAIRP L)) (GO EXIT))
	(AND (NOT (PAIRP (CDR L))) (GO BAD))
	(SETQ PAT (CAR L) ITEM (CADR L))
        ;; Following code weeds out all but the complex patterns
        #+BadNULL
	(AND (NULL PAT) (GO FLUSH-1))
	(TYPECASEQ PAT
          (PAIR () )
          (SYMBOL   (PUSH `(SETQ ,pat ,item) DCMPL)
                    (GO LOOP-CYCLE))
          (CONSTANT (GO FLUSH-1))
          (VECTOR (TYPECASEQ ITEM
                    ((PAIR SYMBOL) () )
                    (VECTOR
                     (AND (< (VECTOR-LENGTH ITEM) (VECTOR-LENGTH PAT))
                          (GO BAD)))
                    (T (GO BAD))))
          (T (GO BAD)) )
                                        ;Fall thru here only if PAT is a PAIR or VECTOR
	(AND (NOVARS? PAT) (GO FLUSH-1))
                                        ;So now we have a valid pattern
	(AND (NULL ITEM) (GO NILLS))
	(TYPECASEQ ITEM
          (SYMBOL (COND ((OR (EQ ITEM (CAR PAT))
                                        ;Like "(DESETQ (A ...) A)"; can use A as temp
                             (EQ ITEM (SETQ DS-VAR (|LET.find-rightmost| PAT)))
                                        ;Like "(DESETQ (... B) B)"; can use B as temp
                             )
                         (SETQ TMP-VAR (SETQ DS-VAR ITEM))
                         (GO DCMP-DS-VAR))
                        ((OR (NOT |LET.gensym-tempvars?|) (SETQ DS-VAR GVAR))
                                        ;DS-VAR, if not GVAR, is from |LET.find-righmost|
                         (GO SET-DS-VAR-PUSH))
                        ((NOT (|LET.in-pattern?| ITEM PAT))
                         (SETQ DS-VAR ITEM TMP-VAR ())
                         (GO DCMP-DS-VAR))
                                        ;Fall thru to case of set GVAR to gensym
                        ('T () )))
                                        ;Normal destructuring, e.g. (desetq (f g h) (mumble 3))
          (PAIR  () )
          #+NIL     (CONSTANT (GO NILLS))
          (T (GO BAD)))
                                        ;LISTs, and some cases of SYMBOLs, fall thru to here
                                        ;Get a variable over which to destructure.
	(SETQ DS-VAR (COND (GVAR)
			   (|LET.gensym-tempvars?| (SETQ GVAR (GENSYM)))
			   ('T (|LET.find-rightmost| PAT))))
     SET-DS-VAR-PUSH
	(PUSH `(SETQ ,DS-VAR ,item)  DCMPL)
	(SETQ TMP-VAR DS-VAR)
     DCMP-DS-VAR
	(PUSHNRL (|LET.decompose| PAT DS-VAR TMP-VAR) DCMPL)
     LOOP-CYCLE
	(SETQ VARS (|LET.listallvars| PAT () ))
	(AND (|LET.repeated?| VARS) (GO BAD))
	(SETQ ALLFLATS (NCONC VARS ALLFLATS))
	(SETQ L (CDDR L))
	(GO LOOP-START)

     FLUSH-1 				;If pattern null, then just eval item
	(PUSH `(PROG2 () ,item) DCMPL)	;possibly for side-effects
	(SETQ PAT () )
	(GO LOOP-CYCLE)
     NILLS
	(MAPC #'(LAMBDA (X) (PUSH `(SETQ ,x () ) DCMPL))     ;bind a bunch of
	      (SETQ PAT (|LET.listallvars| PAT () )))	    ; variables to ()
	(GO LOOP-CYCLE)

     EXIT
	(SETQ DCMPL (NREVERSE DCMPL))
	(RETURN
          (COND ((COND ((NULL GVAR)
                        (SETQ DCMPL (|LET.optimize| DCMPL ALLFLATS))
                        (NULL |LET.dcmp-tempvars|)))
                 `(PROGN ,. dcmpl))
                ('T (AND GVAR
                         (SETQ GVAR-INIT `((,gvar ,(and (eq (caar dcmpl) 'SETQ)
                                                        (eq (cadar dcmpl) gvar)
                                                        (null (cdddar dcmpl))
                                                        (prog2 ()
                                                               (caddar dcmpl)
                                                               (pop dcmpl) ))))
                               DCMPL (|LET.optimize| DCMPL ALLFLATS)))
                    `(LET (,.gvar-init ,. |LET.dcmp-tempvars|)
                       ,. dcmpl) )))

     BAD (ERROR "~A Bad form to DESETQ" `(DESETQ ,pat ,item))
        ))



;;;; |LET.optimize|

;;; A post-optimization phase which converts
;;; 	(...(SETQ G (CAR <x>)) (SETQ G (CDR G)) ...)
;;;   into
;;;  	(... (SETQ G (CDR (CAR <x>))) ...)

(DEFUN |LET.optimize| (DCMPL ALLPATS)
  (PROG (THIS-VAR NEXT-VAR NEXT-CAR THIS-CAR DDL)
        (SETQ DCMPL (CONS () DCMPL))
        (DO ((L DCMPL))
            ((NULL (SETQ DDL (CDDR L))) () )
                                        ;(DESETQ (() 				  ;Compose certain two
                                        ; 	   (() THIS-VAR THIS-CAR) 	  ; adjacent SETQ's by
                                        ;	   (() NEXT-VAR NEXT-CAR))	  ; "splicing out" one
                                        ; 	  L)
          (SETQ THIS-CAR (CDADR L) NEXT-CAR (CDAR DDL)) ;See how much better
          (AND (OR (ATOM THIS-CAR) (ATOM NEXT-CAR))	  ; this would be if it
               (ERROR "~A LET.optimize" DCMPL))		  ; were a DESETQ!
          (SETQ THIS-VAR (CAR THIS-CAR) NEXT-VAR (CAR NEXT-CAR))
          (SETQ THIS-CAR (CADR THIS-CAR) NEXT-CAR (CADR NEXT-CAR))
          (COND ((AND (EQ THIS-VAR (CADR NEXT-CAR))	;requires unoptimized
                      (OR (EQ THIS-VAR NEXT-VAR)
                          (DO ((Z (CDR DDL) (CDR Z)))
                              ((NULL Z)
                                        ;Var not referenced in DCMPL, but ? in PAT ?
                               (NOT (|LET.in-pattern?| THIS-VAR ALLPATS)))
                            (COND ((|LET.in-pattern?| THIS-VAR (CADDAR Z))
                                        ;Var being "used"
                                   (RETURN () ))
                                  ((EQ THIS-VAR (CADAR Z))
                                        ;Var is being SETQ'd so previous value
                                   (RETURN 'T))))))  ;not needed
                 (SETQ THIS-CAR `(,(car next-car) ,(caddr (cadr l))
                                   ,. (cddr next-car)))
                 (RPLACD L `((SETQ ,next-var ,this-car) ,. (cdr ddl))))
                ('T (POP L))))
        (DO ((L |LET.dcmp-tempvars|))
            ((NULL (CDR L)) () )			;Splice out of tempvars
          (SETQ THIS-VAR (CADR L))			; any unused ones
          (COND ((DOMAP-OR (L DCMPL)
                           (OR  ;((SETQ <v> #) ...)
                            (EQ THIS-VAR (CADAR L))
                                        ;((SETQ # <carcdrings>) ...)
                            (|LET.in-pattern?| THIS-VAR (CADDAR L))))
                 (POP L))
                ('T (RPLACD L (CDDR L)))))
        (POP |LET.dcmp-tempvars|)			;Flush vacuuous NIL at
        (POP DCMPL)					; head of lists
        (RETURN DCMPL)))


;;;; Macro definitions

(DEFMACRO DESETQ (&REST L) (DESETQ-expander-1 L))

(DEFMACRO LET* (&REST L) (LET*-expander-1 L))

    ;;; WAIT! You loser, don't move this macro definition.  It should be
    ;;;   at the end, so that the previous LET will be active during
    ;;;   compilation.

(DEFMACRO LET (&REST L) (LET-expander-1 L))
