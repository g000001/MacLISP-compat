;;; THREAD    				-*-Mode:Lisp;Package:SI;Lowercase:T-*-
;;; **************************************************************************
;;; ***** MACLISP ****** THREADed list structure functions *******************
;;; **************************************************************************
;;; ******** (c) Copyright 1981 Massachusetts Institute of Technology ********
;;; **************************************************************************

(in-package :maclisp.internal)

(herald THREAD /7)

;;;THREADs are two-way lists;  each cell has a 'car', 'cdr', and 'uncdr'.
;;;Accessing functions are respectively called THREAD-car, THREAD-cdr, and
;;;  THREAD-uncdr.  THREAD-cons takes three args: the 'car', the 'cdr', and
;;;  the 'uncdr'.
;;;Normal case is to implement them as DEFVST structures, and use the
;;;  pre-defined printing methods; otherwise, then each THREAD cell is a
;;;  list like `(THREAD ,cdr (,car . ,uncdr)), which of course could
;;;  cause circularity when printed.

; #-NIL (include ((lisp) subload lsp))
;
; #-NIL
; (eval-when (eval compile)
;   (subload SHARPCONDITIONALS)
; )
;
; #+(local MacLISP)
; (eval-when (eval compile)
;   (subload MACAID)
;   (subload UMLMAC)
;   )
;
; #+(or LISPM (and NIL (not MacLISP)))
; (progn (globalize "THREADP")
;        (globalize "THREAD-CONS")
;        (globalize "THREAD-CAR")
;        (globalize "THREAD-CDR")
;        (globalize "THREAD-UNCDR")
;
;        (globalize "THREAD-LAST")
;        (globalize "THREAD-FIRST")
;
;        (globalize "THREAD-LENGTH")
;        (globalize "THREAD-LENGTH-CDRING")
;        (globalize "THREAD-LENGTH-UNCDRING")
;
;        (globalize "THREAD-RECLAIM")
;        (globalize "THREAD-RECLAIM-CDRING")
;        (globalize "THREAD-RECLAIM-UNCDRING")
;        (globalize "THREAD-RECLAIM-1")
;        )




;;;; Structures, Vars, etc.

; (eval-when (eval compile)
;   (set-feature-query-mode 'TARGET () )
;   (if (featurep 'Minimal) (setq defmacro-for-compiling () ))
;   (if (or (featurep 'Minimal)
; 	  (and (fboundp 'DEFSTRUCT) (not (get 'DEFVST 'VERSION))))
;       (set-nofeature 'Using-DEFVST)
;       (set-feature 'Using-DEFVST))
; )


;; DEFVST will just ignore the ":type" option in the namelist

;; THREAD is a 'Two-WAy List structure', for moving forwards and backwards

#+Using-DEFVST (progn 'compile
(defvst THREAD CAR LINKS)
(defbothmacro THREADP (x) `(EQ (STRUCT-TYPEP ,x) 'THREAD))
)

(define-symbol-macro 1_20. (ash 1. 20.))


#-Using-DEFVST (eval-when (:compile-toplevel :load-toplevel :execute)

(or (boundp 'THREAD-MARKER)
    (defconst THREAD-MARKER (list :THREAD)))

(defmacro cons-a-THREAD (&rest form)
  (let ((acar (getf form 'CAR))
	(links (getf form 'LINKS)))
;    `(LIST* ,acar ,links  ,. THREAD-MARKER)
    `(LIST* ,acar ,links  ,. THREAD-MARKER)))
(defmacro THREAD-links (x) `(CADR ,x))
(defmacro THREAD-car (x) `(CAR ,x))
(defun THREADP (x)
  (and (not (atom x))
       (not (atom (cdr x)))
       (eq (cddr x) (car THREAD-MARKER))))
)


(defmacro THREAD-linkscdr (links)
  `(CDR ,links))
(defmacro THREAD-linksuncdr (links)
  `(CAR ,links))

(defmacro cons-a-THREAD-links (&rest form)
  (let ((acdr (getf form 'CDR))
	(uncdr (getf form 'UNCDR)))
    `(CONS ,uncdr ,acdr)))


(defmacro THREAD-cdr (th)
  `(THREAD-linkscdr (THREAD-links ,th)))
(defmacro THREAD-uncdr (th)
  `(THREAD-linksuncdr (THREAD-links ,th)))


(defvar THREAD-FREELIST ()
  "Chained thru CAR link of free struct cells.")



(defun THREAD-cons (tcar tcdr tuncdr &aux cell)
  (without-interrupts
     (cond ((setq cell THREAD-FREELIST)
	     (setq THREAD-FREELIST (thread-car cell))
	     (setf (thread-car cell) tcar))))
  (cond (cell
	  (let ((links (THREAD-links cell)))
	    (setf (THREAD-linkscdr links) tcdr)
	    (setf (THREAD-linksuncdr links) tuncdr))
	  cell)
	('T (cons-a-THREAD CAR tcar
			   LINKS (cons-a-THREAD-links CDR tcdr
						      UNCDR tuncdr)))))


(defun THREAD-first (cell)
  (THREAD-move cell 1_20. '(() T () ) 'THREAD-first))
(defun THREAD-last (cell)
  (THREAD-move cell 1_20. '(T T () )  'THREAD-last))
(defun THREAD-LENGTH-cdring (cell)
  (THREAD-move cell 1_20. '(T () T) 'THREAD-cdring))
(defun THREAD-LENGTH-uncdring (cell)
  (THREAD-move cell 1_20. '(() () T) 'THREAD-uncdring))
(defun THREAD-LENGTH (cell)
  (if (null cell)
      0
      (+ (thread-length-uncdring cell)
	 (if *RSET
	     (let (*RSET) (thread-length-cdring cell))
	     (thread-length-cdring cell))
	 -1)))


(defun THREAD-move (original-cell no-moves foo fun)
   "Do either CDRing or UNCDRing until either 'no-moves' moves are made,
    or until hitting the end of the thread.  Then return either the last
    (or first) cell, or return the total number of moves made."
  (declare (ignorable fun))
  (let (((cdrp previousp countp) foo)
	(circularity-limit #.(if (boundp 'NON-CIRCULAR-DEPTH-LIMIT)
				 NON-CIRCULAR-DEPTH-LIMIT
				 100000.)))
    (cond (*RSET #|(or (null original-cell)
		     (check-type original-cell #'THREADP fun))|#
		 #|(check-type no-moves #'FIXNUMP fun)|#))
    (do ((i 0 (1+ i))
	 (cell original-cell (if cdrp (THREAD-cdr cell) (THREAD-uncdr cell)))
	 (previous original-cell cell)
	 (n no-moves))
	((or (null cell) (>= i n))
	 (if (and (not (threadp previous))
		  (or previous (not (= i 0))))
	     (+internal-lossage 'NULL 'THREAD-move (maknum original-cell)))
	 (if countp i (if previousp previous cell)))
      (declare (fixnum n))
      (if (> i circularity-limit)
	#+NIL (setq circularity-limit
		    (circularity-error fun (list original-cell)))
	#-NIL (error "Circular THREAD at this address" (maknum original-cell))
	))))


;;;; THREAD reclaimers and LENGTHers

(defsimplemac THREAD-reclaim-1-f (cell)
  (let ((tmp (gen-local-var () )))
    `((LAMBDA (,tmp)
	(SETF (THREAD-linkscdr ,tmp) () )
	(SETF (THREAD-linksuncdr ,tmp) () )
	(SETF (THREAD-car ,cell) THREAD-FREELIST)
	(SETQ THREAD-FREELIST ,cell)
	() )
      (THREAD-links ,cell))))

(defun THREAD-reclaim-1 (cell)
   "User-level fun to reclaim one cell.  Probably seldom used."
  (if *RSET (check-type cell #'THREADP 'THREAD-reclaim-1))
  (without-interrupts
    (let ((prev (thread-uncdr cell))
	  (next (thread-cdr cell)))
      (THREAD-reclaim-1-f cell)
      (if prev (setf (thread-cdr prev) () ))
      (if next (setf (thread-cdr next) () ))))
  () )


(defun THREAD-reclaim-cdring (cell)
   "Reclaim all cells in the CDR-chain of this thread."
  (THREAD-reclaim-moving cell 'T 'THREAD-reclaim-cdring))

(defun THREAD-reclaim-uncdring (cell)
   "Reclaim all cells in the UNCDR-chain of this thread."
  (THREAD-reclaim-moving cell () 'THREAD-reclaim-uncdring))


(defun THREAD-reclaim (cell)
   "Reclaim all cells of this thread."
  (let ((more (and (threadp cell) (thread-uncdr cell))))
    (THREAD-reclaim-moving cell 'T 'THREAD-reclaim)
    (and more
	 (THREAD-reclaim-moving more () 'THREAD-reclaim))))


(defun THREAD-reclaim-moving (cell cdrp fun)
  (declare (ignorable fun))
  (if *RSET (check-type cell #'THREADP fun))
  (let (tem)
     ;First, disconnect any cell which may point to this one which
     ; is the firstt in a chain to be reclaimed.
    (cond (cdrp
	   (if (setq tem (thread-uncdr cell))
	       (setf (thread-uncdr tem) () )))
	  ((if (setq tem (thread-cdr cell))
	       (setf (thread-cdr tem) () )))))
  (do ()
      ((null cell) )
     ;; Interrupts locked out, but permit them 'every once in a while'.
    (without-interrupts
     (do ((i 256. (1- i)))
	 ((or (null cell) (<= i 0)) )
       (setq cell (prog1 (if cdrp (THREAD-cdr cell) (THREAD-uncdr cell))
			 (THREAD-reclaim-1-f cell))))))
  () )




;;;; :PRINT-SELF method

#+Using-DEFVST
(defmethod* (:PRINT-SELF THREAD-CLASS) (ob stream depth slashifyp)
  (declare (fixnum depth))
  (setq depth (1+ depth))
  (if (and PRINLEVEL (not (< depth PRINLEVEL)))
      (princ PRINLEVEL-EXCESS stream)
      (let ((printer (if slashifyp #'PRIN1 #'PRINC)))
	(princ "#{THREAD" stream)
	(do ((curr (THREAD-first ob) (THREAD-cdr curr))
	     (n (or PRINLENGTH 100000.) (1- n)))
	    ((cond ((or (eq curr ob) (null curr)))
		   ((<= n 0)
		     (princ " " stream)
		     (princ PRINLENGTH-EXCESS stream)
		     'T)) )
	  (declare (fixnum n))
	  (princ " " stream)
	  (funcall printer (THREAD-car curr) stream))
	(princ " $$" stream)
	(do ((curr ob (THREAD-cdr curr))
	     (n (or PRINLENGTH 100000.) (1- n)))
	    ((cond ((null curr))
		   ((<= n 0)
		     (princ " " stream)
		     (princ PRINLENGTH-EXCESS stream)
		     'T)) )
	  (declare (fixnum n))
	  (princ " " stream)
	  (funcall printer (THREAD-car curr) stream))
	(princ "}" stream))))

;;; eof
