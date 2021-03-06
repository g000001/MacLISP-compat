;;; -*-lisp-*-

;Debugging Aids - BS, FS, B-BS  help in looking back the stack - see below.
;		- TIMIT and NTIMIT are little timers.  Say "(TIMIT foo)"
;		  to get the execution time of "foo" in microseconds; saying
;		  "(NTIMIT n foo)" will minimize over n trials.


;;;Variable "BS" holds a current frame.  One can use it in order to
;;;  direct EVALFRAME to go back down the PDL, or forward up the PDL.
;;;  [pdls push upwards, and pop downwards]

;;; Basic two functions are "BS", and "FS", which are acronymic for
;;;   "Back-down-the-Stack", AND "Forward-up-the-Stack".  See below

;;; Function "B-BS" will run a break loop in the environment indicated
;;;   by the frame in "BS"

(in-package :maclisp.internal)
#|||
(DECLARE (*FEXPR TIMIT NTIMIT BS FS)
	 (*EXPR B-BS)
	 (SPECIAL BS TIMIT)
	 (FLONUM (TIMIT)))


(DEFUN BS FEXPR (L)
;;;Go back one frame by (BS)
;;;Go back N frames by (BS <N>)  where <N> is an integer
;;;Go back to application of function BAR by (BS BAR)
;;;Go back to nth application back of BAR with (BS BAR <N>)
;;;Initialize BS to top [current] frame and then go back by
;;;  saying (BS NIL), (BS NIL <N>), (BS NIL BAR), or (BS NIL BAR <N>)
    (DECLARE (FIXNUM I N))
    (SETQ BS (COND ((AND L (NULL (CAR L)))
		    (SETQ L (CDR L))
		    (EVALFRAME NIL))
		   ((AND BS (FIXP (CADR BS))) (EVALFRAME (CADR BS)))
		   (T (EVALFRAME NIL))))
    (COND ((NULL L) BS)
	  (T (DO ((Z BS (EVALFRAME (CADR Z)))
		  (I (COND ((FIXP (CAR L)) (CAR L)) (-1)) (1- I))
		  (N (COND ((AND (CDR L) (FIXP (CADR L))) (CADR L)) (1))))
		 ((OR (NULL Z)
		      (ZEROP I)
		      (COND ((> I 0) NIL)
			    ((NOT (EQ (CAADDR Z) (CAR L))) NIL)
			    ((ZEROP (SETQ N (1- N))))))
		  (SETQ BS Z))))))

(AND (NOT (BOUNDP 'BS)) (SETQ BS NIL))


(DEFUN FS FEXPR (TEM)
;;;Go forward [up] one frame by (FS)
;;;Go forward N frames by (FS <N>)
;;;Initialize to bottom of PDL, and go forward by
;;;	(FS NIL) OR (FS NIL <N>)
  (COND ((AND TEM
	      (NULL (CAR TEM))
	      (SETQ BS (EVALFRAME 0))
	      (NULL (CDR TEM)))
	   BS)
	((AND BS
	      ((LAMBDA  (Z)
			(AND Z
			     (NUMBERP (SETQ Z (CADR Z)))
			     (> Z (CADR BS))))
		(EVALFRAME NIL)))
	  (DO I (COND (TEM (CAR TEM)) (1)) (1- I) (NOT (> I 0))
	      (DECLARE (FIXNUM I))
		(SETQ BS (EVALFRAME (- (CADR BS)))))
	   BS)))


(DEFUN B-BS NIL (EVAL '(BREAK B-BS) (CADDDR BS)))


;(COMMENT ## HELPS USE RUNTIMER)
|||#
(defvar TIMIT 0)			;THE OVERHEAD CONSTANT

(defmacro TIMIT (expr)
    ;;To time the computation (FOO X), do (TIMIT (FOO X))
   `(let ((n (get-internal-run-time)))
     ,expr
     (/ (float (- (get-internal-run-time) n timit))
        internal-time-units-per-second)))

(defmacro NTIMIT (n expr)
    `(do ((n (coerce ,n 'fixnum) (1- n))
          (s 0.0)
          (f most-positive-long-float))
         ((zerop n) f)
       (declare (fixnum n) (flonum f s))
       (and (< (setq s (timit ,expr)) f)
            (setq f s))))
#|||
(lap-a-list '((lap |timit-nop/|| subr)
	      (popj p)
	      () ))

(let (NOUUO *RSET)
 (timit nil)		;SNAP LINKS?
 (setq timit (fix (times 1.0e6 (ntimit 10. (|timit-nop/|| 'T))))))
|||#
