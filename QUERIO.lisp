;;; QUERIO    					    -*-Mode:Lisp;Package:SI-*-
;;; **************************************************************************
;;; ***** MACLISP ****** Build a Bi-directional SFA for Queries to User ******
;;; **************************************************************************
;;; ******** (c) Copyright 1981 Massachusetts Institute of Technology ********
;;; ************ this is a read-only file! (all writes reserved) *************
;;; **************************************************************************


(herald QUERIO /51)


(include ((lisp) subload lsp))

(eval-when (eval compile)
  (subload UMLMAC)
  )


;; Used also by CERROR file
(defun si:FRESH-LINIFY (stream)
  (if (and (sfap stream)
	   (memq 'FRESH-LINE (sfa-call stream 'WHICH-OPERATIONS () )))
       ;; If the command can be "passed down", then do so
      (sfa-call stream 'FRESH-LINE () )
       ;; Otherwise, just try a cursorpos 'A.
      (cursorpos 'A stream)))

(defun si:SPREAD-CURSORPOS (data out)
  (caseq (length data)
    (0 (cursorpos out))		 ;0 extra arg: Read pos
    (1 (cursorpos (car data) out))	 	;1 more arg : Hac some
    (T (cursorpos (car data) (cadr data) out))	; Set pos
    ))



(defvar SI:QUERY-IO-EXTRA-OPTIONS () 
  "Used to communicate to the SFA-function whether or not there are
   certain methods in the real file arrays (CURSORPOS, RUBOUT, ??)")
(defvar SI:QUERY-IO-NEW-LISP (ALPHALESSP "2090" (STATUS LISPV))) 


(defun GEN-QUERY-SLOTS macro (l)
  (pop l)
  (setq no-of-QUERY-IO-slots (length l))
  `(PROGN 'COMPILE 
	  ,.(do ((ll l (cdr ll)) (i 0 (1+ i)) (z))
		((null ll) z)
	      (push `(DEFUN ,(symbolconc '|QUERY-IO-| (car ll)) MACRO (X)
			       `(SFA-GET ,(CADR X) ,,i))
		    z))))

 ;; makes things like   (defmacro QUERY-IO-input (x) `(SFA-GET ,x 1))
(gen-query-slots output input omode imode whichops putbacklist) 

(defun BI-DIRECTIONAL-CORRESPONDENT macro (x) `(SFA-GET ,(cadr x) 'XCONS))

(defun cons-a-QUERY-IO macro (l)
  (pop l)
  (let ((x (or (get l 'IN) 
	       '(IF (STATUS STATUS TTYIFA) (STATUS TTYIFA) TYI)))
	(y (get l 'OUT))
	z setout in out)
    (si:gen-local-var in "in")
    (si:gen-local-var out "out")
    (si:gen-local-var z)
    (setq setout `(SETQ ,out (STATUS TTYCONS ,in)))
    (if y (setq setout `(OR ,out ,setout)))
    `(LET ((,in ,x) (,out ,y) ,z)
       ,setout 
       (SETQ SI:QUERY-IO-EXTRA-OPTIONS 
	     (APPEND (CDR (STATUS FILEMODE ,out))
		      ;; FILEPOS currently only gets you the output data
		     (DELQ 'FILEPOS (APPEND (CDR (STATUS FILEMODE ,in)) () ))))
       (SETQ ,z (SFA-CREATE 'QUERY-IO-HANDLER ,no-of-QUERY-IO-slots 'QUERY-IO))
       (SETF (QUERY-IO-input ,z) ,in)
       (SETF (QUERY-IO-output ,z) ,out)
       (SETF (QUERY-IO-omode ,z) (STATUS FILEMODE ,out))
       (SETF (QUERY-IO-imode ,z) (STATUS FILEMODE ,in))
        ;; For newer lisps, this permits the LISP toplevel routines to know
        ;;  that it's a bi-directional device, and probably the echo of a 
        ;;  <cr> inputted will suffice instead of also doing a (TERPRI).
       (AND SI:QUERY-IO-NEW-LISP 
	    (SETF (BI-DIRECTIONAL-CORRESPONDENT ,z) ,z))
       ,z)))




(defun QUERY-IO-HANDLER (self op data &aux (in   (QUERY-IO-input self))
					   (out  (QUERY-IO-output self))
					   (bufl (QUERY-IO-putbacklist self)))
  (cond 
    ((eq op 'UNTYI) 
       ;; For old lisps, without the UNTYI function, we support UNTYI by just
       ;;  keeping a list of the characters sent back.  Note that we could 
       ;;  support a msg to store and retrieve this slot, and thus facilitate
       ;;  a user writing a TTYBUFFER function which could keep separate from
       ;;  the base-level TYI.
      (if (not SI:QUERY-IO-NEW-LISP) 
	  (setf (QUERY-IO-putbacklist self) (cons data bufl))
	  (untyi data in)))
    ((eq op 'TYI) 
      (if (and (not SI:QUERY-IO-NEW-LISP) bufl)
	  (progn (pop bufl data) 
		 (setf (QUERY-IO-putbacklist self) bufl)
		 data)
	  (tyi data in)))
    ((cond ((memq op '(TYO PRINT PRINC)))
	   ((memq op '(READ READLINE))
	     (setq out in)
	     'T))
       ;; Several trivial operations are just "passed down" directly to 
       ;;  the appropriate part of the sfa.
      (funcall op data out))
    ((caseq op 
	(CURSORPOS  
		(if (memq 'CURSORPOS (cdr (QUERY-IO-omode self)))
		    (si:spread-cursorpos data out)
		     ;; Just do nothing now if output side can't do CURSORPOS
		    ))
	(TYIPEEK    (if (and (not SI:QUERY-IO-NEW-LISP) bufl)
			(car bufl)
			(tyipeek data in -1)))
	(OPEN       (open in data) (open out data))
	(CLOSE      (close in) (close out))
	(RUBOUT     (if (memq 'RUBOUT (cdr (QUERY-IO-omode self)))
			(rubout data out)))
	(FRESH-LINE (SI:FRESH-LINIFY out))
	((CHARPOS LINEL PAGEL PAGENUM FILEPOS 
	  CLEAR-OUTPUT FORCE-OUTPUT) 
	   ;; Notice how these funtions only pay attention to the output side
	   ;;  of the bi-directional sfa.  Also, The latter 2 better have had
	   ;;  the third sfa argument ("data") sent as ().
	  (lexpr-funcall op out data))
	(LISTEN 
	  (+ (cond ((and (not SI:QUERY-IO-NEW-LISP) bufl)
		     (length bufl))
		   (0))
	     (listen in)))
	(CLEAR-INPUT 
	  (if (and (not SI:QUERY-IO-NEW-LISP) bufl) 
	      (setf (QUERY-IO-putbacklist self) () ))
	  (CLEAR-INPUT in))
	((TTY TTYSCAN TTYINT TTYTYPE TTYSIZE OSPEED TERPRI LINMOD)
	   ;; Wow, look at all these [S]STATUS options!
	   ;; Remember, 'data' = () means STATUS, otherwise a list of args 
	   ;;  for SSTATUS to use.
	  (let (sstatusp operation-list)
	    (cond ((eq op 'TTYINT)  
		    (desetq (operation-list . sstatusp) data)
		    (let (((char-no . fun?) operation-list)
			  quotifyp)
		      (if fun? (setq fun? `(',(car fun?)) quotifyp 'T))
		      (if (not (numberp char-no))
			  (setq char-no `',char-no quotifyp 'T))
		      (if quotifyp (setq operation-list `(,char-no . ,fun?)))))
		  ('T (cond ((eq op 'TERPRI) (setq in out))
			    ((not (memq op '(TTY TTYSCAN LINMOD)))
			     (if data  
				  ;; Can't SSTATUS on TTYTYPE, TTYSIZE, OSPEED
				 (+internal-lossage 'SSTATUS 
						    'QUERY-IO-HANDLER 
						    data))
			     (setq in out)))
		      (setq operation-list data 
			    sstatusp data)))
	      ;; Note that "in" and the items in the list "data" should be
	      ;;  evaluative constants by now -- probably fixnums, or T or ().
	    (setq operation-list `(,op ,@operation-list ,in))
	    (if sstatusp
		(apply #'SSTATUS operation-list)
		(apply #'STATUS operation-list))))
	(FILEMODE 
	  ;;(status FILEMODE ...) sends () as "data", so we get the file mode
	  ;;  of the "output" side of the SFA.
	  ;;If user does (SFA-CALL <foo> 'FILEMODE 'IN), he gets input mode,
	  ;; and (SFA-CALL <foo> 'FILEMODE 'OUT) likewise gets the output mode.
	 (cond ((memq data '(() OUT))  (QUERY-IO-omode self))
	       ((eq data 'IN)          (QUERY-IO-imode self))
	       ('T (+internal-lossage 'FILEMODE 'QUERY-IO-HANDLER data))))
	 ;(TTYCONS ...)	;Is a system slot in the SFA, the "XCONS" slot and thus
			;  this status call does not send a message.
	(+INTERNAL-TTYSCAN-SUBR 
	   ;; Well, can you imagine (funcall (status ttyscan <foo>) <bar> ...)
	   ;;  so just "pass it down".
	  (+INTERNAL-TTYSCAN-SUBR in (car data) (cadr data)))
	(WHICH-OPERATIONS 
	   ;; Notice that (SFA-CALL <foo> 'WHICH-OPERATIONS <non-null-list>)
	   ;;  will store into the WHICH-OPERATIONS slot
	  (if data (setf (QUERY-IO-whichops self) data))
	  (if (null (QUERY-IO-whichops self))
	      (setf (QUERY-IO-whichops self) 
		    `(,@SI:QUERY-IO-EXTRA-OPTIONS 
		      TYI UNTYI TYIPEEK TYO READ READLINE PRINT PRINC 
		      OPEN CLOSE LISTEN CHARPOS LINEL PAGEL PAGENUM 
		      TTY TTYSCAN TTYTYPE TTYSIZE TTYINT OSPEED LINMOD 
		      FRESH-LINE CLEAR-OUTPUT FORCE-OUTPUT CLEAR-INPUT 
		      FILEMODE WHICH-OPERATIONS)))
	  (QUERY-IO-whichops self))
	(T (sfa-unclaimed-message self op data))))))


(defvar QUERY-IO 'T 
  "Where to ask questions from.  Bidirectional. SFA-form is unaffected by ^W.")

(and (eq QUERY-IO 'T)
     (status feature SFA)
     (setq QUERY-IO (cons-a-QUERY-IO)))



