(SETQ *READTABLE* (COPY-READTABLE NIL))

(SET-SYNTAX-FROM-CHAR #\/ #\\)
(SET-MACRO-CHARACTER #\\ #'VALUES)
(SET-DISPATCH-MACRO-CHARACTER #\# #\N
                              (LAMBDA (STREAM CHAR ARG)
                                (DECLARE (IGNORE CHAR ARG))
                                (READ STREAM)
                                (VALUES)))
