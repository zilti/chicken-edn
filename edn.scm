(cond-expand
  (r7rs)
  (chicken (begin (require-extension r7rs srfi-1 srfi-69)
		  (import chicken))))

(use r7rs srfi-1 srfi-69 srfi-88)
(define-library (edn)
  (import (scheme base)
	  (scheme char)
	  (scheme write)
	  (scheme cxr)
	  (srfi 1)
	  (srfi 69)
	  (chicken))
  (export
   scm-kw->edn-kw boolean->edn char->edn string->edn
   list->edn vector->edn
   parse-entry write-edn read-edn)
  (include "edn-impl.scm"))
