(cond-expand
  (r7rs)
  (chicken (require-extension r7rs srfi-1 srfi-69)))

(use r7rs srfi-1 srfi-69)
(define-library (edn)
  (import (scheme base)
	  (scheme char)
	  (scheme write)
	  (srfi 1)
	  (srfi 69))
  (export
   scm-kw->edn-kw boolean->edn char->edn string->edn
   list->edn vector->edn
   parse-entry write-edn)
  (include "edn-impl.scm"))
