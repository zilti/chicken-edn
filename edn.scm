(cond-expand
  (r7rs)
  (chicken (require-extension r7rs srfi-1)))

(use r7rs srfi-1)
(define-library edn
  (import (scheme base))
  (export
   scm-kw->edn-kw boolean->edn char->edn string->edn
   list->edn vector->edn
   parse-entry write-edn)
  (include "edn-impl.scm"))
