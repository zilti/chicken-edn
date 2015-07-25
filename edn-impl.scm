(import chicken)
;; EDN Reading
;; ===========

(define edn->atom
  (case-lambda
    ((skip-fn end-fn finalizer) (lambda (subparser input)
				  (edn->atom subparser skip-fn end-fn finalizer '() '() input)))
    ((subparser skip-fn end-fn finalizer result pile input)
     (cond ((or (null? input)
		(end-fn result pile input))
	    (cons (finalizer (reverse result))
		  (if (null? input)
		      input
		      (cdr input))))
	   ((skip-fn result pile input)
	    (edn->atom subparser skip-fn end-fn finalizer result (cons (car input) pile) (cdr input)))
	   (else (edn->atom subparser skip-fn end-fn finalizer (cons (car input) result) (cons (car input) pile)
			    (if (null? input) input (cdr input))))))))

(define edn->string
  (edn->atom (lambda (result pile input)
	       (or (char=? #\\ (car input))
		   (and (null? result)
			(char=? #\" (car input)))))
	     (lambda (result pile input)
	       (and (char=? #\" (car input))
		    (not (null? pile))
		    (or (not (char=? #\\ (car pile)))
			(char=? #\" (car pile)))))
	     list->string))

(define edn->keyword
  (edn->atom (lambda (result pile input)
	       (char=? #\: (car input)))
	     (lambda (result pile input)
	       (or (char-whitespace? (car input))
		   (char=? #\, (car input))
		   (char=? #\) (car input))
		   (char=? #\] (car input))
		   (char=? #\} (car input))))
	     (lambda (in) (string->keyword (list->string in)))))

(define edn->symbol
  (edn->atom (lambda (result pile input) #f)
	     (lambda (result pile input)
	       (or (char-whitespace? (car input))
		   (char=? #\, (car input))
		   (char=? #\) (car input))
		   (char=? #\] (car input))
		   (char=? #\} (car input))))
	     (lambda (in) (string->symbol (list->string in)))))

(define edn->number
  (edn->atom (lambda (result pile input) #f)
	     (lambda (result pile input)
	       (or (char-whitespace? (car input))
		   (char=? #\M (car input))
		   (char=? #\N (car input))
		   (char=? #\, (car input))
		   (char=? #\) (car input))
		   (char=? #\] (car input))
		   (char=? #\} (car input))))
	     (lambda (in) (string->number (list->string in)))))

(define edn->rtag
  (edn->atom (lambda (result pile input)
	       (char=? #\# (car input)))
	     (lambda (result pile input)
	       (or (char-whitespace? (car input))
		   (char=? #\( (car input))
		   (char=? #\[ (car input))
		   (and (not (null? pile))
			(char=? #\{ (car pile)))))
	     (lambda (in) (cons edn/tag: (string->keyword (list->string in))))))

(define edn->coll
  (case-lambda
    ((ld rd finalize) (lambda (subparser input) (edn->coll subparser ld rd finalize '() input)))
    ((subparser ld rd finalize result input)
     (cond ((or (null? input)
		(char=? rd (car input)))
	    (cons (finalize (reverse result))
		  (if (null? input) input (cdr input))))
	   ((char=? ld (car input))
	    (edn->coll subparser ld rd finalize result (cdr input)))
	   (else (let ((compiled (subparser input)))
		   (edn->coll (first compiled)
			      ld rd finalize
			      (if (equal? (second compiled) edn/omit:)
				  result
				  (cons (second compiled) result))
			      (third compiled))))))))

(define edn->list (edn->coll #\( #\) (lambda (x) x)))
(define edn->vector (edn->coll #\[ #\] (lambda (x) (list->vector x))))

(define edn->alist
  (case-lambda
    ((subparser input) (edn->alist subparser '() input))
    ((subparser result input)
     (cond ((or (null? input)
		(char=? #\} (car input)))
	    (cons result (cdr input)))
	   ((char=? #\{ (car input))
	    (edn->alist subparser result (cdr input)))
	   (else (let ((compiled (subparser input)))
		   (edn->alist (first compiled)
			       (if (= 2 (length (car result)))
				   (cons (second compiled) result)
				   (cons (cons (caar result) (second compiled)) (cdr result)))
			       (third compiled))))))))

(define (edn->whitespace subparser input)
  (cons (car input) (cdr input)))

(define (is-char? a)
  (lambda (b)
    (and (char? b)
	 (char=? a b))))

(define (is-number? c)
  (or (char-numeric? c)
      (char=? #\+ c)
      (char=? #\- c)))

(define tag-handlers
  (list))

(define reader-handlers
  (list (cons (is-char? #\() edn->list)
	(cons (is-char? #\[) edn->vector)
	(cons (is-char? #\{) edn->alist)
	(cons (is-char? #\#) edn->rtag)
	(cons (is-char? #\:) edn->keyword)
	(cons (is-char? #\") edn->string)
	(cons char-alphabetic? edn->symbol)
	(cons is-number? edn->number)
	(cons char-whitespace? edn->whitespace)))

(define (is-tag? in)
  (and (not (list? in))
       (pair? in)
       (equal? (car in) edn/tag:)))

(define (parse-edn state)
  (lambda (struct)
    (let* ((struct-handler (cdr
			    (find (lambda (item) ((car item) (car struct)))
				  reader-handlers)))
	   (result (struct-handler (parse-edn state) struct)))
      (list (if (is-tag? (car result)) (parse-edn result) (parse-edn '()))
	    (cond ((and (is-tag? state)
			(assq state tag-handlers))
		   ((assq state tag-handlers) (car result)))
		  ((is-tag? (car result))
		   edn/omit:)
		  (else (car result)))
	    (cdr result)))))

(define (read-edn)
  1)

;; EDN writing
;; ===========
(define (pair->reader-tag subparser in)
  (string-append "#" (keyword->string (cdr in))))

(define (scm-kw->edn-kw subparser in)
  (string-append ":" (keyword->string in)))

(define (boolean->edn subparser in)
  (case in
    ((#t) "true")
    ((#f) "false")
    (else "nil")))

(define (char->edn subparser in)
  (string #\\ in))

(define (string->edn subparser in)
  (string-append "\"" in "\""))

(define (number->edn subparser in)
  (number->string in))

(define (sequential->edn subparser ld rd in)
  (string-append ld
		 (foldr (lambda (elem init)
			  (string-append (subparser elem)
					 (if (equal? "" init) "" " ")
					 init))
			"" in)
		 rd))

(define (list->edn subparser in)
  (sequential->edn subparser "(" ")" in))

(define (vector->edn subparser in)
  (sequential->edn subparser  "[" "]" (vector->list in)))

(define (map->edn subparser in)
  (string-append "{"
		 (foldr (lambda (elem init)
			  (string-append (subparser (car elem))
					 " "
					 (subparser (cdr elem))
					 (if (equal? "" init) "" " ")
					 init))
			"" in)
		 "}"))

(define (nil->edn subparser in)
  "nil")

(define (symbol->edn subparser in)
  (symbol->string in))

(define (edn-readertag? in)
  (and
   (not (list? in))
   (pair? in)
   (equal? edn/reader-tag: (car in))))

(define (edn-alist? in)
  (and (list? in)
       (any (lambda (item) (and (not (list? item)) (pair? item)))
	    in)))

(define writer-handlers
  (list (cons null? nil->edn)
	(cons string? string->edn)
	(cons char? char->edn)
	(cons boolean? boolean->edn)
	(cons number? number->edn)
	(cons keyword? scm-kw->edn-kw)
	(cons symbol? symbol->edn)
	
	(cons vector? vector->edn)
	(cons edn-alist? map->edn)
	(cons edn-readertag? pair->reader-tag)
	(cons list? list->edn)))

(define (parse-entry in)
  ((cdr
    (find (lambda (item) ((car item) in))
	  writer-handlers))
   parse-entry in))

(define (write-edn struct)
  (display (parse-entry struct) (current-output-port)))
