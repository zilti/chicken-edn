;; EDN Reading
;; ===========

(define edn->atom
  (case-lambda
    ((skip-fn end-fn finalizer) (lambda (subparser input)
				  (edn->atom subparser skip-fn end-fn finalizer '() '() input)))
    ((subparser skip-fn end-fn finalizer result pile input)
     (cond ((or (eq? #!eof (peek-char input))
		(end-fn result pile input))
	    (cons (finalizer (reverse result))
		  (if (or (eq? #!eof input)
			  (char=? #\) (peek-char input))
			  (char=? #\] (peek-char input))
			  (char=? #\} (peek-char input)))
		      input
		      (begin (read-char input) input))))
	   ((skip-fn result pile input)
	    (edn->atom subparser skip-fn end-fn finalizer result (cons (read-char input) pile) input))
	   (else (edn->atom subparser skip-fn end-fn finalizer (cons (peek-char input) result) (cons (peek-char input) pile)
			    (if (null? input) input (begin (read-char input) input))))))))

(define edn->string
  (edn->atom (lambda (result pile input)
	       (or (char=? #\\ (peek-char input))
		   (and (null? result)
			(char=? #\" (peek-char input)))))
	     (lambda (result pile input)
	       (and (char=? #\" (peek-char input))
		    (not (null? pile))
		    (or (not (char=? #\\ (car pile)))
			(char=? #\" (car pile)))))
	     list->string))

(define edn->keyword
  (edn->atom (lambda (result pile input)
	       (char=? #\: (peek-char input)))
	     (lambda (result pile input)
	       (or (char-whitespace? (peek-char input))
		   (char=? #\, (peek-char input))
		   (char=? #\) (peek-char input))
		   (char=? #\] (peek-char input))
		   (char=? #\} (peek-char input))))
	     (lambda (in) (string->keyword (list->string in)))))

(define edn->symbol
  (edn->atom (lambda (result pile input) #f)
	     (lambda (result pile input)
	       (or (char-whitespace? (peek-char input))
		   (char=? #\, (peek-char input))
		   (char=? #\) (peek-char input))
		   (char=? #\] (peek-char input))
		   (char=? #\} (peek-char input))))
	     (lambda (in) (string->symbol (list->string in)))))

(define edn->number
  (edn->atom (lambda (result pile input) #f)
	     (lambda (result pile input)
	       (or (char-whitespace? (peek-char input))
		   (char=? #\M (peek-char input))
		   (char=? #\N (peek-char input))
		   (char=? #\, (peek-char input))
		   (char=? #\) (peek-char input))
		   (char=? #\] (peek-char input))
		   (char=? #\} (peek-char input))))
	     (lambda (in) (string->number (list->string in)))))

(define edn->rtag
  (edn->atom (lambda (result pile input)
	       (char=? #\# (peek-char input)))
	     (lambda (result pile input)
	       (or (char-whitespace? (peek-char input))
		   (char=? #\( (peek-char input))
		   (char=? #\[ (peek-char input))
		   (and (not (null? pile))
			(char=? #\{ (car pile)))))
	     (lambda (in) (cons edn/tag: (string->keyword (list->string in))))))

(define edn->coll
  (case-lambda
    ((ld rd finalize) (lambda (subparser input) (edn->coll subparser ld rd finalize '() input #t)))
    ((subparser ld rd finalize result input fresh?)
     (cond
      ;; End of sequence
      ((or (eq? #!eof (peek-char input))
	   (char=? rd (peek-char input)))
       (cons (finalize (reverse result)) (begin (read-char input) input)))
      ;; First character of sequence
      ((and (char=? ld (peek-char input))
	    fresh?)
       (edn->coll subparser ld rd finalize result (begin (read-char input) input) #f))
      ;; Sub-sequence of same type
      ((char=? ld (peek-char input))
       (let ((sub-result (subparser input)))
	 (edn->coll subparser ld rd finalize (cons (cadr sub-result) result) (caddr sub-result) #f)))
      ;; Stuff in the data!
      (else (let ((compiled (subparser input)))
	      (edn->coll (first compiled)
			 ld rd finalize
			 (if (equal? (second compiled) edn/omit:)
			     result
			     (cons (second compiled) result))
			 (third compiled) #f)))))))

(define edn->list (edn->coll #\( #\) (lambda (x) x)))
(define edn->vector (edn->coll #\[ #\] (lambda (x) (list->vector x))))

(define edn->htable
  (case-lambda
    ((subparser input) (edn->htable subparser (make-hash-table) '() input #t))
    ((subparser result key input fresh?)
     (cond ((or (eq? #!eof (peek-char input))
		(char=? #\} (peek-char input)))
	    (cons result (begin (read-char input) input)))
	   ((and (char=? #\{ (peek-char input))
		 fresh?)
	    (edn->htable subparser result key (begin (read-char input) input) #f))
	   (else (let ((compiled (subparser input)))
		   (cond 
		    ((eq? edn/omit: (second compiled))
		     (edn->htable (first compiled) result key (third compiled) #f))
		    ((null? key)
		     (edn->htable (first compiled) result (second compiled) (third compiled) #f))
		    (else
		     (edn->htable (first compiled) (begin (hash-table-set! result key (second compiled)) result)
				  '()  (third compiled) #f)))))))))

(define (edn->whitespace subparser input)
  (if (char-whitespace? (peek-char input))
      (cons edn/omit: (begin (read-char input) input))
      (cons (read-char input) input)))

(define (is-char? a)
  (lambda (b)
    (and (char? b)
	 (char=? a b))))

(define (is-number? c)
  (or (char-numeric? c)
      (char=? #\+ c)
      (char=? #\- c)))

(define (guard-charcheck fun)
  (lambda (x)
    (and (char? x)
	 (fun x))))

(define tag-handlers
  (list))

(define reader-handlers
  (list (cons (is-char? #\() edn->list)
	(cons (is-char? #\)) edn->list)
	(cons (is-char? #\[) edn->vector)
	(cons (is-char? #\]) edn->vector)
	(cons (is-char? #\{) edn->htable)
	(cons (is-char? #\}) edn->htable)
	(cons (is-char? #\#) edn->rtag)
	(cons (is-char? #\:) edn->keyword)
	(cons (is-char? #\") edn->string)
	(cons (guard-charcheck char-alphabetic?) edn->symbol)
	(cons (guard-charcheck is-number?) edn->number)
	(cons (guard-charcheck char-whitespace?) edn->whitespace)))

(define (is-tag? in)
  (and (not (list? in))
       (pair? in)
       (equal? (car in) edn/tag:)))

(define (parse-edn state)
  (lambda (in-port)
    (let* ((struct-handler (cdr
			    (find (lambda (item) ((car item) (peek-char in-port)))
				  reader-handlers)))
	   (result (struct-handler (parse-edn state) in-port)))
      (list (if (is-tag? (car result))
		(parse-edn result)
		(parse-edn '()))
	    (cond ((and (is-tag? state)
			(assq state tag-handlers))
		   ((assq state tag-handlers) (car result)))
		  ((is-tag? (car result))
		   edn/omit:)
		  (else (car result)))
	    (cdr result)))))

(define (read-edn in-port)
  ((parse-edn '()) in-port))

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

(define (htable->edn subparser in)
  (string-append "{"
		 (hash-table-fold in
				  (lambda (hkey hval folded)
				    (string-append (subparser hkey)
						   " "
						   (subparser hval)
						   (if (equal? "" folded) "" " ")
						   folded))
				  "")
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

(define (edn-htable? in)
  (hash-table? in))

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
	(cons edn-htable? htable->edn)
	(cons edn-readertag? pair->reader-tag)
	(cons list? list->edn)))

(define (parse-entry in)
  ((cdr
    (find (lambda (item) ((car item) in))
	  writer-handlers))
   parse-entry in))

(define (write-edn struct)
  (lambda (out-port)
    (display (parse-entry struct) out-port)))
