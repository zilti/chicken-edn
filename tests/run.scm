(require-extension r7rs srfi-69 srfi-78 srfi-88 srfi-1 ports trace)
(include "../edn-impl.scm")

(define s->k string->keyword)

(print "Writing EDN")

(check (parse-entry keyword:) => ":keyword")
(check (parse-entry #t) => "true")
(check (parse-entry #f) => "false")
(check (parse-entry '()) => "nil")
(check (parse-entry #\a) => "\\a")
(check (parse-entry "String") => "\"String\"")
(check (parse-entry (cons edn/reader-tag: neat:)) => "#neat")


(check (list->edn parse-entry '(1 2 3 4)) => "(1 2 3 4)")
(check (vector->edn parse-entry #(a: b: c: d:)) => "[:a :b :c :d]")
(check (parse-entry '((a: . "Hi")
		      (b: . i-am:)
		      (c: . (a list)))) => "{:a \"Hi\" :b :i-am :c (a list)}")

(print "Reading EDN")
(define wifs with-input-from-string)

(check (wifs "(:keyword)" read-edn)  => '(keyword:))
(check (wifs "(123)" read-edn) => '(123))
(check (wifs "(\"Hello World!\")" read-edn) => '("Hello World!"))
(check (wifs "(false)" read-edn) => '(#f))
(check (wifs "(true)" read-edn) => '(#t))
(check (wifs "(:Hello \"World\" 1)" read-edn) => '(Hello: "World" 1))
(check (wifs "[:a :b :c :d]" read-edn) => #(a: b: c: d:))
(check (wifs "{:a \"Hi\" :b :i-am :c (a list)}" read-edn)
       (=> (lambda (a b)
	     (and (equal? (hash-table-ref b a:) "Hi")
		  (equal? (hash-table-ref b b:) i-am:)
		  (equal? (hash-table-ref b c:) `(a list)))))
       (alist->hash-table '((a: . "Hi") (b: . i-am:) (c: . (a list)))))

(quit)
