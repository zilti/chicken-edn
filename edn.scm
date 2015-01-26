(module edn
    (
     ;;edn-read-file edn-read-string edn-tokenize edn-parse-tokenlist
     ;;edn-register-handler edn-remove-handler
		   ;; writing functions
		   scm-kw->edn-kw boolean->edn char->edn string->edn
		   list->edn vector->edn
		   parse-entry write-edn
		   )

  (import chicken scheme)
  (require-extension r7rs srfi-1)

  ;; ;; EDN Reading
  ;; (define handlers (list))
  ;; (define empty-delimiters (list #\, #\space))
  ;; (define list-delimiters (list #\( #\)))
  ;; (define vector-delimiters (list #\[ #\]))
  ;; (define map-delimiters (list #\{ #\}))
  ;; (define seq-delimiters (append list-delimiters vector-delimiters map-delimiters (list)))

  ;; (define (edn-register-handler tag fn)
  ;;   (set! handlers (alist-update (string->keyword tag) fn handlers)))

  ;; (define (edn-remove-handler tag)
  ;;   (set! handlers (foldr (lambda (e l) (cond [(equal? (car e) (string->keyword tag)) l]
  ;;                                             (else (cons e l))))
  ;;                         (list) handlers)))

  ;; ;; Intern tags
  ;; (edn-register-handler "#inst" (lambda (in) in))
  ;; (edn-register-handler "#uuid" (lambda (in) in))
  
  ;; ;; Collection construction
  ;; (define (construct-list in)
  ;;   (foldr (lambda (elem init)
  ;;            (cons elem init))
  ;;          (list) in))

  ;; (define (construct-vector in)
  ;;   (list->vector (construct-list in)))

  ;; (define (construct-map in)
  ;;   (do [(stuff in)
  ;;        (alist (list))]
  ;;       [(> 2 (length stuff)) alist]
  ;;     (set! alist (alist-update (car stuff) (cadr stuff) alist))
  ;;     (set! stuff (cddr stuff))))

  ;; (define (construct-set in)
  ;;   (fold (lambda (elem init)
  ;; 	     (set-add! elem init) init)
  ;; 	   (set equal? (car in)) (cdr in)))

  ;; (define (apply-tag tag struct)
  ;;   (let [(fn (alist-ref tag handlers))]
  ;;     (case fn [(#f) struct]
  ;;           [else (fn struct)])))

  ;; ;; Token list parsing
  ;; (define (edn-parse-tokenlist tl)
  ;;   (do [(colltype #f)
  ;;        (in (reverse (cons #\space (reverse tl))))
  ;;        (out (list))
  ;;        (cache (list))
  ;;        (tag #f)
  ;;        (omit (string->keyword "#_"))
  ;;        (token #f)]
  ;;       [(null-list? in) (cond [(null-list? out) (reverse out)]
  ;;                              [else (car (reverse out))])]
  ;;     (set! token (car in))
  ;;     (set! in (cdr in))
  ;;     (case colltype
  ;;       [(#f pre-set:)
  ;;        (set! colltype
  ;; 	   (case token
  ;; 	     [(#\() list:]
  ;; 	     [(#\[) vector:]
  ;; 	     [(#\{) (cond [(eq? colltype pre-set:) set:]
  ;; 			  [else map:])]
  ;; 	     [(#\#) (cond [(char=? #\{ (car in)) pre-set:]
  ;; 			  [else (error "Invalid EDN. # outside a collection form used not for starting a set.")])]
  ;; 	     [else (cond [(char-whitespace? token)]
  ;; 			 [else (error (string-append "Invalid EDN. Data outside a collection form: " (->string token) " (colltype: " (->string colltype) ")\n"))])]))]
  ;;       [else (case token
  ;;               [(#\( #\[ #\{) (cond [(eq? tag omit) (set! tag #f)]
  ;;                                    [else (begin
  ;;                                            (set! cache (cons
  ;;                                                         (apply-tag tag
  ;;                                                                    (edn-parse-tokenlist
  ;;                                                                     (do [(sublist (list))
  ;;                                                                          (inp (cons token in))
  ;;                                                                          (fst #t)
  ;;                                                                          (counter '((#\( . 0) (#\[ . 0) (#\{ . 0)))]
  ;;                                                                         [(and (not fst)
  ;;                                                                               (eq? 0 (alist-ref #\( counter))
  ;;                                                                               (eq? 0 (alist-ref #\[ counter))
  ;;                                                                               (eq? 0 (alist-ref #\{ counter)))
  ;;                                                                          (set! in inp)
  ;;                                                                          (reverse sublist)]
  ;;                                                                       (set! fst #f)
  ;;                                                                       (case (car inp)
  ;;                                                                         [(#\() (set! counter (alist-update #\( (+ (alist-ref #\( counter) 1) counter))]
  ;;                                                                         [(#\[) (set! counter (alist-update #\[ (+ (alist-ref #\[ counter) 1) counter))]
  ;;                                                                         [(#\{) (set! counter (alist-update #\{ (+ (alist-ref #\{ counter) 1) counter))]
  ;;                                                                         [(#\)) (set! counter (alist-update #\( (- (alist-ref #\( counter) 1) counter))]
  ;;                                                                         [(#\]) (set! counter (alist-update #\[ (- (alist-ref #\[ counter) 1) counter))]
  ;;                                                                         [(#\}) (set! counter (alist-update #\{ (- (alist-ref #\{ counter) 1) counter))])
  ;;                                                                       (set! sublist (cons (car inp) sublist))
  ;;                                                                       (set! inp (cdr inp)))))
  ;;                                                         cache))
  ;;                                            (set! tag #f))])]
  ;;               [(#\) #\] #\})
  ;;                (begin (cond [(eq? tag omit) (set! tag #f)]
  ;;                             [else (begin
  ;;                                     (set! out (cons (apply-tag tag (case token
  ;;                                                                      [(#\)) (construct-list (reverse cache))]
  ;;                                                                      [(#\]) (construct-vector (reverse cache))]
  ;;                                                                      [(#\}) (cond [(eq? colltype set:) (construct-set (reverse cache))]
  ;;                                                                                   [else (construct-map (reverse cache))])])) out))
  ;;                                     (set! tag #f))])
  ;;                       (set! cache (list))
  ;;                       (set! colltype #f))]
  ;;               [else (cond [(pair? token)
  ;;                            (cond [(eq? edn/reader-tag: (car token)) (set! tag (cdr token))]
  ;;                                  [else (error (string-append "Invalid EDN: " (->string token) " in coll " (->string colltype)))])]
  ;;                           [(and (char? token) (char-whitespace? token))]
  ;;                           [else (cond [(eq? tag omit) (set! tag #f)]
  ;;                                       [else (begin
  ;;                                               (set! cache (cons (apply-tag tag token) cache))
  ;;                                               (set! tag #f))])])])])))

  ;; ;; Tokenizing and helper functions
  ;; (define (parse-keyword in)
  ;;   (string->keyword (substring in 1 (string-length in))))

  ;; (define (parse-number in)
  ;;   (string->number
  ;;    (let* [(num (cond [(string-suffix? "N" in) (substring in 0 (- (string-length in) 1))]
  ;;                      [(string-suffix? "M" in) (substring in 0 (- (string-length in) 1))]
  ;;                      [else in]))]
  ;;      (cond [(string-prefix? "." num) (string-append "0" num)]
  ;;            [(string-prefix? "+" num) (substring num 1 (string-length num))]
  ;;            [else num]))))

  ;; (define (char-contains? c chars)
  ;;   (let [(c? #f)]
  ;;     (map (lambda (in) (cond ([char=? c in]
  ;;                              (set! c? #t) in)
  ;;                             (else in)))
  ;;          chars)
  ;;     c?))
  
  ;; (define (edn-tokenize in)
  ;;   (do [(cur-seq #f)
  ;;        (cache (list))
  ;;        (out (list))
  ;;        (next #f)
  ;;        (cc #f)
  ;;        (in (string->list (string-append in " ")))]
  ;;       [(null-list? in)
  ;;        (reverse out)]
  ;;     (set! next (car in))
  ;;     (set! in (cdr in))
  ;;     (set! cur-seq (call/cc (lambda (cont)
  ;;                              (set! cc cont)
  ;;                              cur-seq)))
  ;;     (case cur-seq
  ;;       ;; Sequence filling / termination
  ;;       [(end-seq:) (set! cur-seq #f)]
  ;;       [(string:) (cond [(and (char=? next #\") (not (char=? #\\ (car cache))))
  ;;                         (begin (set! cur-seq end-seq:)
  ;;                                (set! out (cons (reverse-list->string cache) out))
  ;;                                (set! cache (list)))]
  ;;                        ;; Properly insert escaped "
  ;;                        [(and (char=? next #\") (char=? #\\ (car cache)))
  ;;                         (set! cache (cons #\" (cdr cache)))]
  ;;                        [else (set! cache (cons next cache))])]

  ;;       [(char:) (cond [(or (char-contains? next (append seq-delimiters (list #\space))) (char-whitespace? next))
  ;;                       (begin (set! cur-seq #f)
  ;;                              (let [(ch (cond [(> (length cache) 1)
  ;;                                               (case (reverse-list->string cache)
  ;;                                                 [("newline") #\newline]
  ;;                                                 [("return") #\return]
  ;;                                                 [("space") #\space]
  ;;                                                 [("tab") #\tab]
  ;;                                                 [else (error (string-append "Unrecognized character: " (reverse-list->string cache)))])]
  ;;                                              [else (car cache)]))]
  ;;                                (set! out (cons ch out)))
  ;;                              (set! cache (list)))]
  ;;                      [else (set! cache (cons next cache))])]
        
  ;;       [(reader-tag:) (cond
  ;;                       [(char-contains? next (append seq-delimiters (list #\space #\,)))
  ;;                        (begin (set! cur-seq #f)
  ;;                               (set! out (cons (cons edn/reader-tag:
  ;; 						      (let ((cstring (reverse-list->string cache)))
  ;; 							(string->keyword (substring cstring 1 (string-length cstring))))) out))
  ;;                               (set! cache (list)))]
  ;;                       [else (cond [(char=? next #\_)
  ;;                                    (begin (set! cache (cons next cache))
  ;;                                           (set! out (cons (cons edn/reader-tag:
  ;; 								  (let ((cstring (reverse-list->string cache)))
  ;; 								    (string->keyword (substring cstring 1 (string-length cstring))))) out))
  ;;                                           (set! cache (list))
  ;;                                           (set! cur-seq end-seq:))]
  ;;                                   [else (set! cache (cons next cache))])])]
        
  ;;       [(symbol:) (cond
  ;;                   [(char-contains? next (append seq-delimiters (list #\space #\,)))
  ;;                    (begin (set! cur-seq #f)
  ;;                           (set! out (cons (let [(x (reverse-list->string cache))]
  ;;                                             (cond 
  ;;                                              [(equal? x "true") #t]
  ;;                                              [(equal? x "false") #f]
  ;;                                              [(equal? x "nil") #f]
  ;;                                              [else (string->symbol x)])) out))
  ;;                           (set! cache (list)))]
  ;;                   [else (set! cache (cons next cache))])]
        
  ;;       [(keyword:) (cond
  ;;                    [(char-contains? next (append seq-delimiters (list #\space #\" #\,)))
  ;;                     (begin (set! cur-seq #f)
  ;;                            (set! out (cons (parse-keyword (reverse-list->string cache)) out))
  ;;                            (set! cache (list)))]
  ;;                    [else (set! cache (cons next cache))])]
        
  ;;       [(number:) (cond
  ;;                   [(char-contains? next (append seq-delimiters (list #\space #\" #\,)))
  ;;                    (begin (set! cur-seq #f)
  ;;                           (set! out (cons (parse-number (reverse-list->string cache)) out))
  ;;                           (set! cache (list)))]
  ;;                   [else (set! cache (cons next cache))])])
  ;;     ;; Check for collection delimiters
  ;;     (cond [(and (char-contains? next seq-delimiters) (equal? #f cur-seq))
  ;;            (set! out (cons next out))]
  ;;           ;; Check for mode set
  ;;           [else (cond [(equal? #f cur-seq)
  ;;                        (cond [(char=? next #\") (set! cur-seq string:)]
  ;;                              [(char=? next #\:) (cc keyword:)]
  ;;                              [(char=? next #\#) (cc reader-tag:)]
  ;;                              [(char-numeric? next) (cc number:)]
  ;;                              [(char-contains? next empty-delimiters) #f]
  ;;                              [(char=? next #\\) (set! cur-seq char:)]
  ;;                              [(and (char-contains? next (list #\+ #\- #\.))
  ;;                                    (char-numeric? (car in))) (cc number:)]
  ;;                              [else (cc symbol:)])])])))

  ;; (define (edn-read-string string)
  ;;   (do [(in (string->list (string-delete #\newline string)))
  ;;        (fst #t)
  ;;        (cache "")
  ;;        (counter '((#\( . 0) (#\[ . 0) (#\{ . 0)))
  ;;        (out (list))]
  ;;       [(null-list? in) (case (length out)
  ;;                          [(1) (car (reverse out))]
  ;;                          [else (reverse out)])]
  ;;     (case (car in)
  ;;       [(#\( #\[ #\{) (set! counter (alist-update (car in) (+ (alist-ref (car in) counter) 1) counter))]
  ;;       [(#\)) (set! counter (alist-update #\( (- (alist-ref #\( counter) 1) counter))]
  ;;       [(#\]) (set! counter (alist-update #\[ (- (alist-ref #\[ counter) 1) counter))]
  ;;       [(#\}) (set! counter (alist-update #\{ (- (alist-ref #\{ counter) 1) counter))])
  ;;     (set! cache (string-append cache (->string (car in))))
  ;;     (case fst
  ;;       [(#t) (set! fst #f)]
  ;;       [else (cond [(and (= 0 (alist-ref #\( counter))
  ;;                         (= 0 (alist-ref #\[ counter))
  ;;                         (= 0 (alist-ref #\{ counter))
  ;;                         (not (char-whitespace? (car in))))
  ;;                    (begin (set! out (cons (edn-parse-tokenlist (edn-tokenize cache)) out))
  ;;                           (set! cache ""))])])
  ;;     (set! in (cdr in))))
  
  ;; (define (edn-read-file file)
  ;;   (let [(res (read-token (lambda (in) #t)
  ;;                          (open-input-file* (file-open file open/rdonly))))]
  ;;     (edn-read-string res)))

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
    (display struct (current-output-port)))

)
