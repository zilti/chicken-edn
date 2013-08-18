(module edn
  (
   edn-read-file edn-read-string edn-tokenize edn-parse-tokenlist
  )

  (import chicken scheme)
  (require-extension srfi-1 srfi-13 srfi-69 extras data-structures posix)

  ;; EDN Reading
  (define handlers (list))
  (define empty-delimiters (list #\, #\space))
  (define list-delimiters (list #\( #\)))
  (define vector-delimiters (list #\[ #\]))
  (define map-delimiters (list #\{ #\}))
  (define seq-delimiters (append list-delimiters vector-delimiters map-delimiters (list)))

  (define (edn-register-handler tag fn)
    (set! handlers (cons (cons tag fn) handlers)))

  (define (edn-remove-handler tag)
    (set! handlers (foldr (lambda (e l) (cond [(equal? (car e) tag) l]
                                              (else (cons e l))))
                          (list) handlers)))

  ;; Collection construction
  (define (construct-list in)
    (foldr (lambda (elem init)
             (cons elem init))
           (list) in))

  (define (construct-vector in)
    (list->vector (construct-list in)))

  (define (construct-map in)
    (do [(stuff in)
         (alist (list))]
        [(> 2 (length stuff)) alist]
      (set! alist (alist-update (car stuff) (cadr stuff) alist))
      (set! stuff (cddr stuff))))

  (define construct-set construct-list)

  ;;(define (apply-tag tag struct))

  ;; Token list parsing
  (define (edn-parse-tokenlist tl)
    (do [(colltype #f)
         (in (reverse (cons #\space (reverse tl))))
         (out (list))
         (cache (list))
         (tag #f)
         (token #f)]
        [(null-list? in) (car (reverse out))]
      (set! token (car in))
      (set! in (cdr in))
      (case colltype
        [(#f pre-set:)
         (set! colltype
               (case token
                 [(#\() list:]
                 [(#\[) vector:]
                 [(#\{) (cond [(eq? colltype pre-set:) set:]
                              [else map:])]
                 [(#\#) (cond [(char=? #\{ (car in)) pre-set:]
                              [else (error "Invalid EDN.")])]
                 [else (cond [(char-whitespace? token)]
                             [else (error (string-append "Invalid EDN. Data outside a collection form: " (->string token) " (colltype: " (->string colltype) ")\n"))])]))]
        [else (case token
                [(#\( #\[ #\{) (set! cache (cons 
                                            (edn-parse-tokenlist
                                             (do [(sublist (list))
                                                  (inp (cons token in))
                                                  (fst #t)
                                                  (counter '((#\( . 0) (#\[ . 0) (#\{ . 0)))]
                                                 [(and (not fst)
                                                       (eq? 0 (alist-ref #\( counter))
                                                       (eq? 0 (alist-ref #\[ counter))
                                                       (eq? 0 (alist-ref #\{ counter)))
                                                  (set! in inp)
                                                  (reverse sublist)]
                                               (set! fst #f)
                                               (case (car inp)
                                                 [(#\() (set! counter (alist-update #\( (+ 1 (alist-ref #\( counter)) counter))]
                                                 [(#\[) (set! counter (alist-update #\[ (+ 1 (alist-ref #\[ counter)) counter))]
                                                 [(#\{) (set! counter (alist-update #\{ (+ 1 (alist-ref #\{ counter)) counter))]
                                                 [(#\)) (set! counter (alist-update #\( (- (alist-ref #\( counter) 1) counter))]
                                                 [(#\]) (set! counter (alist-update #\[ (- (alist-ref #\[ counter) 1) counter))]
                                                 [(#\}) (set! counter (alist-update #\{ (- (alist-ref #\{ counter) 1) counter))])
                                               (set! sublist (cons (car inp) sublist))
                                               (set! inp (cdr inp))))
                                            cache))]
                [(#\)) (begin (set! out (cons (construct-list (reverse cache)) out))
                              (set! cache (list))
                              (set! colltype #f))]
                [(#\]) (begin (set! out (cons (construct-vector (reverse cache)) out))
                              (set! cache (list))
                              (set! colltype #f))]
                [(#\}) (begin (set! out (cons (cond [(eq? colltype set:) (construct-list (reverse cache))]
                                                    [else (construct-map (reverse cache))]) out))
                              (set! cache (list))
                              (set! colltype #f))]
                [else (cond [(list? token) (cond [(eq? edn/reader-tag: (car token)) (begin (set! tag (cdr token)))]
                                                 [else (error "Invalid EDN.")])]
                            [(and (char? token) (char-whitespace? token))]
                            [else (set! cache (cons token cache))])])])))

  ;; Tokenizing and helper functions
  (define (parse-keyword in)
    (string->keyword (substring in 1 (string-length in))))

  (define (parse-number in)
    (string->number
     (cond ((string-suffix? "N" in) (substring in 0 (- (string-length in) 1)))
           ((string-suffix? "M" in) (substring in 0 (- (string-length in) 1)))
           (else in))))

  (define (char-contains? c chars)
    (let [(c? #f)]
      (map (lambda (in) (cond ([char=? c in]
                               (set! c? #t) in)
                              (else in)))
           chars)
      c?))

  (define (edn-tokenize in)
    (do [(cur-seq #f)
         (cache (list))
         (out (list))
         (next #f)
         (cc #f)
         (in (string->list (string-append in " ")))]
        ((null-list? in)
         (reverse out))
      (set! next (car in))
      (set! in (cdr in))
      (set! cur-seq (call/cc (lambda (cont)
                               (set! cc cont)
                               cur-seq)))
      (case cur-seq
        ;; Sequence filling / termination
        ((end-string:) (set! cur-seq #f))
        ((string:) (cond [(and (char=? next #\") (not (char=? #\\ (car cache))))
                          (begin (set! cur-seq end-string:)
                                 (set! out (cons (reverse-list->string cache) out))
                                 (set! cache (list)))]
                         ;; Properly insert escaped "
                         [(and (char=? next #\") (char=? #\\ (car cache)))
                          (set! cache (cons #\" (cdr cache)))]
                         [else (set! cache (cons next cache))]))

        ((reader-tag:) (cond
                        [(char-contains? next (append seq-delimiters (list #\space #\,)))
                         (begin (set! cur-seq #f)
                                (set! out (cons (cons edn/reader-tag: (reverse-list->string cache)) out))
                                (set! cache (list)))]
                        [else (set! cache (cons next cache))]))
        
        ((symbol:) (cond
                    [(char-contains? next (append seq-delimiters (list #\space #\,)))
                     (begin (set! cur-seq #f)
                            (set! out (cons (string->symbol (reverse-list->string cache)) out))
                            (set! cache (list)))]
                    [else (set! cache (cons next cache))]))
        
        ((keyword:) (cond
                     [(char-contains? next (append seq-delimiters (list #\space #\" #\,)))
                      (begin (set! cur-seq #f)
                             (set! out (cons (parse-keyword (reverse-list->string cache)) out))
                             (set! cache (list)))]
                     [else (set! cache (cons next cache))]))
        
        ((number:) (cond
                    [(char-contains? next (append seq-delimiters (list #\space #\" #\,)))
                     (begin (set! cur-seq #f)
                            (set! out (cons (parse-number (reverse-list->string cache)) out))
                            (set! cache (list)))]
                    [else (set! cache (cons next cache))])))
      ;; Check for collection delimiters
      (cond [(and (char-contains? next seq-delimiters) (equal? #f cur-seq))
             (set! out (cons next out))]
            ;; Check for mode set
            [else (cond [(equal? #f cur-seq)
                         (cond [(char=? next #\") (set! cur-seq string:)]
                               [(char=? next #\:) (cc keyword:)]
                               [(char=? next #\#) (cc reader-tag:)]
                               [(char-numeric? next) (cc number:)]
                               [(char-contains? next empty-delimiters) #f]
                               [else (cc symbol:)])])])))

  (define (edn-read-string string)
    (do [(in (string->list (string-delete #\newline string)))
         (fst #t)
         (cache "")
         (counter '((#\( . 0) (#\[ . 0) (#\{ . 0)))
         (out (list))]
        [(null-list? in) (case (length out)
                           [(1) (car (reverse out))]
                           [else (reverse out)])]
      (case (car in)
        [(#\( #\[ #\{) (set! counter (alist-update (car in) (+ (alist-ref (car in) counter) 1) counter))]
        [(#\)) (set! counter (alist-update #\( (- (alist-ref #\( counter) 1) counter))]
        [(#\]) (set! counter (alist-update #\[ (- (alist-ref #\[ counter) 1) counter))]
        [(#\}) (set! counter (alist-update #\{ (- (alist-ref #\{ counter) 1) counter))])
      (set! cache (string-append cache (->string (car in))))
      (case fst
        [(#t) (set! fst #f)]
        [else (cond [(and (= 0 (alist-ref #\( counter))
                          (= 0 (alist-ref #\[ counter))
                          (= 0 (alist-ref #\{ counter))
                          (not (char-whitespace? (car in))))
                     (begin (set! out (cons (edn-parse-tokenlist (edn-tokenize cache)) out))
                            (set! cache ""))])])
      (set! in (cdr in))))
  
  (define (edn-read-file file)
    (let [(res (read-token (lambda (in) #t)
                           (open-input-file* (file-open file open/rdonly))))]
      (edn-read-string res)))
  ;;(define (edn-write-string string))
  ;;(define (edn-write-file file))  
)
