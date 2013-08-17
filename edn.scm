(require-extension srfi-13)
(require-extension srfi-1)

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
  (foldr (lambda (init elem)
           (cons elem init))
         (list) in))

(define (construct-vector in)
  (list->vector (construct-list in)))

(define (construct-map in)
  (do [(stuff in)
       (alist (list))]
      [(list-null? stuff) alist]
    (set! alist (cons (cons (caar stuff) (cdar stuff)) alist))
    (set! stuff (cdr stuff))))

(define construct-set construct-list)

(define (apply-tag tag struct))

;; Token list parsing
(define (parse-tokenlist tl)
  (do [(colltype #f)
       (in (append tl #\space))
       (out (list))
       (cache (list))
       (tag #f)
       (token #f)]
      [(list-null? in) (reverse out)]
    (set! token (car in))
    (set! out (cdr out))
    (cond [(not colltype) (case token
                              [(#\() (set! colltype list:)]
                            [(#\[) (set! colltype vector:)]
                            [(#\{) (cond [(= colltype pre-set:) (set! colltype set:)]
                                         [else (set! colltype map:)])]
                            [(#\#) (cond [(char=? #\{ (car in)) (set! colltype pre-set:)]
                                         [else (error "Invalid EDN.")])]
                            [else (error "Invalid EDN. Data outside a collection form.")])]
          [else (case token
                  [(#\( #\[ #\{) ;; Use counter to count opening and closing braces; Extract sub-element and recurse.
                   ]
                  [(#\)) (begin (set! out (cons (construct-list (reverse cache)) out))
                                (set! cache (list))
                                (set! colltype #f))]
                  [(#\]) (begin (set! out (cons (construct-vector (reverse cache)) out))
                                (set! cache (list))
                                (set! colltype #f))]
                  [(#\}) (begin (set! out (cons (cond [(= colltype set:) (construct-list (reverse cache))]
                                                      [else (construct-map (reverse-cache))]) out))
                                (set! cache (list))
                                (set! colltype #f))]
                  [else (cond [(list? token) (cond [(eq? (string->symbol "#edn/read") (car token)) (begin (set! tag (cdr token)))]
                                                   [else (error "Invalid EDN.")])]
                              [else (set! out (cons token out))])])])))

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

(define (tokenize in)
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
                              (set! out (cons (string->symbol "#edn/read") (reverse-list->string cache) out))
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

  ;;(define (read-edn-string string))
  ;;(define (read-edn-file file))
  ;;(define (write-edn-string string))
  ;;(define (write-edn-file file))
  

