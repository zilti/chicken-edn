

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

(define (parse-keyword in)
  (string->keyword (substring in 1 (string-length in))))

(define (parse-number in)
  (string->number
   (cond ((string-suffix? "N" in) (substring in 0 (- (string-length in) 1)))
         ((string-suffix? "M" in) (substring in 0 (- (string-length in) 1)))
         (else in))))

;;(define (construct-list . in))
;;(define (construct-vector . in))
;;(define (construct-map . in))
;;(define (construct-set . in))

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

  ;;(define (read-edn-string string))
  ;;(define (read-edn-file file))
  ;;(define (write-edn-string string))
  ;;(define (write-edn-file file))
  

