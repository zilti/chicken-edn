(require-extension srfi-13)
(require-extension srfi-1)

(define handlers '())
(define empty-delimiters (list #\,))
(define list-delimiters (list #\( #\)))
(define vector-delimiters (list #\[ #\]))
(define map-delimiters (list #\{ #\}))
(define seq-delimiters (append list-delimiters vector-delimiters map-delimiters (list)))

;;(define (register-handler tag fn))
;;(define (remove-handler tag))

(define (parse-keyword in)
  (string->keyword (substring in 1 (string-length in))))

(define (parse-character in)
  (string-concatenate (list "#" in)))

(define (parse-number in)
  (string->number
   (cond ((string-suffix? "N" in) (substring in 0 (- (string-length in) 1)))
         ((string-suffix? "M" in) (substring in 0 (- (string-length in) 1)))
         (else in))))

;;(define (construct-list . in))
;;(define (construct-vector . in))
;;(define (construct-map . in))
;;(define (construct-set . in))

(define (call/cc=< fn . args)
  (call/cc (lambda (cc)
             (apply fn (cons cc args)))))

(define (char-contains? cc c chars)
  (map (lambda (in) (cond ((char=? c in) (cc #t))
			  (else in))) chars)
  (cc #f))

(define (tokenize in)
  (do [(in-string? #f)
       (in-number? #f)
       (in-keyword? #f)
       (string-cache (list))
       (digit-cache (list))
       (keyword-cache (list))
       (out (list))
       (next #f)
       (in (string->list (string-append in " ")))]
      ((null-list? in)
       (reverse out))
    (set! next (car in))
    (set! in (cdr in))
    (cond
     ;; Sequence filling / termination
     (in-string? (cond ((char=? next #\") (begin (set! in-string? #f)
                                            (set! out (cons (reverse-list->string string-cache) out))
                                            (set! string-cache (list))))
                       (else (set! string-cache (cons next string-cache)))))
     (in-keyword? (cond
		   ((call/cc=< char-contains? next (append seq-delimiters (list #\space #\" #\,)))
		    (begin (set! in-keyword? #f)
			   (set! out (cons (parse-keyword (reverse-list->string keyword-cache)) out))
			   (set! keyword-cache (list))
			   (cond ((call/cc=< char-contains? next seq-delimiters) (set! out (cons next out))))))
		   (else (set! keyword-cache (cons next keyword-cache)))))
     
     (in-number? (cond
		  ((call/cc=< char-contains? next (append seq-delimiters (list #\space #\" #\,)))
		   (begin (set! in-number? #f)
			  (set! out (cons (parse-number (reverse-list->string digit-cache)) out))
			  (set! digit-cache (list))
			  (cond ((call/cc=< char-contains? next seq-delimiters) (set! out (cons next out))))))
		  (else (set! digit-cache (cons next digit-cache)))))
     ;; Sequence start detection
     ((char=? next #\") (set! in-string? #t))
     ((char-numeric? next) (begin (set! in-number? #t)
                                  (set! digit-cache (cons next digit-cache))))
     ((char=? next #\:) (begin (set! in-keyword? #t)
                            (set! keyword-cache (cons next keyword-cache))))
     ((char-whitespace? next) #f)
     (else (set! out (cons next out))))))

;;(define (read-edn-string string))
;;(define (read-edn-file file))
;;(define (write-edn-string string))
;;(define (write-edn-file file))

