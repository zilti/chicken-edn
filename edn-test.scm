(use missbehave missbehave-matchers missbehave-stubs miscmacros)
(load "edn.scm")
(define tokenizer-challenge
  '(("1234" . (1234))
    (":abc" . (abc:))
    ("\"Hello World!\"" . ("Hello World!"))
    ("(inc 5)" . (#\( inc 5 #\)))
    ("#asdf/rdr" . ((edn/reader-tag: . "#asdf/rdr")))
    ("\"Does \\\"this\\\" work?\"" . ("Does \"this\" work?"))
    ("(1234 {:Hi \"I'm a map.\" :confuse \"(1234)\"})" . (#\( 1234 #\{ Hi: "I'm a map." confuse: "(1234)" #\} #\)))))

(describe "tokenizer tests"
          (for-each (lambda (entry)
                      (it "" (expect (tokenize (car entry)) (to (be (cdr entry))))))
                    tokenizer-challenge))
