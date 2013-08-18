(include "../edn.scm")
(import edn)
(require-extension test)

(edn-register-handler "#test" (lambda (in) (cons '+ in)))
(define s->k string->keyword)

(test-group "edn-tokenize"
            (test "Number conversion" '(1234) (edn-tokenize "1234"))
            (test "Number suffix conversion" '(1234 12.34) (edn-tokenize "1234N 12.34M"))
            (test "Number prefix conversion" '(123 -123 0.123) (edn-tokenize "+123 -123 .123"))
            (test "Keyword conversion" '(abc:) (edn-tokenize ":abc"))
            (test "Character conversion" '(#\a #\b) (edn-tokenize "\\a \\b"))
            (test "String conversion" '("Hello World!") (edn-tokenize "\"Hello World!\""))
            (test "Boolean conversion" '(#t #f) (edn-tokenize "true false"))
            (test "Discard tag recognition" `((edn/reader-tag: . ,(s->k "#_")) 5) (edn-tokenize "#_5"))
            (test "List conversion" '(#\( inc 5 #\)) (edn-tokenize "(inc 5)"))
            (test "Reader tag conversion" `((edn/reader-tag: . ,(s->k "#asdf/rdr"))) (edn-tokenize "#asdf/rdr"))
            (test "Escaped \" characters" '("Does \"this\" work?") (edn-tokenize "\"Does \\\"this\\\" work?\""))
            (test "Combined" '(#\( 1234 #\{ Hi: "I'm a map." confuse: "(1234)" #\} #\)) (edn-tokenize "(1234 {:Hi \"I'm a map.\" :confuse \"(1234)\"})")))

(test-group "edn-parse-tokenlist"
            (test "List conversion" '(1 2 "3" "4") (edn-parse-tokenlist '(#\( 1 2 "3" "4" #\))))
            (test "Vector conversion" (list->vector '(1 2 "3" "4")) (edn-parse-tokenlist '(#\[ 1 2 "3" "4" #\])))
            (test "Set conversion" '(1 2 "3" "4") (edn-parse-tokenlist '(#\# #\{ 1 2 "3" "4" #\})))
            (test "Map conversion" '((1 . 2) ("3" . "4")) (edn-parse-tokenlist '(#\{ 1 2 "3" "4" #\})))
            (test "Char handling" '(#\a #\b) (edn-parse-tokenlist '(#\( #\a #\b #\))))
            (test "Boolean handling" '(#t #f) (edn-parse-tokenlist '(#\( #t #f #\))))
            (test "Nested lists" '(1 2 (3 4 (5 6) 7) 8) (edn-parse-tokenlist '(#\( 1 2 #\( 3 4 #\( 5 6 #\) 7 #\) 8 #\))))
            (test "Tag on a list" '(1 (+ 2 3 4)) (edn-parse-tokenlist `(#\( 1 (edn/reader-tag: . ,(s->k "#test")) #\( 2 3 4 #\)#\))))
            (test "\"Omit\" (#_) tag" '(1 3 4) (edn-parse-tokenlist `(#\( 1 (edn/reader-tag: . ,(s->k "#_")) 2 3 4 #\))))
            (test "Omit only entry of a list" '(1 2 () 4) (edn-parse-tokenlist `(#\( 1 2 #\((edn/reader-tag: . ,(s->k "#_")) 3 #\) 4 #\))))
            (test "#inst built-in tag" '("1985-04-12T23:20:50.52Z") (edn-parse-tokenlist `(#\((edn/reader-tag: . ,(s->k "#inst")) "1985-04-12T23:20:50.52Z" #\))))
            (test "#uuid built-in tag" '("f81d4fae-7dec-11d0-a765-00a0c91e6bf6") (edn-parse-tokenlist `(#\((edn/reader-tag: . ,(s->k "#uuid")) "f81d4fae-7dec-11d0-a765-00a0c91e6bf6" #\)))))

(test-group "edn-read-string"
            (test "If only one entry, must not return a nested list" '(1 2 3) (edn-read-string "(1 2 3)"))
            (test "Two lists" '((1 2 3)(4 5 6)) (edn-read-string "(1 2 3)(4 5 6)"))
            (test "Two lists, spaced" '((1 2 3) (4 5 6)) (edn-read-string "(1 2 3) (4 5 6)")))
(test-exit)

