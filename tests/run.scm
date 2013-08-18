(include "../edn.scm")
(import edn)
(require-extension test)

(test-group "edn-tokenizer"
            (test "Number conversion" '(1234) (edn-tokenize "1234"))
            (test "Keyword conversion" '(abc:) (edn-tokenize ":abc"))
            (test "String conversion" '("Hello World!") (edn-tokenize "\"Hello World!\""))
            (test "List conversion" '(#\( inc 5 #\)) (edn-tokenize "(inc 5)"))
            (test "Reader tag conversion" '((edn/reader-tag: . "#asdf/rdr")) (edn-tokenize "#asdf/rdr"))
            (test "Escaped \" characters" '("Does \"this\" work?") (edn-tokenize "\"Does \\\"this\\\" work?\""))
            (test "Combined" '(#\( 1234 #\{ Hi: "I'm a map." confuse: "(1234)" #\} #\)) (edn-tokenize "(1234 {:Hi \"I'm a map.\" :confuse \"(1234)\"})")))

(test-group "edn-parse-tokenlist"
            (test "List conversion" '(1 2 "3" "4") (edn-parse-tokenlist '(#\( 1 2 "3" "4" #\))))
            (test "Vector conversion" (list->vector '(1 2 "3" "4")) (edn-parse-tokenlist '(#\[ 1 2 "3" "4" #\])))
            (test "Set conversion" '(1 2 "3" "4") (edn-parse-tokenlist '(#\# #\{ 1 2 "3" "4" #\})))
            (test "Map conversion" '((1 . 2) ("3" . "4")) (edn-parse-tokenlist '(#\{ 1 2 "3" "4" #\})))
            (test "Nested lists" '(1 2 (3 4 (5 6) 7) 8) (edn-parse-tokenlist '(#\( 1 2 #\( 3 4 #\( 5 6 #\) 7 #\) 8 #\)))))

(test-exit)

