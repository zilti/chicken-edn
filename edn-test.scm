(load "edn.scm")
(import edn)
(use missbehave missbehave-matchers missbehave-stubs miscmacros)

(describe "edn-tokenizer tests"
          (it "Number conversion" (expect (edn-tokenize "1234") (be '(1234))))
          (it "Keyword conversion" (expect (edn-tokenize ":abc") (be '(abc:))))
          (it "String conversion" (expect (edn-tokenize "\"Hello World!\"") (be '("Hello World!"))))
          (it "List conversion" (expect (edn-tokenize "(inc 5)") (be '(#\( inc 5 #\)))))
          (it "Reader tag conversion" (expect (edn-tokenize "#asdf/rdr") (be '((edn/reader-tag: . "#asdf/rdr")))))
          (it "Escaped \" characters" (expect (edn-tokenize "\"Does \\\"this\\\" work?\"") (be '("Does \"this\" work?"))))
          (it "Combined" (expect (edn-tokenize "(1234 {:Hi \"I'm a map.\" :confuse \"(1234)\"})") (be '(#\( 1234 #\{ Hi: "I'm a map." confuse: "(1234)" #\} #\))))))

(describe "edn-parse-tokenlist tests"
          (it "List conversion" (expect (edn-parse-tokenlist '(#\( 1 2 "3" "4" #\))) (be '(1 2 "3" "4"))))
          (it "Vector conversion" (expect (edn-parse-tokenlist '(#\[ 1 2 "3" "4" #\])) (be (list->vector '(1 2 "3" "4")))))
          (it "Set conversion" (expect (edn-parse-tokenlist '(#\# #\{ 1 2 "3" "4" #\})) (be '(1 2 "3" "4"))))
          (it "Map conversion" (expect (edn-parse-tokenlist '(#\{ 1 2 "3" "4" #\})) (be '((1 . 2) ("3" . "4")))))
          (it "Nested list" (expect (edn-parse-tokenlist '(#\( 1 2 #\( 3 4 #\( 5 6 #\) 7 #\) 8 #\))) (be '(1 2 (3 4 (5 6) 7) 8)))))
