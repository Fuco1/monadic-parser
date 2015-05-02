;; -*- lexical-binding: t -*-

(defun mp-test-run-string-parser (parser)
  (-lambda ((input . expected))
    (-let* (([_Consumed [_Ok value]] (mp-run-string parser input)))
      (expect value :to-equal expected))))

(describe "An elisp parser."

  (describe "A symbol parser."

    (let (test)
      (before-all
        (setq test (mp-test-run-string-parser (mp-parse-symbol))))

      (it "should parse elisp symbols."
        (--each '(
                  ("foo" . foo)
                  ("foo bar" . foo)
                  ("with-dash" . with-dash)
                  ("with-dash and more" . with-dash)
                  ("ending-with-number485" . ending-with-number485)
                  ("number-34-in-the-middle" . number-34-in-the-middle)
                  )
          (funcall test it)))))

  (describe "An integer parser."

    (let (test)
      (before-all
        (setq test (mp-test-run-string-parser (mp-parse-integer))))

      (it "should parse integers."
        (--each '(
                  ("124" . 124)
                  ("99" . 99)
                  ("0034" . 34)
                  ("1" . 1)
                  )
          (funcall test it)))

      (it "should not parse decimals."
        (--each '(
                  ("124.23" . 124)
                  ("99.34" . 99)
                  )
          (funcall test it)))))

  (describe "A number parser."

    (let (test)
      (before-all
        (setq test (mp-test-run-string-parser (mp-parse-number))))

      (it "should parse numbers."
        (--each '(
                  ("124" . 124)
                  ("99" . 99)
                  ("0034" . 34)
                  ("1" . 1)
                  ("1asd" . 1)
                  ("1.asd" . 1)
                  ("1.23asd" . 1.23)
                  ("10.34u" . 10.34)
                  ("124.23" . 124.23)
                  ("99.03" . 99.03)
                  ("0034.8" . 34.8)
                  ("1.0" . 1.0)
                  )
          (funcall test it)))))

  (describe "A quote parser."

    (let (test)
      (before-all
        (setq test (mp-test-run-string-parser (mp-parse-quote))))

      (it "should parse quoted forms."
        (--each '(
                  ("'(foo)" . '(foo))
                  ("'symbol" . 'symbol)
                  ("'34" . '34)
                  )
          (funcall test it)))))

  (describe "A char parser."

    (let (test)
      (before-all
        (setq test (mp-test-run-string-parser (mp-parse-char))))

      (it "should parse a character literal."
        (--each '(
                  ("?a" . ?a)
                  ("?\\(" . ?\()
                  ("?\\)" . ?\))
                  ("?\\\\" . ?\\)
                  ("?\\n" . ?\n)
                  )
          (funcall test it)))))

  (describe "A list parser."

    (let (test)
      (before-all
        (setq test (mp-test-run-string-parser (mp-parse-list))))

      (it "should parse lists."
        (--each '(
                  ("(a b c d)" . (a b c d))
                  ("(  a   b  c   d)" . (a b c d))
                  ("(a (b c) d)" . (a (b c) d))
                  ("(  a   ( b  c)  d)" . (a (b c) d))
                  ("(  a   ( b  c )  d  )" . (a (b c) d))
                  ("((a (b c) d) (((a)) (b c) d))" . ((a (b c) d) (((a)) (b c) d)))
                  ("(a (?a 23) d)" . (a (?a 23) d))
                  ("((a (?a 23) d))" . ((a (?a 23) d)))
                  ("(a)" . (a))
                  ("()" . nil)
                  ("(())" . (nil))
                  ("(() (a b))" . (nil (a b)))
                  ("((()))" . ((nil)))
                  ("( ( ( ) ) )" . ((nil)))
                  ("(\n(\n(\n)\n)\n)" . ((nil)))
                  )
          (funcall test it)))

      (it "should not parse lists which aren't lists."
        (--each '(
                  ("(a(!))" "!" ("whitespace" "letter" "-" "digit" "'" "?" "(" ")"))
                  )
          (-lambda ((input e-unexpected e-first))
            (-let* (([_Consumed [_Error [_Message pos unexpected first]]] (mp-run-string (mp-parse-list) input)))
              (expect unexpected :to-equal e-unexpected)
              (expect first :to-equal e-first)))))

      (it "should not parse something which doesn't start with ("
        (--each '(
                  ("a" "a" ("("))
                  ("3" "3" ("("))
                  )
          (-lambda ((input e-unexpected e-first))
            (-let* (([_Empty [_Error [_Message pos unexpected first]]] (mp-run-string (mp-parse-list) input)))
              (expect pos :to-equal 0)
              (expect unexpected :to-equal e-unexpected)
              (expect first :to-equal e-first)))))))

  (describe "An elisp item parser."

    (let (test)
      (before-all
        (setq test (mp-test-run-string-parser (mp-parse-lisp-item))))

      (it "should parse any valid elisp token."
        (--each '(
                  ("?a" . ?a)
                  ("asd foo" . asd)
                  ("12.34" . 12.34)
                  ("12" . 12)
                  ("( a b   c   d)" . (a b c d))
                  ("'( a b   c   d)" . '(a b c d))
                  )
          (funcall test it))))))
