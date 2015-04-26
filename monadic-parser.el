;; -*- lexical-binding: t -*-
(require 'cl)
;; data Message = (Int, String, [String]) -- position, unexpected input, first set (expected inputs)
(defun mp-message (pos input first)
  (vector 'Message pos input first))
(defun mp-message-pos (msg)
  (aref msg 1))
(defun mp-message-input (msg)
  (aref msg 2))
(defun mp-message-first (msg)
  (aref msg 3))

;; data State u = (Int, String, u) -- position in string, rest, user state
(defun mp-state (pos string user)
  (vector 'State pos string user))
(defun mp-state-position (state)
  (aref state 1))
(defun mp-state-rest (state)
  (aref state 2))
(defun mp-state-user (state)
  (aref state 3))

;; data Error = Message
(defun mp-error (msg)
  (vector 'Error msg))
(defun mp-error-message (err)
  (aref err 1))

;; data OK u a = (a, State u, Message)
(defun mp-ok (value state msg)
  (vector 'Ok value state msg))
(defun mp-ok-value (ok)
  (aref ok 1))
(defun mp-ok-state (ok)
  (aref ok 2))
(defun mp-ok-message (ok)
  (aref ok 3))

;; We will not wrap the binary constructors in an additional layer
;; (the "data" type).  We are simply expecting everything to fit
;; properly :D  See also Reply
;; data Consumed a = Consumed a | Empty a
(defun mp-consumed (value)
  (vector 'Consumed value))
(defun mp-empty (value)
  (vector 'Empty value))
(defun mp-with-consumed (consumed-or-empty consumed-code empty-code)
  (declare (indent 1))
  (-let* (([type] consumed-or-empty))
    (cond
     ((eq type 'Consumed)
      (funcall consumed-code consumed-or-empty))
     ((eq type 'Empty)
      (funcall empty-code consumed-or-empty))
     (t (error "Invalid type %s, expected Consumed or Empty" type)))))

;; data Reply u a =  OK u a Message | Error
(defun mp-with-reply (ok-or-error ok-code error-code)
  (declare (indent 1))
  (-let* (([type] ok-or-error))
    (cond
     ((eq type 'Ok)
      (funcall ok-code ok-or-error))
     ((eq type 'Error)
      (funcall error-code ok-or-error))
     (t (error "Invalid type %s, expected Ok or Error" type)))))

;; Finally, the parser type is.  We will omit the wrapper and just use
;; lambda directly
;; data Parser u a = State u -> Reply u a

;; Parser u a -> State u -> Reply u a
(defun mp-run (parser state)
  "Run the PARSER on STATE."
  (funcall parser state))

;; Parser u a -> String -> Reply u a
(defun mp-run-string (parser string)
  "Run the PARSER on STRING."
  (mp-run parser (mp-state 0 (string-to-list string) nil)))

;; (a -> b) -> Parser u a -> Parser u b
(defun mp-fmap (f parser)
  "Apply F to the result of PARSER."
  (lambda (state)
    (mp-with-consumed (mp-run parser state)
      (-lambda ([_Consumed reply])
        (mp-consumed
         (mp-with-reply reply
           (-lambda ([_Ok value state msg])
             (mp-ok (funcall f value) state msg))
           (lambda (err)
             err))))
      (-lambda ([_Empty reply])
        (mp-empty
         (mp-with-reply reply
           (-lambda ([_Ok value state msg])
             (mp-ok (funcall f value) state msg))
           (lambda (err)
             err)))))))

;; a -> Parser u a
(defun mp-return (value)
  "Create a parser returning VALUE while not consuming any input."
  (lambda (state) (mp-empty (mp-ok value state (mp-message (mp-state-position state) "" nil)))))

;; Message -> Reply u a -> Reply u a
(defun mp--merge-error-reply (msg1 reply)
  (mp-with-reply reply
    (-lambda ([_Ok value state msg2])
      (mp-ok value state (mp--merge-messages msg1 msg2)))
    (-lambda ([_Error msg2])
      (mp-error (mp--merge-messages msg1 msg2)))))

;; Message -> Message -> Message
(defun mp--merge-messages (msg1 msg2)
  (-let* (([_Message _   _      first1] msg1)
          ([_Message pos input first2] msg2))
    (mp-message pos input (-concat first1 first2))))

;; Parser u a -> (a -> Parser u b) -> Parser u b
(defun mp-bind (parser fun)
  "Use the result of first PARSER to construct another using FUN."
  (lambda (state)
    (mp-with-consumed (mp-run parser state)
      (-lambda ([_Consumed reply])
        (mp-consumed
         (mp-with-reply reply
           (-lambda ([_Ok value state1 msg1])
             (mp-with-consumed (mp-run (funcall fun value) state1)
               (-lambda ([_Consumed reply2])
                 reply2) ;; unwrap one layer of Consumed
               (-lambda ([_Empty reply2])
                 (mp--merge-error-reply msg1 reply2))))
           (lambda (err) err))))
      (-lambda ([_Empty reply])
        (mp-with-reply reply
          (-lambda ([_Ok value state1 msg1])
            (mp-with-consumed (mp-run (funcall fun value) state1)
              (-lambda (consumed) consumed)
              (-lambda ([_Empty reply2])
                (mp-empty (mp--merge-error-reply msg1 reply2)))))
          (lambda (err) (mp-empty err)))))))

;; Parser u a -> Parser u b -> Parser u b
(defun mp-then (first another)
  "Sequence two parsers, ignoring result from the first."
  (mp-bind first (lambda (_) another)))

;; All this fuss around or is due to need of lazy evaluation of the
;; rest of the parsers.  We must not evaluate them right away in the
;; case first parser succeeds.

;; [Parser u a] -> Parser u a
(defmacro mp-or (&rest parsers)
  "Try parsers in sequence until some consumes input or succeeds."
  `(mp--or1 ,parsers))

(defmacro mp--or1 (parsers)
  (cond
   ((not (cdr parsers))
    (car parsers))
   (t `(mp--or ,(car parsers) #'(lambda (s) (mp-run `,(mp--or1 ,(cdr parsers)) s))))))

;; Parser u a -> Parser u a -> Parser u a
(defun mp--or (first second)
  "Implement choice.

Try FIRST parser.  If it succeeds, return its return value.  If
it fails and *does not consume any input*, try the SECOND
parser."
  (lambda (state)
    (mp-with-consumed (mp-run first state)
      (lambda (consumed) consumed)
      (-lambda ([_Empty reply])
        (mp-with-reply reply
          (lambda (ok) (mp-empty ok))
          (-lambda ([_Error msg])
            (mp-with-consumed (mp-run second state)
              (lambda (consumed) consumed)
              (-lambda ([_Empty reply])
                (mp-empty (mp--merge-error-reply msg reply))))))))))

;; Parser u a -> Parser u a
(defun mp-try (parser)
  "Run PARSER without consuming any input if it fails.

Useful for arbitrary look-ahead."
  (lambda (state)
    (-let* (([_State _ pos] state))
      (mp-with-consumed (mp-run parser state)
        (-lambda ([_Consumed reply])
          (mp-with-reply reply
            (lambda (ok)
              (mp-consumed ok))
            (-lambda ([_Error [_Message _ input expected]])
              (mp-empty (mp-error (mp-message pos input expected))))))
        (lambda (empty) empty)))))

;; (mp-run-string (mp-or (mp-label (mp-try (mp-then (mp-char ?a) (mp-char ?b))) "ab")
;;                       (mp-char ?z)) "abdd")

;; Message -> String -> Message
(defun mp--expect (msg expected)
  (-let (([_Message pos input _] msg))
    (mp-message pos input (list expected))))

;; Parser u a -> String -> Parser u a
(defun mp-label (parser expected)
  "Label the parser."
  (lambda (state)
    (mp-with-consumed (mp-run parser state)
      (lambda (consumed) consumed)
      (-lambda ([_Empty reply])
        (mp-with-reply reply
          (-lambda ([_Ok value state1 msg])
            (mp-empty (mp-ok value state1 (mp--expect msg expected))))
          (-lambda ([_Error msg])
            (mp-empty (mp-error (mp--expect msg expected)))))))))

;; (a -> Bool) -> Parser u a
(defun mp-satisfies (predicate)
  "Construct a parser which consumes one item and returns it if it satisfies PREDICATE."
  ;; (mp-item) eats an item, so the function we bind to should create
  ;; a parser which does not consume any input, returning value based
  ;; on the predicate.  mp-return does the job!
  (-lambda ([_State pos (c . cs) user])
    (cond
     ((not c)
      (mp-empty (mp-error (mp-message pos "end of input" nil))))
     ((funcall predicate c)
      (mp-consumed (mp-ok c (mp-state (1+ pos) cs user) (mp-message pos "" nil))))
     (t (mp-empty (mp-error (mp-message pos (char-to-string c) nil)))))))

;; Char -> Parser u Char
(defun mp-char (char)
  "Create a parser matching CHAR."
  (mp-label (mp-satisfies (lambda (x) (eq char x))) (char-to-string char)))

;; String -> Parser u String
(defun mp-string (string)
  "Create a parser matching STRING."
  (let ((tokens (string-to-list string)))
    (if (not tokens)
        (lambda (state) (mp-return nil))
      (lambda (state)
        (-let* (([_State pos stream user] state)
                (token (car tokens))
                (x (car stream)))
          (cond
           ((and tokens (not x))
            ;; TODO: abstract the error message creation
            (mp-empty (mp-error (mp-message pos "end of input" (list string)))))
           ((eq token x)
            (mp-consumed
             (progn
               (cl-do* ((tokens (cdr tokens) (cdr tokens))
                        (stream (cdr stream) (cdr stream))
                        (token (car tokens) (car tokens))
                        (x (car stream) (car stream))
                        (pos (1+ pos) (1+ pos)))
                   (nil)
                 (cond
                  ((not tokens)
                   (cl-return (mp-ok string (mp-state pos stream user) (mp-message pos "" nil))))
                  ((not x)
                   (cl-return (mp-error (mp-message pos "end of input" (list string)))))
                  ((not (eq token x))
                   (cl-return (mp-error (mp-message pos (char-to-string x) (list string))))))))))
           ;; TODO: abstract the error message creation
           (t (mp-empty (mp-error (mp-message pos (char-to-string x) (list string)))))))))))

;; (mp-run-string (mp-char ?a) "abcd")
;; (mp-run-string (mp-then (mp-char ?a) (mp-or (mp-char ?c) (mp-char ?d))) "abcd")
;; (mp-run-string (mp-string "abd") "abda")
