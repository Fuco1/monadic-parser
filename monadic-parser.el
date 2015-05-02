;;; monadic-parser.el --- Efficient monadic parser. -*- lexical-binding: t -*-

;; Copyright (C) 2015 Matúš Goljer <matus.goljer@gmail.com>

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 2nd May 2015
;; Package-requires: ((dash "2.10.0"))
;; Keywords: data

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))


;;; Data definitions
;; data Message = (Int, String, [String]) -- position, unexpected input, first set (expected inputs)
(defun mp-message (pos input first)
  "Create new message instance.

Messages are used to report errors.

POS is the position in the input stream where the message originated.

INPUT is the unexpected input we have encountered.

FIRST is the first-set, in other words, the set of expected tokens."
  (vector 'Message pos input first))
(defun mp-message-pos (msg)
  (aref msg 1))
(defun mp-message-input (msg)
  (aref msg 2))
(defun mp-message-first (msg)
  (aref msg 3))

;; data State u = (Int, String, u) -- position in string, rest, user state
(defun mp-state (pos string user)
  "Create new state instance.

State contains all the necessary information for parsing.

This is an opaque type, users should not directly inspect it.

POS is the current position in the input stream.

STRING is the rest of the input stream.

USER is arbitrary user-supplied state.  The state is passed in
the background during the entire parsing computation."
  (vector 'State pos string user))
(defun mp-state-position (state)
  (aref state 1))
(defun mp-state-rest (state)
  (aref state 2))
(defun mp-state-user (state)
  (aref state 3))

;; data Reply u a =  OK u a Message | Error
;; data Error = Message
(defun mp-error (msg)
  "Create new error instance.

Error is the reply of the parser when it can't proceed because it
has encountered unexpected input.

MSG is a message instance, see `mp-message'."
  (vector 'Error msg))
(defun mp-error-message (err)
  (aref err 1))

;; data OK u a = (a, State u, Message)
(defun mp-ok (value state msg)
  "Create new ok instance.

OK is the reply of the parser when it proceeded and successfully
parsed some input

Beware, this does not mean it has consumed input, it can marely
be a reply to a look-ahead assertion etc.

VALUE is the return value of the parser.

STATE is the new state, see `mp-state'.

MSG is a message, see `mp-message'."
  (vector 'Ok value state msg))
(defun mp-ok-value (ok)
  (aref ok 1))
(defun mp-ok-state (ok)
  (aref ok 2))
(defun mp-ok-message (ok)
  (aref ok 3))

;; We will not wrap the binary constructors in an additional layer
;; (the "data" type).  We are simply expecting everything to fit
;; properly :D  See also Reply (Error or Ok)
;; data Consumed a = Consumed a | Empty a
(defun mp-consumed (reply)
  "Create new consumed instance.

A result _consumed_ means that the parser consumed portion of the
input.  We do not allow backtracking once part of input stream is
consumed.  If you need arbitrary look-ahead, you should wrap your
parsers with `mp-look-ahead' or `mp-try' combinators.

REPLY is an instance of `mp-error' or `mp-ok'."
  (vector 'Consumed reply))
(defun mp-empty (reply)
  "Create new consumed instance.

A result _empty_ means that the parser did not consume any input.
Since we do not allow backtracking once part of input stream is
consumed, this is used to distinguish look-ahead.  By default,
the parsers are LL(1), that is, they allow look ahead of one
token before consuming it.

REPLY is an instance of `mp-error' or `mp-ok'."
  (vector 'Empty reply))

(defun mp-with-consumed (consumed-or-empty consumed-code empty-code)
  "Helper function to deal with results.

CONSUMED-OR-EMPTY is an instace of `mp-consumed' or `mp-empty'.
Based on the type, CONSUMED-CODE or EMPTY-CODE is executed.

The arguments CONSUMED-CODE and EMPTY-CODE are lambdas/functions
which allows for \"lazy\" evaluations of the branches."
  (declare (indent 1))
  (-let* (([type] consumed-or-empty))
    (cond
     ((eq type 'Consumed)
      (funcall consumed-code consumed-or-empty))
     ((eq type 'Empty)
      (funcall empty-code consumed-or-empty))
     (t (error "Invalid type %s, expected Consumed or Empty" type)))))

(defun mp-with-reply (ok-or-error ok-code error-code)
  "Helper function to deal with replies.

OK-OR-ERROR is an instace of `mp-ok' or `mp-error'.
Based on the type, OK-CODE or ERROR-CODE is executed.

The arguments OK-CODE or ERROR-CODE are lambdas/functions which
allows for \"lazy\" evaluations of the branches."
  (declare (indent 1))
  (-let* (([type] ok-or-error))
    (cond
     ((eq type 'Ok)
      (funcall ok-code ok-or-error))
     ((eq type 'Error)
      (funcall error-code ok-or-error))
     (t (error "Invalid type %s, expected Ok or Error" type)))))

;; Finally, the parser type.  We will omit the wrapper and just use
;; lambda directly
;; data Parser u a = State u -> Reply u a

;; Parser u a -> State u -> Reply u a
(defun mp-run (parser state)
  "Run the PARSER on STATE.

This function is the most generic way to run a parser.  The
return value is either `mp-consumed' or `mp-empty'."
  (funcall parser state))

;; Parser u a -> String -> Reply u a
(defun mp-run-string (parser string)
  "Run the PARSER on STRING.

This works like `mp-run' but automatically wraps `string' in a
default `mp-state'.

The return value is either `mp-consumed' or `mp-empty'."
  (mp-run parser (mp-state 0 (string-to-list string) nil)))

;; (a -> b) -> Parser u a -> Parser u b
(defun mp-fmap (f parser)
  "Apply F to the result of PARSER.

This function creates a new parser which behaves just like PARSER
but the return value is run through F."
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

;; (State u -> State u) -> Parser u (State u)
(defun mp-update-parser-state (fun)
  "Update the parser state with function FUN."
  (lambda (state)
    (let ((new-state (funcall fun state)))
      (mp-empty (mp-ok new-state new-state (mp-message (mp-state-position new-state) "" nil))))))

;; Parser u (State u)
(defun mp-get-parser-state ()
  "Return the internal parser state."
  (mp-update-parser-state 'identity))

;; State u -> Parser u ()
(defun mp-set-parser-state (new-state)
  "Set the internal parser state."
  (mp-then
   (mp-update-parser-state (-const new-state))
   (mp-return nil)))

;; Parser u u
(defun mp-get-user-state ()
  "Return the user state.

See `mp-set-user-state'."
  (fmap (-lambda ([_State _ _ user]) user) (mp-get-parser-state)))

;; u -> Parser u ()
(defun mp-set-user-state (new-user-state)
  "Set the user state.

This and `mp-get-user-state' allows the user to thread arbitrary
state through the computation in a pure way.

When you update the state, *you must never change the value by
side effect*, otherwise backtracking will break horribly."
  (mp-then
   (mp-update-parser-state (-lambda ([_State pos string _]) (mp-state pos string new-user-state)))
   (mp-return nil)))

;; Message -> Reply u a -> Reply u a
(defun mp--merge-error-reply (msg1 reply)
  "Merge the message MSG1 into the message tracked inside REPLY."
  (mp-with-reply reply
    (-lambda ([_Ok value state msg2])
      (mp-ok value state (mp--merge-messages msg1 msg2)))
    (-lambda ([_Error msg2])
      (mp-error (mp--merge-messages msg1 msg2)))))

;; Message -> Message -> Message
(defun mp--merge-messages (msg1 msg2)
  "Merge the first sets of MSG1 and MSG2."
  (-let* (([_Message _   _      first1] msg1)
          ([_Message pos input first2] msg2))
    (mp-message pos input (-concat first1 first2))))

;; Parser u a -> (a -> Parser u b) -> Parser u b
(defun mp-bind (parser fun)
  "Use the result of first PARSER to construct another using FUN.

First, run PARSER.  Extract its return value and pass it into
FUN.  FUN should return a new parser, using or ignoring the bound
value.  The state is passed in the background automatically."
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
;; case first parser succeeds.  For example, if inside a parser `foo'
;; we did (mp-or (bar) (foo)), this would lead to infinite recursion
;; because (foo) would try to evaluate to a function which would call
;; the (mp-or ...) which would try to evaluate (foo) ...  You can
;; still crash your stack if you let-bind or do other unexpected
;; things :/

;; [Parser u a] -> Parser u a
(defmacro mp-or (&rest parsers)
  "Try PARSERS in sequence until some consumes input or succeeds.

Return the value of the first parser that consumed input or succeeded."
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

;; Parser u a -> Parser u a
(defun mp-look-ahead (parser)
  "Run PARSER without consuming any input."
  (mp-do
   (state := (mp-get-parser-state))
   (x := parser)
   (mp-set-parser-state state)
   (mp-return x)))

;; (mp-run-string (mp-look-ahead (mp-string "abc")) "abcd")
;; (mp-run-string (mp-get-parser-state) "abcd")

(defun mp-many-till (parser end)
  "Parse zero or more occurrences of PARSER until END succeeds.

Return the list of values returned by PARSER."
  (mp-or
   (mp-then end (mp-return nil))
   (mp-do
    (x := parser)
    (xs := (mp-many-till parser end))
    (mp-return (cons x xs)))))

;; (mp-run-string (mp-many-till (mp-item) (mp-char ?$)) "foo bar$ baz")

;; Message -> String -> Message
(defun mp--expect (msg expected)
  "Set the first set to a singleton set containing EXPECTED."
  (-let (([_Message pos input _] msg))
    (mp-message pos input (list expected))))

;; Parser u a -> String -> Parser u a
(defun mp-label (parser expected)
  "Label the parser.

The parsers automatically collect the possible following tokens
into a set which is then displayed to the user.  This label is
used as an \"expected\" string to identify the parser in the
first-set."
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
  (-lambda ([_State pos (c . cs) user])
    (cond
     ((not c)
      (mp-empty (mp-error (mp-message pos "end of input" nil))))
     ((funcall predicate c)
      ;; TODO: abstract creating empty error
      (mp-consumed (mp-ok c (mp-state (1+ pos) cs user) (mp-message pos "" nil))))
     (t (mp-empty (mp-error (mp-message pos (char-to-string c) nil)))))))

(defun mp--walk-many (accumulator default parser ok)
  "Read tokens as long as PARSER succeeds.

Accumulate results using the funciton ACCUMULATOR, with DEFAULT as the initial value.

OK is the OK reply after first token has been consumed.

This function is only called if we consumed at least one token."
  (-let* (([_Ok value state _msg] ok)
          (re (funcall accumulator value default)))
    (cl-do ((result (mp-run parser state) (mp-run parser state)))
        (nil)
      (mp-with-consumed result
        (-lambda ([_Consumed reply])
          (mp-with-reply reply
            (-lambda ([_Ok value state1 _msg])
              (setq re (funcall accumulator value re))
              (setq state state1))
            (lambda (err) (cl-return err))))
        (-lambda ([_Empty reply])
          (mp-with-reply reply
            (lambda (_ok) (error "Combinator `mp-many' is applied to a parser that accepts an empty string."))
            (-lambda ([_Error msg]) (cl-return (mp-ok re state msg)))))))))

;; (a -> b -> b) -> b -> Parser u a -> Parser u b
(defun mp-many-accum (accumulator default parser)
  "Match PARSER zero or more times and accumulate its results using ACCUMULATOR.

DEFAULT is the initial value.

Notice the similarity with folding/reducing."
  (lambda (state)
    (mp-with-consumed (mp-run parser state)
      (-lambda ([_Consumed reply])
        (mp-consumed
         (mp-with-reply reply
           (lambda (ok) (mp--walk-many accumulator default parser ok))
           (lambda (err) err))))
      (-lambda ([_Empty reply])
        (mp-with-reply reply
          (lambda (_ok) (error "Combinator `mp-many' is applied to a parser that accepts an empty string."))
          (-lambda ([_Error msg]) (mp-empty (mp-ok nil state msg))))))))

;; (mp-run-string (mp-spaces) "   asd")

(defun mp-many (parser)
  "Match PARSER zero or more times and return its results as list."
  (mp-fmap 'nreverse (mp-many-accum 'cons nil parser)))

(defun mp-many1 (parser)
  "Match PARSER one or more times and return its results as list."
  (mp-do
   (x := parser)
   (xs := (mp-many parser))
   (mp-return (cons x xs))))

(defun mp-skip-many (parser)
  "Apply PARSER zero or more times, ignoring its results."
  (mp-do
   (mp-many-accum (lambda (_1 _2) nil) nil parser)
   (mp-return nil)))

;; Parser u a
(defun mp-item ()
  "Consume one token of input."
  (mp-satisfies (-const t)))

;; Parser u ()
(defun mp-not-followed-by (parser)
  "Succeed when PARSER fails.

Does not consume any input."
  (mp-try
   (mp-or
    (mp-do
     (x := parser)
     (-lambda ([_State pos _ _]) (mp-empty (mp-error (mp-message pos x nil)))))
    (mp-return ()))))

;; (mp-run-string (mp-not-followed-by (mp-try (mp-string "abc"))) "abd")

(defun mp-option (default parser)
  "Try to parse PARSER, returning DEFAULT if PARSER fails without consuming input."
  (mp-or parser (mp-return default)))

;; (mp-run-string (mp-then (mp-string "ab") (mp-option "." (mp-char 59))) "ab;")

(defun mp-optional (parser)
  "Optionally parse PARSER, discarding its value.

It will parse PARSER or nothing. It only fails if PARSER fails
after consuming input."
  (mp-or (mp-then parser (mp-return nil)) (mp-return nil)))

;; (mp-run-string (mp-do (mp-string "foo") (mp-optional (mp-many (mp-char ?-))) (mp-string "bar")) "foo----bar")
;; (mp-run-string (mp-do (mp-string "foo") (mp-optional (mp-many (mp-char ?-))) (mp-string "bar")) "foo-bar")
;; (mp-run-string (mp-do (mp-string "foo") (mp-optional (mp-many (mp-char ?-))) (mp-string "bar")) "foobar")

;; Parser u nil
(defun mp-end-of-input ()
  "Consume no input and fail everywhere except at the end of input."
  (mp-label (mp-not-followed-by (mp-item)) "end of input"))

;; (mp-run-string (mp-or (mp-end-of-input) (mp-char ?b)) "a")

;; Char -> Parser u Char
(defun mp-char (char)
  "Create a parser matching CHAR."
  (mp-label (mp-satisfies (lambda (x) (eq char x))) (char-to-string char)))

;; Parser u Char
(defun mp-letter ()
  "Match any char in [:alpha:]."
  (mp-label (mp-satisfies (lambda (x) (eq (char-syntax x) ?w))) "letter"))

(defun mp-digit ()
  "Match any char in [:digit:]."
  (mp-label (mp-satisfies (lambda (x) (and (<= 48 x) (<= x 57)))) "digit"))

(defun mp-space ()
  "Match any whitespace character: space, tab, newline or carriage return."
  (mp-label (mp-satisfies (lambda (x) (memq x '(32 10 9 12)))) "whitespace"))

(defun mp-spaces ()
  "Match any sequence of whitespace."
  (mp-skip-many (mp-space)))

;; Could've been implemented as series of mp-char parsers, but this is
;; way faster (we don't need to bind the entire chain)
;; String -> Parser u String
(defun mp-string (string)
  "Create a parser matching STRING."
  (let ((tokens (string-to-list string)))
    (if (not tokens)
        (lambda (_state) (mp-return nil))
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

(defun mp--edebug-is-assign (symbol)
  "Check if the symbol is :="
  (eq symbol :=))

(defun mp--do-substitute (form)
  (cond
   ((and (listp form) (eq (car form) 'let))
    `(-let ,(cadr form)
       (mp-do ,@(cddr form))))
   ((and (listp form) (eq (car form) 'let*))
    `(-let* ,(cadr form)
       (mp-do ,@(cddr form))))
   ((and (listp form) (eq (car form) 'if))
    `(if ,(nth 1 form)
         (mp-do ,(nth 2 form))
       (mp-do ,@(nthcdr 3 form))))
   (t form)))

;; (mp-run-string (mp-do
;;                 (a := (mp-char ?a))
;;                 (if (eq a ?a)
;;                     (mp-return "Success")
;;                   (let ((r 8))
;;                     (mp-char ?b)
;;                     (mp-return (+ r a))))
;;                 (mp-return "Total return"))
;;                "abcd")

(defmacro mp-do (&rest things)
  "Compose parsers and forms, binding variables automatically.

THINGS is a list of PARSER-FORM, ASSIGN-FORM, LET-FORM or IF-FORM.

- PARSER-FORM is either a symbol evaluating to a parser or a
function returning a parser.

- ASSIGN-FORM is a list of the structure (PATTERN :=
PARSER-FORM).  It binds the result value of the parser returned
by PARSER-FORM using PATTERN specification.

PATTERN uses the same syntax as `-let' from dash.el library.

All parsers after an ASSIGN-FORM have access to the bound variable.

- LET-FORM is a just like regular `let' of elisp but supports
binding patterns of `-let' automatically.  Note that the LET-FORM
must evaluate to a parser.

- IF-FORM is just a regular `if' from elisp, but both branches must
evaluate to parsers."
  (declare (debug (&rest [&or (sexp mp--edebug-is-assign form)
                              form])))
  (let ((head (car things)))
    (cond
     ((and (listp head) (eq (cadr head) ':=))
      `(mp-bind
        ,(mp--do-substitute (nth 2 head))
        (-lambda (,(car head))
          (mp-do ,@(cdr things)))))
     ((cdr things)
      `(mp-then
        ,(mp--do-substitute (car things))
        (mp-do ,@(cdr things))))
     (t (mp--do-substitute (car things))))))

(provide 'monadic-parser)
;;; monadic-parser.el ends here
