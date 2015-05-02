;;; elisp-parser.el --- Simplistic elisp parser. -*- lexical-binding: t -*-

;; Copyright (C) 2015 Matúš Goljer <matus.goljer@gmail.com>

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 2nd May 2014
;; Package-requires: ((monadic-parser "0.0.1") (dash "2.10.0"))
;; Keywords:

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

;; This is a simplistic elisp parser which showcases the
;; monadic-parser library.  It does not recognize all elisp syntax.
;; Feel free to submit pull requests with additions! :)

;;; Code:

(defun mp-parse-symbol ()
  "Parse an elisp symbol."
  (mp-fmap
   (lambda (x) (intern (apply 'string x)))
   (mp-do
    (first := (mp-or (mp-letter) (mp-char ?-)))
    (rest := (mp-many (mp-or (mp-letter) (mp-digit) (mp-char ?-))))
    (mp-return (cons first rest)))))

(defun mp-parse-integer ()
  "Parse an integer number."
  (mp-fmap (lambda (x) (string-to-number (apply 'string x))) (mp-many1 (mp-digit))))

(defun mp-parse-number ()
  "Parse any number."
  (mp-do
   (integer := (mp-many1 (mp-digit)))
   (decimal := (mp-or
                (mp-try (mp-then (mp-char ?.) (mp-many1 (mp-digit))))
                (mp-return nil)))
   (mp-return (if decimal
                  (string-to-number (concat integer "." decimal))
                (string-to-number (apply 'string integer))))))

(defun mp-parse-quote ()
  "Parse a quoted elisp form."
  (mp-do
   (mp-char ?')
   (item := (mp-parse-lisp-item))
   (mp-return `',item)))

(defun mp-parse-char ()
  "Parse the character token.

In elisp, this means ? followed by an item."
  (mp-do
   (mp-char ??)
   (mp-or
    (mp-do
     (mp-char ?\\)
     (char := (mp-item))
     (mp-return
      ;; Special read syntax for special characters
      (cond
       ((eq char ?a) 7)
       ((eq char ?b) 8)
       ((eq char ?t) 9)
       ((eq char ?n) 10)
       ((eq char ?v) 11)
       ((eq char ?f) 12)
       ((eq char ?r) 13)
       ((eq char ?e) 27)
       ((eq char ?s) 32)
       ((eq char ?\\) 92)
       ((eq char ?d) 127)
       (t char))))
    (mp-item))))

(defun mp-parse-list ()
  "Parse elisp list."
  (mp-do
   (mp-char ?\()
   (s := (mp-many (mp-try (mp-then (mp-spaces) (mp-parse-lisp-item)))))
   (mp-spaces)
   (mp-char ?\))
   (mp-return s)))

(defun mp-parse-lisp-item ()
  "Parse any elisp item."
  (mp-or
   (mp-parse-symbol)
   (mp-parse-number)
   (mp-parse-quote)
   (mp-parse-char)
   (mp-parse-list)))

(provide 'elisp-parser)
;;; elisp-parser.el ends here
