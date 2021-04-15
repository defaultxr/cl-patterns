;;; cl-patterns-skeletons.el --- collection of skeletons  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 modula t.

;; Author: modula t. <defaultxr AT gmail DOT com>
;; Homepage: https://github.com/defaultxr/cl-patterns
;; Version: 0.5
;; Package-Requires: ((s "1.0") (emacs "24.4"))
;; Keywords: convenience, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; These are just a few skeletons for commonly-used

;;; Code:

(require 's)
(require 'cl-patterns-helpers)

(defcustom cl-patterns-bdef-default-directory nil
  "The directory that `bdef-skeleton''s filename completion should start from. If nil (the default), start from the current buffer's directory instead.")

(define-skeleton tempo-skeleton
  "Insert (tempo ...) with the current *clock* tempo."
  ""
  "(tempo " (number-to-string (or (cl-patterns-lisp-eval `(cl:* 60 (cl-patterns:tempo cl-patterns:*clock*))) 110)) _ "/60)")

(define-skeleton bdef-skeleton
  "Prompt for a file, then insert (bdef ...) that loads said file."
  ""
  "(bdef "
  (let* ((filename (read-file-name "bdef file? " cl-patterns-bdef-default-directory))
         (filename-base (file-name-base filename))
         (sym (concat ":"
                      (or (cl-patterns-lisp-eval `(cl:when (cl:find-package 'mutility)
                                                           (cl:funcall
                                                            (cl:find-symbol "FRIENDLY-STRING" 'mutility)
                                                            ,filename-base)))
                          (s-replace-regexp "-+" "-"
                                            (s-replace-all (list (cons "_" "-")
                                                                 (cons " " "-")
                                                                 (cons "(" "")
                                                                 (cons ")" ""))
                                                           filename-base))))))
    (concat sym " \"" (s-replace "\"" "\\\"" filename) "\""))
  ")")

(define-skeleton pb-skeleton
  "Insert (pb ...), prompting for a name and an instrument."
  ""
  "(pb " (read-string "pb name? " (cl-patterns-guess-pdef)) "
  :instrument :" (let* ((instrument (completing-read "Instrument: "
                                                     (cl-patterns-lisp-eval
                                                      `(cl:mapcar 'cl:string-downcase (cl-patterns:all-instruments)))
                                                     nil nil))
                        (args (cl-patterns-lisp-eval `(cl:mapcar
                                                       (cl:lambda (x)
                                                                  (cl:symbol-name (cl:car (alexandria:ensure-list x))))
                                                       (cl-patterns::backend-instrument-controls (cl:intern ,(upcase instrument) :keyword) (cl:car (cl-patterns:enabled-backends))))))
                        (buf-arg (or (member "BUFFER" args)
                                     (member "BUFNUM" args))))
                   (concat instrument (when buf-arg
                                        (concat "\n  :" (downcase (car buf-arg)) " :" (cl-patterns-guess-bdef))))) "
  :dur 1" _ "
  :pfindur 4)")

(define-skeleton pt-skeleton
  "Insert a basic ptracker pattern."
  ""
  "(pdef :" _ "
    (ptracker
     (list :note 0 :dur 1/4 :instrument :" (cl-patterns-guess-instrument) ")
     #T(- ;; 0
        -
        -
        -
        - ;; 4
        -
        -
        -
        - ;; 8
        -
        -
        -
        - ;; 12
        -
        -
        -
        )))")

(provide 'cl-patterns-skeletons)
;;; cl-patterns-skeletons.el ends here
