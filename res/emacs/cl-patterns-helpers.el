;;; cl-patterns-helpers.el --- helper functions for use with cl-patterns  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 modula t.

;; Author: modula t. <defaultxr AT gmail DOT com>
;; Homepage: https://github.com/defaultxr/cl-patterns
;; Version: 0.5
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, languages, lisp, multimedia

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

;; This is a small library that makes it more convenient to work with
;; cl-patterns and the synthesis engines it supports. Incuded are functions
;; for:
;; - playing/stopping the pattern, buffer, proxy, or other object under point
;; - stopping all playing patterns, nodes, etc.
;; - opening the SuperCollider documentation for a specific class (w/completion)
;; - and a few others.
;;
;; Recommended setup for your Emacs init file:
;;
;; (defun cl-patterns-helpers-load ()
;;   (interactive)
;;   (sly-eval-async '(cl:namestring (asdf:system-source-directory (asdf:find-system 'cl-patterns)))
;;     (lambda (path)
;;       (load (concat path "res/emacs/cl-patterns-helpers") nil nil nil t)
;;       (load (concat path "res/emacs/cl-patterns-skeletons") nil nil nil t)))
;;   (define-key sly-mode-map (kbd "C-c p") 'cl-patterns-play-or-end-context-or-select-pdef)
;;   (define-key sly-mode-map (kbd "C-c P") 'cl-patterns-play-or-stop-context-or-select-pdef)
;;   (define-key sly-mode-map (kbd "C-c s") 'cl-patterns-stop-all)
;;   (define-key sly-doc-map (kbd "s") 'cl-patterns-supercollider-documentation))
;; 
;; (add-hook 'sly-connected-hook 'cl-patterns-helpers-load)
;;
;; The above should also work with slime; just replace all instances of "sly"
;; with "slime".

;;; Code:

(require 'cl-lib)

;; helper functions

(defvar cl-patterns-named-object-regexp "\[\(:\]\\(pdef\\|pb\\|defsynth\\|ds\\|proxy\\|dn\\|bdef\\)\[ \t\n\]+\\([:']?\[^ \t\n\]+\\)"
  "Regexp to find \"named objects\", i.e. pdef, pb, defsynth, proxy, bdef, etc.")

(defun cl-patterns-lisp-eval (sexp)
  "Evaluate SEXP via slime or sly."
  (if (fboundp 'slime-eval)
      (slime-eval sexp)
    (sly-eval sexp)))

(defun cl-patterns-lisp-interactive-eval (string)
  "Evaluate STRING via slime or sly."
  (let ((current-prefix-arg nil))
    (if (fboundp 'slime-eval)
        (slime-interactive-eval string)
      (sly-interactive-eval string))))

(defun cl-patterns-lisp-eval-region (start end)
  "Evaluate the specified region via slime or sly."
  (if (fboundp 'slime-eval)
      (slime-eval-region start end)
    (sly-eval-region start end)))

(defun cl-patterns-context ()
  "Get the type (i.e., pdef, defsynth, proxy, etc) and name of the current context, or nil if there is nothing under the point."
  (when-let ((bounds (bounds-of-thing-at-point 'defun)))
    (save-excursion
      (save-restriction
        (narrow-to-region (car bounds) (cdr bounds))
        (goto-char (point-min))
        (search-forward-regexp cl-patterns-named-object-regexp nil t)
        (list (ignore-errors (intern (match-string-no-properties 1))) (match-string-no-properties 2))))))

(defun cl-patterns-get-name-of-previous-item (items)
  "Get the name of the previous form whose type is one of ITEMS. For example, if ITEMS is '(defsynth proxy) then find the nearest defsynth or proxy before the point and return its name."
  (save-excursion
    (let ((context (or (cl-patterns-context)
                       (list nil))))
      (beginning-of-defun)
      (while (and (not (member (car context) items))
                  (not (= (point) (point-min))))
        (backward-sexp)
        (setf context (cl-patterns-context)))
      (cadr context))))

(defun cl-patterns-guess-pdef ()
  "Find the nearest pdef name before point."
  (cl-patterns-get-name-of-previous-item (list 'pb 'pdef)))

(defun cl-patterns-guess-instrument ()
  "Find the nearest instrument name before point."
  (cl-patterns-get-name-of-previous-item (list 'defsynth 'ds 'proxy 'dn)))

(defun cl-patterns-guess-bdef ()
  "Find the nearest bdef name before point."
  (cl-patterns-get-name-of-previous-item (list 'bdef)))

(defun cl-patterns-select-instrument ()
  "Select an instrument from the list of currently-defined instruments."
  (interactive)
  (let ((instruments (cl-patterns-lisp-eval `(cl:mapcar 'cl:string-downcase (cl-patterns:all-instruments))))
        (guess (cl-patterns-guess-instrument)))
    (completing-read "Instrument? " instruments nil nil (when (member guess instruments) guess))))

;; commands

(defun cl-patterns-stop-all ()
  "Stop all currently-playing patterns, nodes, etc."
  (interactive)
  (cl-patterns-lisp-eval '(cl-patterns:stop t)))

(defun cl-patterns-play-or-end-context (&optional stop)
  "Play or end the pdef, synthdef, bdef, etc, underneath the point. When STOP is true, play or stop instead."
  (interactive)
  (cl-destructuring-bind (type name) (cl-patterns-context)
    (cond
     ((member type (list 'pdef 'pb 'pbind))
      (cl-patterns-lisp-interactive-eval (concat "(cl-patterns:" (if stop "play-or-stop" "play-or-end") " " name ")")))
     ((member type (list 'proxy 'dn))
      (let ((bounds (bounds-of-thing-at-point 'defun)))
        (if (cl-patterns-lisp-eval `(cl-patterns:playing-p (cl-patterns::lookup-object-for-symbol ,(intern name))))
            (cl-patterns-lisp-interactive-eval (concat "(cl-patterns:" (if stop "play-or-stop" "play-or-end") " " name ")"))
          (cl-patterns-lisp-eval-region (car bounds) (cdr bounds)))))
     ((member type (list 'defsynth 'defsynth* 'ds))
      (cl-patterns-lisp-interactive-eval
       (concat "(cl-patterns:play (cl-patterns:event :instrument " name " :dur 2 :amp 1 :latency 0 :quant 0))")))
     ((member type (list 'bdef))
      (cl-patterns-lisp-interactive-eval (concat "(cl-patterns:play (bdef:bdef " name "))"))))))

(defun cl-patterns-play-or-stop-context ()
  "Play or stop the pdef, synthdef, bdef, etc, etc, underneath the point."
  (interactive)
  (cl-patterns-play-or-end-context t))

(defun cl-patterns-play-or-end-pdef (&optional stop) ;; FIX: list playing patterns first and write " (playing)" after each one.
  "Select a pdef to play or end. With a prefix argument, stop instead of end."
  (interactive "P")
  (when-let* ((pdefs (cl-patterns-lisp-eval `(cl-patterns:all-pdefs)))
              (selection (completing-read "Pdef? " pdefs nil nil))
              (func (if stop "cl-patterns:play-or-stop" "cl-patterns:play-or-end")))
    (cl-patterns-lisp-interactive-eval (concat "(" func " " selection ")"))))

(defun cl-patterns-play-or-stop-pdef ()
  "Select a pdef to play or stop."
  (interactive)
  (cl-patterns-play-or-end-pdef t))

(defun cl-patterns-play-or-end-context-or-select-pdef (&optional arg stop)
  "Play or end the current context, or if no relevant context was found (and ARG was not provided), call `cl-patterns-play-or-end-pdef' to select a pdef to play/end. When STOP is true, stop instead of end."
  (interactive "P")
  (let ((context (cl-patterns-context)))
    (if (or arg
            (not (car context)))
        (cl-patterns-play-or-end-pdef stop)
      (cl-patterns-play-or-end-context stop))))

(defun cl-patterns-play-or-stop-context-or-select-pdef (&optional arg)
  "Play or stop the current context, or if no relevant context was found (and ARG was not provided), call `cl-patterns-play-or-stop-pdef' to select a pdef to play/stop."
  (interactive "P")
  (let ((context (cl-patterns-context)))
    (if (or (not (car context)) arg)
        (cl-patterns-play-or-stop-pdef)
      (cl-patterns-play-or-stop-context))))

;; supercollider documentation functions

(defvar cl-patterns-supercollider-classes-list nil
  "List of SuperCollider classes for `cl-patterns-supercollider-populate-classes-list'.")

(defun cl-patterns-populate-supercollider-classes-list (&optional force)
  "Populate `cl-patterns-supercollider-classes-list' if it is nil or FORCE is true."
  (when (or (not cl-patterns-supercollider-classes-list)
            force)
    (let ((file (concat (temporary-file-directory) "supercollider-populate-classes-list.scd")))
      (with-temp-file file
        (insert "\"-----\".postln;Object.allSubclasses.do(_.postcs);\"-----\".postln;0.exit;"))
      (let ((sclang-classes-process (start-process "sclang-classes-process" "sclang-classes-output" "sclang" file)))
        (set-process-sentinel sclang-classes-process
                              (lambda (process event)
                                (when (string= "finished\n" event)
                                  (with-current-buffer "sclang-classes-output"
                                    (goto-char (point-min))
                                    (search-forward "\n-----\n")
                                    (setf cl-patterns-supercollider-classes-list
                                          (sort (split-string (buffer-substring-no-properties (point) (- (save-excursion (search-forward "\n-----\n") (point)) 6)) "\n" t) #'string<)))
                                  (kill-buffer "sclang-classes-output"))))))))

(cl-patterns-populate-supercollider-classes-list)

(defun cl-patterns-supercollider-documentation (ugen)
  (interactive (list
                (completing-read "Class: " cl-patterns-supercollider-classes-list nil nil "^")))
  (browse-url (concat "http://doc.sccode.org/Classes/" ugen ".html")))

(provide 'cl-patterns-helpers)
;;; cl-patterns-helpers.el ends here
