;;; inform.el --- Symbol links in Info buffers to their help documentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  H. Dieter Wilhelm

;; Author: H. Dieter Wilhelm <dieter@duenenhof-wilhelm.de>
;; Maintainer: H. Dieter Wilhelm
;; Package-Requires: ((emacs "25.1"))
;; Keywords: help, docs, convenience
;; Version: 1.0
;; URL: https://github.com/dieter-wilhelm/inform

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides links for symbols (functions, variables, ...)
;; within texinfo (*info*) buffers to their help documentation.

;; (setq mouse-1-click-follows-link nil) is influencing the behaviour
;; of the links, default: 450 (milli seconds) (Drew)

;; The code is mostly copied from lisp/help-mode.el

;;; Todo:

;; Back / Forward button in help buffer - back to info buffer or
;; remain in help mode?

;; Twice clicking or RETurning removes *Help* buffer (Drew)

;; Different colours for different symbol type (Drew) see package
;; helpful on Melpa

;; Documentation strings are not yet adopted from help to Inform.

;; Possibly useful help features are not yet explored and still
;; commented out

;;; Code:

(require 'info)				;redundant?
(require 'button)
(require 'cl-lib)
(require 'help-mode)			;redundant?

(defvar describe-symbol-backends) 	;from help-mode.el
(defvar help-xref-following)		;dito

(defcustom inform-make-xref-flag t
  "Non-nil means create symbol links in info buffers."
  :type '(boolean)
  :group 'Info)

(when inform-make-xref-flag
  (add-hook 'Info-selection-hook 'inform-make-xrefs))

;; Button types

(define-button-type 'inform-xref
  'link t			   ;for Info-next-reference-or-link
  'follow-link t
  'action #'inform-button-action)

(define-button-type 'inform-function
  :supertype 'inform-xref
  'inform-function 'describe-function
  'inform-echo (purecopy "mouse-2, RET: describe this function"))

(define-button-type 'inform-variable
  :supertype 'inform-xref
  'inform-function 'describe-variable
  'inform-echo (purecopy "mouse-2, RET: describe this variable"))

(define-button-type 'inform-face
  :supertype 'inform-xref
  'inform-function 'describe-face
  'inform-echo (purecopy "mouse-2, RET: describe this face"))

(define-button-type 'inform-symbol
  :supertype 'inform-xref
  'inform-function #'describe-symbol
  'inform-echo (purecopy "mouse-2, RET: describe this symbol"))

(define-button-type 'inform-function-def
  :supertype 'inform-xref
  'inform-function (lambda (fun &optional file type)
                   (or file
                       (setq file (find-lisp-object-file-name fun type)))
                   (if (not file)
                       (message "Unable to find defining file")
                     (require 'find-func)
                     (when (eq file 'C-source)
                       (setq file
                             (help-C-file-name (indirect-function fun) 'fun)))
                     ;; Don't use find-function-noselect because it follows
                     ;; aliases (which fails for built-in functions).
                     (let ((location
                            (find-function-search-for-symbol fun type file)))
                       (pop-to-buffer (car location))
                       (run-hooks 'find-function-after-hook)
                       (if (cdr location)
                           (goto-char (cdr location))
                         (message "Unable to find location in file")))))
  'inform-echo (purecopy "mouse-2, RET: find function's definition"))

;; Functions

(defun inform-button-action (button)
  "Call BUTTON's help function."
  (inform-do-xref nil
                (button-get button 'inform-function)
                (button-get button 'inform-args)))

;; -TODO-
(defun inform-do-xref (_pos function args)
  "Call the help cross-reference function FUNCTION with args ARGS.
Things are set up properly so that the resulting `help-buffer' has
a proper [back] button."
  ;; There is a reference at point.  Follow it.
  (let ((help-xref-following nil))
    (apply function (if (eq function 'info)
                        (append args (list (generate-new-buffer-name "*info*"))) args))))

(defun inform-xref-button (match-number type &rest args)
  "Make a hyperlink for cross-reference text previously matched.
MATCH-NUMBER is the subexpression of interest in the last matched
regexp.  TYPE is the type of button to use.  Any remaining arguments are
passed to the button's help-function when it is invoked.
See `inform-make-xrefs' Don't forget ARGS." ; -TODO-
  ;; Don't mung properties we've added specially in some instances.
  (unless (button-at (match-beginning match-number))
    ;; (message "Creating button: %s." args)
    (make-text-button (match-beginning match-number)
                      (match-end match-number)
                      'type type 'inform-args args)))

(defconst inform-xref-symbol-regexp
  (purecopy (concat "\\(\\<\\(\\(variable\\|option\\)\\|"  ; Link to var
                    "\\(function\\|command\\|call\\)\\|"   ; Link to function
                    "\\(face\\)\\|"			   ; Link to face
                    "\\(symbol\\|program\\|property\\)\\|" ; Don't link
                    "\\(source \\(?:code \\)?\\(?:of\\|for\\)\\)\\)"
                    "[ \t\n]+\\)?"
                    ;; Note starting with word-syntax character:
                    "['`‘]\\(\\sw\\(\\sw\\|\\s_\\)+\\|`\\)['’]"))
  "Regexp matching doc string references to symbols.

The words preceding the quoted symbol can be used in doc strings to
distinguish references to variables, functions and symbols.")

(defun inform-make-xrefs (&optional buffer)
  "Parse and hyperlink documentation cross-references in the given BUFFER.

Find cross-reference information in a buffer and activate such cross
references for selection with `help-follow'.  Cross-references have
the canonical form `...'  and the type of reference may be
disambiguated by the preceding word(s) used in
`inform-xref-symbol-regexp'.  Faces only get cross-referenced if
preceded or followed by the word `face'.  Variables without
variable documentation do not get cross-referenced, unless
preceded by the word `variable' or `option'."

  ;; -TODO-

  ;;   If the variable `help-xref-mule-regexp' is non-nil, find also
  ;; cross-reference information related to multilingual environment
  ;; \(e.g., coding-systems).  This variable is also used to disambiguate
  ;; the type of reference as the same way as `help-xref-symbol-regexp'.

  ;; A special reference `back' is made to return back through a stack of
  ;; help buffers.  Variable `help-back-label' specifies the text for
  ;; that.


  (interactive "b")
  ;; (message "Creating xrefs..")
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      ;; Skip the header-type info, though it might be useful to parse
      ;; it at some stage (e.g. "function in `library'").
      ;;      (forward-paragraph)
      (with-silent-modifications	;from Stefan
        (let (;(stab (syntax-table))
              (case-fold-search t)
              (inhibit-read-only t))
          (with-syntax-table emacs-lisp-mode-syntax-table
	    ;; unwind-protect is out, from Stefan
          ;; The following should probably be abstracted out.
              (progn
                ;; ;; Info references
                ;; (save-excursion
                ;;   (while (re-search-forward help-xref-info-regexp nil t)
                ;;     (let ((data (match-string 2)))
                ;;       (save-match-data
                ;;         (unless (string-match "^([^)]+)" data)
                ;;           (setq data (concat "(emacs)" data)))
                ;;      (setq data ;; possible newlines if para filled
                ;;            (replace-regexp-in-string "[ \t\n]+" " " data t t)))
                ;;       (help-xref-button 2 'help-info data))))
                ;; ;; URLs
                ;; (save-excursion
                ;;   (while (re-search-forward help-xref-url-regexp nil t)
                ;;     (let ((data (match-string 1)))
                ;;       (help-xref-button 1 'help-url data))))
                ;; ;; Mule related keywords.  Do this before trying
                ;; ;; `help-xref-symbol-regexp' because some of Mule
                ;; ;; keywords have variable or function definitions.
                ;; (if help-xref-mule-regexp
                ;;     (save-excursion
                ;;       (while (re-search-forward help-xref-mule-regexp nil t)
                ;;         (let* ((data (match-string 7))
                ;;                (sym (intern-soft data)))
                ;;           (cond
                ;;            ((match-string 3) ; coding system
                ;;             (and sym (coding-system-p sym)
                ;;                  (help-xref-button 6 'help-coding-system sym)))
                ;;            ((match-string 4) ; input method
                ;;             (and (assoc data input-method-alist)
                ;;                  (help-xref-button 7 'help-input-method data)))
                ;;            ((or (match-string 5) (match-string 6)) ; charset
                ;;             (and sym (charsetp sym)
                ;;                  (help-xref-button 7 'help-character-set sym)))
                ;;            ((assoc data input-method-alist)
                ;;             (help-xref-button 7 'help-input-method data))
                ;;            ((and sym (coding-system-p sym))
                ;;             (help-xref-button 7 'help-coding-system sym))
                ;;            ((and sym (charsetp sym))
                ;;             (help-xref-button 7 'help-character-set sym)))))))

                ;; Quoted symbols
                (save-excursion
                  (while (re-search-forward inform-xref-symbol-regexp nil t)
                    (let* ((data (match-string 8))
                           (sym (intern-soft data)))
                      (if sym
                          (cond
                           ((match-string 3) ; `variable' &c
                            (and (or (boundp sym) ; `variable' doesn't ensure
                                        ; it's actually bound
                                     (get sym 'variable-documentation))
                                 (inform-xref-button 8 'inform-variable sym)))
                           ((match-string 4) ; `function' &c
                            (and (fboundp sym) ; similarly
                                 (inform-xref-button 8 'inform-function sym)))
                           ((match-string 5) ; `face'
                            (and (facep sym)
                                 (inform-xref-button 8 'inform-face sym)))
                           ((match-string 6)) ; nothing for `symbol'
                           ((match-string 7)
                            (inform-xref-button 8 'inform-function-def sym))
                           ((cl-some (lambda (x) (funcall (nth 1 x) sym))
                                     describe-symbol-backends)
                            (inform-xref-button 8 'inform-symbol sym)))))))
                ;; An obvious case of a key substitution:
                ;; (save-excursion
                ;;   (while (re-search-forward
                ;;           ;; Assume command name is only word and symbol
                ;;           ;; characters to get things like `use M-x foo->bar'.
                ;;           ;; Command required to end with word constituent
                ;;           ;; to avoid `.' at end of a sentence.
                ;;           "\\<M-x\\s-+\\(\\sw\\(\\sw\\|\\s_\\)*\\sw\\)" nil t)
                ;;     (let ((sym (intern-soft (match-string 1))))
                ;;       (if (fboundp sym)
                ;;           (help-xref-button 1 'help-function sym)))))
                ;; ;; Look for commands in whole keymap substitutions:
                ;; (save-excursion
                ;;   ;; Make sure to find the first keymap.
                ;;   (goto-char (point-min))
                ;;   ;; Find a header and the column at which the command
                ;;   ;; name will be found.

                ;;   ;; If the keymap substitution isn't the last thing in
                ;;   ;; the doc string, and if there is anything on the same
                ;;   ;; line after it, this code won't recognize the end of it.
                ;;   (while (re-search-forward "^key +binding\n\\(-+ +\\)-+\n\n"
                ;;                             nil t)
                ;;     (let ((col (- (match-end 1) (match-beginning 1))))
                ;;       (while
                ;;           (and (not (eobp))
                ;;                ;; Stop at a pair of blank lines.
                ;;                (not (looking-at-p "\n\\s-*\n")))
                ;;         ;; Skip a single blank line.
                ;;         (and (eolp) (forward-line))
                ;;         (end-of-line)
                ;;         (skip-chars-backward "^ \t\n")
                ;;         (if (and (>= (current-column) col)
                ;;                  (looking-at "\\(\\sw\\|\\s_\\)+$"))
                ;;             (let ((sym (intern-soft (match-string 0))))
                ;;               (if (fboundp sym)
                ;;                   (help-xref-button 0 'help-function sym))))
                ;;         (forward-line))))))
              ))
          ;; Delete extraneous newlines at the end of the docstring
          ;; (goto-char (point-max))
          ;; (while (and (not (bobp)) (bolp))
          ;;   (delete-char -1))
          ;; (insert "\n")
          ;; (when (or help-xref-stack help-xref-forward-stack)
          ;;   (insert "\n"))
          ;; ;; Make a back-reference in this buffer if appropriate.
          ;; (when help-xref-stack
          ;;   (help-insert-xref-button help-back-label 'help-back
          ;;                            (current-buffer)))
          ;; ;; Make a forward-reference in this buffer if appropriate.
          ;; (when help-xref-forward-stack
          ;;   (when help-xref-stack
          ;;     (insert "\t"))
          ;;   (help-insert-xref-button help-forward-label 'help-forward
          ;;                            (current-buffer)))
          ;; (when (or help-xref-stack help-xref-forward-stack)
          ;;   (insert "\n")))
          )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'inform)
;;; inform.el ends here

;; Local Variables:
;; mode: outline-minor
;; indicate-empty-lines: t
;; show-trailing-whitespace: t
;; word-wrap: t
;; time-stamp-active: t
;; time-stamp-format: "%:y-%02m-%02d"
;; End:

