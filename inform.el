;;; inform.el --- Link symbols in Info buffers to their help documentation  -*- lexical-binding: t; -*-

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

;; This library provides links of symbols (functions, variables,
;; faces) within Emacs' Info viewer to their help documentation.  This
;; linking is done, when the symbol names in texinfo documentations
;; (like the Emacs- and Elisp manual) are `quoted-symbols' or
;; functions which are prefixed by M-x, for example "... use M-x
;; function-name ..." or "... use `M-x function-name' ...".  The
;; symbol names must be known to Emacs, i.e. their names are stored in
;; the variable `obarray'.

;; You can follow these additional links with the usual Info
;; keybindings.  The customisation variable
;; `mouse-1-click-follows-link' is influencing the clicking behaviour
;; (and the tooltips) of the links, the variable's default is 450
;; (milli seconds) setting it to nil means only clicking with mouse-2
;; is following the link (hint: Drew Adams).

;; The code uses mostly mechanisms from Emacs' lisp/help-mode.el file.

;;; Todo:

;; `M-x\nfunction-name' is not buttonised in an *info* buffer

;; Back / Forward button in help buffer - back to info buffer or
;; remain in help mode?

;; Twice clicking or RETurning removes *Help* buffer (idea: Drew Adams)

;; Different colour for different symbol types (idea Drew Adams) see
;; package helpful, info+ and info-colors on Melpa

;; Documentation strings are not yet adopted from help to inform!

;; Possibly useful help features are not yet explored and still
;; commented out

;; How to shorten URLs shortening?  But how to handle then the
;; indentation?

;;; Code:

(require 'info)				;redundant?
(require 'button)
(require 'cl-lib)
(require 'help-mode)			;redundant?

(defvar describe-symbol-backends) 	;from help-mode.el
(defvar help-xref-following)		;dito

;; activate inform without manually loading it.
;;;###autoload (require 'inform)

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


(defun inform-do-xref (_pos function args)
  "Call the help cross-reference function FUNCTION with args ARGS.
Things are set up properly so that the resulting `help-buffer' has
a proper [back] button."
  ;; There is a reference at point.  Follow it.
  (let ((help-xref-following nil))
    (apply
     function (if (eq function 'info)
		  (append args (list (generate-new-buffer-name "*info*")))args))))

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
  (interactive "b")
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

	    ;; M-x prefixed functions
	    (save-excursion
	      (while (re-search-forward
		      ;; Assume command name is only word and symbol
		      ;; characters to get things like `use M-x foo->bar'.
		      ;; Command required to end with word constituent
		      ;; to avoid `.' at end of a sentence.
		      ;; "\\<M-x\\s-+\\(\\sw\\(\\sw\\|\\s_\\)*\\sw\\)" nil t)
		      "['`‘]?M-x\\s-+\\(\\sw\\(\\sw\\|\\s_\\)*\\sw\\)['’]?" nil t)
		(let ((sym (intern-soft (match-string 1))))
		  (message "found %s" sym)
		  (if (fboundp sym)
		      (inform-xref-button 1 'inform-function sym)))))))))))

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

