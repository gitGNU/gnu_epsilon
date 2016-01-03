;;; epsilon.el --- GNU epsilon editing mode

;; Copyright (C) 1986-1988, 1997-1998, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 2013  Luca Saiu
;; Updated in 2015 and 2016 by Luca Saiu

;; Adapted from scheme.el, within the GNU Emacs distribution, by Luca Saiu.

;; Original code in scheme.el by Bill Rozas <jinx@martigny.ai.mit.edu> and
;; Dave Love <d.love@dl.ac.uk>

;; Keywords: languages, epsilon, lisp

;; GNU epsilon is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU epsilon is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU epsilon.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; As a first step, I will only support indenting and font locking, to
;; make epsilon coding a little more comfortable.

;; Later I'm going to implement communication with the interpreter, in
;; a way similar to what Guile does.

;; The epsilon interpreter should also provide updates about new syntactic
;; forms.  This would make font locking much more precise.

;;; Code:

(require 'lisp-mode)

(defvar epsilon-mode-syntax-table
  ;;; FIXME: I can probably simplify this for epsilon.  I haven't
  ;;; touched it for the time being.  --Luca Saiu
  (let ((st (make-syntax-table))
	(i 0))
    ;; Symbol constituents
    ;; We used to treat chars 128-256 as symbol-constituent, but they
    ;; should be valid word constituents (Bug#8843).  Note that valid
    ;; identifier characters are epsilon-implementation dependent.
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{ "(}  " st)
    (modify-syntax-entry ?} "){  " st)
    ;; (modify-syntax-entry ?\| "\" 23bn" st) ;; this is from Scheme, but I don't want it in epsilon
    ;; Guile allows #! ... !# comments.
    ;; But SRFI-22 defines the comment as #!...\n instead.
    ;; Also Guile says that the !# should be on a line of its own.
    ;; It's too difficult to get it right, for too little benefit.
    ;; (modify-syntax-entry ?! "_ 2" st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  " st)
    (modify-syntax-entry ?\) ")(  " st)
    ;; It's used for single-line comments as well as for #;(...) sexp-comments.
    (modify-syntax-entry ?\; "< 2 " st)
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)

    ;; Special characters
    (modify-syntax-entry ?, "'   " st)
    (modify-syntax-entry ?@ "'   " st)
    (modify-syntax-entry ?# "' 14" st)
    (modify-syntax-entry ?\\ "\\   " st)
    st))

(defvar epsilon-mode-abbrev-table nil)
(define-abbrev-table 'epsilon-mode-abbrev-table ())

;; (defvar epsilon-imenu-generic-expression
;;       '((nil
;; 	 "^(define\\(\\|-\\(generic\\(\\|-procedure\\)\\|method\\)\\)*\\s-+(?\\(\\sw+\\)" 4)
;; 	("Types"
;; 	 "^(define-class\\s-+(?\\(\\sw+\\)" 1)
;; 	("Macros"
;; 	 "^(\\(defmacro\\|define-macro\\|define-syntax\\)\\s-+(?\\(\\sw+\\)" 2))
;;   "Imenu generic expression for epsilon mode.  See `imenu-generic-expression'.")

(defun epsilon-mode-variables ()
  (set-syntax-table epsilon-mode-syntax-table)
  (setq local-abbrev-table epsilon-mode-abbrev-table)

  ;; I took the following line from Scheme mode, but it causes
  ;; infinite loops with Emacs bzr as of 2013-11-08.  Or at least I
  ;; thought it did; I can't reproduce the thing any more.  Damn. --L.S.
  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter)) ;; !!!!!!

  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'fill-paragraph-function) 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (set (make-local-variable 'adaptive-fill-mode) nil)
  (set (make-local-variable 'indent-line-function) 'lisp-indent-line)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'outline-regexp) ";;; \\|(....")
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-add) 1)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (set (make-local-variable 'font-lock-comment-start-skip) ";+ *")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'lisp-indent-function) 'epsilon-indent-function)
  (setq mode-line-process '("" epsilon-mode-line-process))
  (set (make-local-variable 'imenu-case-fold-search) t)
  ;;(setq imenu-generic-expression epsilon-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
	'(("+-*/.<>=?!$%_&~^:" . "w")))
  (set (make-local-variable 'font-lock-defaults)
       '(;; (epsilon-font-lock-keywords
         ;;  epsilon-font-lock-keywords-1 epsilon-font-lock-keywords-2)
         (epsilon-font-lock-keywords epsilon-font-lock-keywords)
         nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
         beginning-of-defun
         (font-lock-mark-block-function . mark-defun)
         (font-lock-syntactic-face-function
          . epsilon-font-lock-syntactic-face-function)
         (parse-sexp-lookup-properties . t)
         (font-lock-extra-managed-props syntax-table)))
  (set (make-local-variable 'lisp-doc-string-elt-property)
       'epsilon-doc-string-elt))

(defvar epsilon-mode-line-process "")

(defvar epsilon-mode-map
  (let ((smap (make-sparse-keymap))
	(map (make-sparse-keymap "epsilon")))
    (set-keymap-parent smap lisp-mode-shared-map)
    ;;(define-key smap [menu-bar epsilon] (cons "epsilon" map))
    ;;(define-key map [run-epsilon] '("Run Inferior epsilon" . run-epsilon))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)
    smap)
  "Keymap for epsilon mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

;; Used by cmuepsilon
(defun epsilon-mode-commands (map)
  ;;(define-key map "\t" 'indent-for-tab-command) ; default
  ;; (define-key map "\177" 'backward-delete-char-untabify) ;; FIXME: tentatively commented-out by Luca Saiu.
  (define-key map "\e\C-q" 'indent-sexp))

;;;###autoload
(define-derived-mode epsilon-mode prog-mode "ε₁" ;; "epsilon"
  "Major mode for editing epsilon code.
Editing commands are similar to those of `lisp-mode'.

[FIXME: this is not implemented yet.  In addition, if an inferior
epsilon process is running, some additional commands will be
defined, for evaluating expressions and controlling the
interpreter, and the state of the process will be displayed in
the modeline of all epsilon buffers.]

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{epsilon-mode-map}
Entry to this mode calls the value of `epsilon-mode-hook'
if that value is non-nil."
  (epsilon-mode-variables))

(defgroup epsilon nil
  "Editing epsilon code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'lisp)

(defcustom epsilon-mode-hook nil
  "Normal hook run when entering `epsilon-mode'.
See `run-hooks'."
  :type 'hook
  :group 'epsilon)

;; This is shared by cmuscheme and xscheme.
;; (defcustom epsilon-program-name "epsilon"
;;   "*Program invoked by the `run-epsilon' command."
;;   :type 'string
;;   :group 'epsilon)

(defconst epsilon-whitespace "\\(?:[ \t\r\n]+\\)")
(defconst epsilon-optional-whitespace (concat "\\(?:"
                                              epsilon-whitespace
                                              "?\\)"))
;(defconst epsilon-whitespace-or-open-par (concat "\\(?:"
;                                                 epsilon-whitespace
;                                                 "\\|(\\)"))
(defconst epsilon-whitespace-or-par (concat "\\(?:"
                                            epsilon-whitespace
                                            "\\|[()]\\)"))
(defconst epsilon-separator (concat "\\(?:"
                                    epsilon-whitespace
                                    "\\|"
                                    "[().]"
                                    "\\)"))

(defconst epsilon-natural "\\(?:[0-9]+\\)")
(defconst epsilon-integer (concat "\\(?:"
                                  "[+-]?"
                                  epsilon-natural
                                  "\\)"))
(defconst epsilon-non-integer (concat "\\(?:"
                                      "\\(?:"
                                      epsilon-integer
                                      "\\."
                                      epsilon-natural "?"
                                      "\\|"
                                      epsilon-integer "?"
                                      "\\."
                                      epsilon-natural
                                      "\\)"
                                      "\\(?:" "[eE]" epsilon-integer "\\)" "?"
                                      "\\)"))
(defconst epsilon-number (concat "\\(?:"
                                 epsilon-integer
                                 "\\|"
                                 epsilon-non-integer
                                 "\\)"))

;;; Return a regexp matching the given regexp as the first element of
;;; a list, recognizing the given regexp as \1.
(defun epsilon-at-list-beginning (regexp)
  (concat "(" epsilon-optional-whitespace
          "\\("
          regexp
          "\\)"
          epsilon-whitespace-or-par))

;;; Like epsilon-at-list-beginning, but make a regexp recognizing the given regexp
;;; as \1 at the form name position, plus the defined entity as \2 and formal
;;; parameters, if any, as \3.
(defun epsilon-at-define-nonprocedure-beginning (regexp)
  (concat "(" epsilon-optional-whitespace
          "\\("
          regexp
          "\\)\\(?:[ \t\r\n]+\\)\\([^ ().\t\r\n]+\\)"
          epsilon-whitespace-or-par))
(defun epsilon-at-define-procedure-beginning (regexp)
  (concat "(" epsilon-optional-whitespace
          "\\("
          regexp
          "\\)"
          epsilon-optional-whitespace
          "(" epsilon-optional-whitespace
          "\\([^ ().\t\r\n]+\\)"
          "\\(" epsilon-whitespace "?" "[^)]*\\)"
          ")"
          ;;epsilon-whitespace-or-par
          ))

(defconst epsilon-font-lock-keywords
  (eval-when-compile
    `((,(epsilon-at-list-beginning
         (regexp-opt '("e0:variable" "e0:value" "e0:bundle" "e0:let"
                       "e0:call" "e0:call-indirect" "e0:primitive"
                       "e0:if-in" "e0:fork" "e0:join"
                       ;; Same forms (except for implicit sequences), with the e1 namespace:
                       "e1:variable" "e1:value" "e1:bundle" "e1:let"
                       "e1:call" "e1:call-indirect" "e1:primitive"
                       "e1:if-in" "e1:fork" "e1:join")))
       (1 font-lock-keyword-face))
      (,(epsilon-at-define-nonprocedure-beginning
         (regexp-opt '("e1:define"
                       "e1:define-non-procedure"
                       "e1:define-regexp")))
       (1 font-lock-keyword-face)
       (2 font-lock-variable-name-face))
      (,(epsilon-at-define-nonprocedure-beginning
         (regexp-opt '("e1:trivial-define-macro")))
       (1 font-lock-keyword-face)
       (2 font-lock-preprocessor-face))
      (,(epsilon-at-define-procedure-beginning
         (regexp-opt '("e1:define"
                       "e1:define-procedure"
                       "e1:define-with-keywords")))
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face)
       (3 font-lock-variable-name-face))
      (,(epsilon-at-define-procedure-beginning
         (regexp-opt '("e1:define-macro")))
       (1 font-lock-keyword-face)
       (2 font-lock-preprocessor-face)
       (3 font-lock-variable-name-face))
      ;;; Scheme keywords, for Guile definitions
      (,(epsilon-at-list-beginning
         (regexp-opt '("define" "define-macro" "e1:toplevel")))
       (1 font-lock-warning-face))
      (,(epsilon-at-list-beginning
         (regexp-opt '("e1:begin" "e1:begin1" "e1:begin2" "e1:begin3" "e1:begin-2" "e1:begin-3"
                       "e1:bundle"
                       "e1:match"
                       "e1:let" "e1:let*"
                       "e1:bundle" "e1:future" "e1:join"
                       "e1:exec" "e1:unexec"
                       "e1:and" "e1:or"
                       "e1:if" "e1:when" "e1:unless" "e1:cond" "e1:case" "else" "bind" "bind*"
                       "e1:nonclosure" "e1:call-nonclosure"
                       "e1:closure"
                       "e1:lambda" "e1:ml-lambda" "e1:call-closure"
                       "e1:while" "e1:do" "e1:doalist" "e1:dobuffer" "e1:dohash" "e1:dolist" "e1:dovector" "e1:dotimes" "e1:for"
                       )))
       (1 font-lock-keyword-face))
      (,(epsilon-at-define-nonprocedure-beginning
         (regexp-opt '("record:define"
                       "sum:define" "sum:define-open" "sum:extend-open"
                       "e1:define-record"
                       "e1:define-sum"
                       "e1:define-sum-open"
                       "e1:extend-sum"
                       "variadic:define-left-deep" "variadic:define-right-deep" "variadic:define-associative"
                       "e1:define-variadic-left-deep" "e1:define-variadic-right-deep" "e1:define-variadic-associative")))
       (1 font-lock-keyword-face)
       (2 font-lock-builtin-face))
      (,(epsilon-at-list-beginning
         (regexp-opt '("variadic:define-left-deep" "variadic:define-right-deep" "variadic:define-associative"
                       "e1:define-variadic-left-deep" "e1:define-variadic-right-deep" "e1:define-variadic-associative"
                       )))
       (1 font-lock-preprocessor-face))
      (,(epsilon-at-list-beginning (regexp-opt '("e1:not")))
       (1 font-lock-keyword-face;;font-lock-negation-char-face
          ))
      (,(concat "\\<\\(" epsilon-number "\\)\\>")
       (1 font-lock-constant-face))
      (,(concat "\\<\\(" "#[tf]" "\\)\\>")
       (1 font-lock-constant-face))
      ("\\(,@?\\|\`\\|'\\|\\)"
       (1 font-lock-keyword-face))
      (,(concat "\\<\\("
                ;;; FIXME: this should be a regexp matching all form names
                "e1:define" "\\|"
                "e1:define-procedure" "\\|"
                "e1:define-non-procedure" "\\|"
                "e1:define-macro"
                "\\)\\>")
       (1 font-lock-warning-face))
      ))
  "Expressions to highlight in epsilon mode.")

(defconst epsilon-sexp-comment-syntax-table
  (let ((st (make-syntax-table epsilon-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    ;;(modify-syntax-entry ?|  "|" st)
    st))

;;;; FIXME: Adapt and re-enable later if needed.  --Luca Saiu
;; (put 'lambda 'epsilon-doc-string-elt 2)
;; Docstring's pos in a `define' depends on whether it's a var or fun def.
;; (put 'define 'epsilon-doc-string-elt
;;      (lambda ()
;;        ;; The function is called with point right after "define".
;;        (forward-comment (point-max))
;;        (if (eq (char-after) ?\() 2 0)))

(defun epsilon-font-lock-syntactic-face-function (state)
  (when (and (null (nth 3 state))
             (eq (char-after (nth 8 state)) ?#)
             (eq (char-after (1+ (nth 8 state))) ?\;))
    ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
    (save-excursion
      (let ((pos (point))
            (end
             (condition-case err
                 (let ((parse-sexp-lookup-properties nil))
                   (goto-char (+ 2 (nth 8 state)))
                   ;; FIXME: this doesn't handle the case where the sexp
                   ;; itself contains a #; comment.
                   (forward-sexp 1)
                   (point))
               (scan-error (nth 2 err)))))
        (when (< pos (- end 2))
          (put-text-property pos (- end 2)
                             'syntax-table epsilon-sexp-comment-syntax-table))
        (put-text-property (- end 1) end 'syntax-table '(12)))))
  ;; Choose the face to use.
  (lisp-font-lock-syntactic-face-function state))

;;;###autoload

(defvar calculate-lisp-indent-last-sexp)


;; FIXME this duplicates almost all of lisp-indent-function.
;; Extract common code to a subroutine.
(defun epsilon-indent-function (indent-point state)
  "epsilon mode function for the value of the variable `lisp-indent-function'.
This behaves like the function `lisp-indent-function', except that:

i) it checks for a non-nil value of the property `epsilon-indent-function'
\(or the deprecated `epsilon-indent-hook'), rather than `lisp-indent-function'.

ii) if that property specifies a function, it is called with three
arguments (not two), the third argument being the default (i.e., current)
indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
					 calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (get (intern-soft function) 'epsilon-indent-function)
			 (get (intern-soft function) 'epsilon-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
                        ;;; Most epsilon1 forms should be indented à-la-defun:
                        (and (string-match "\\`e1:" function)
                             (not (string-match epsilon-e1-forms-to-indent-a-la-and function)))))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method state indent-point normal-indent)))))))


;;; Let is different in epsilon

(defun would-be-symbol (string)
  (not (string-equal (substring string 0 1) "(")))

(defun next-sexp-as-string ()
  ;; Assumes that it is protected by a save-excursion
  (forward-sexp 1)
  (let ((the-end (point)))
    (backward-sexp 1)
    (buffer-substring (point) the-end)))

;; This is correct but too slow.
;; The one below works almost always.
;;(defun epsilon-let-indent (state indent-point)
;;  (if (would-be-symbol (next-sexp-as-string))
;;      (epsilon-indent-specform 2 state indent-point)
;;      (epsilon-indent-specform 1 state indent-point)))

(defun epsilon-let-indent (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

;; (put 'begin 'epsilon-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'e1:define 'epsilon-indent-function 1)
(put 'e1:define-procedure 'epsilon-indent-function 1)
(put 'e1:define-non-procedure 'epsilon-indent-function 1)
(put 'e1:define-macro 'epsilon-indent-function 1)
(put 'e1:trivial-define-macro 'epsilon-indent-function 1)
(put 'e1:define-record 'epsilon-indent-function 1)
(put 'e1:define-sum 'epsilon-indent-function 1)
(put 'e1:define-sum-open 'epsilon-indent-function 1)
(put 'e1:extend-sum 'epsilon-indent-function 1)
(put 'e1:define-with-keywords 'epsilon-indent-function 1)
(put 'e1:define-keyword-syntax 'epsilon-indent-function 1)
(put 'e1:define-regexp 'epsilon-indent-function 1)

(put 'e1:begin 'epsilon-indent-function 0)
(put 'e1:begin1 'epsilon-indent-function 0)
(put 'e1:begin2 'epsilon-indent-function 0)
(put 'e1:begin3 'epsilon-indent-function 0)
(put 'e1:begin-2 'epsilon-indent-function 0)
(put 'e1:begin-3 'epsilon-indent-function 0)
(put 'e1:case 'epsilon-indent-function 1)
(put 'e1:match 'epsilon-indent-function 1)
(put 'e1:do 'epsilon-indent-function 2)
(put 'e1:dotimes 'epsilon-indent-function 1)
(put 'e1:while 'epsilon-indent-function 1)
(put 'e1:doalist 'epsilon-indent-function 1)
(put 'e1:dobuffer 'epsilon-indent-function 1)
(put 'e1:dohash 'epsilon-indent-function 1)
(put 'e1:dolist 'epsilon-indent-function 1)
(put 'e1:dovector 'epsilon-indent-function 1)
(put 'e1:for 'epsilon-indent-function 1)
(put 'e1:lambda 'epsilon-indent-function 1)
(put 'e1:ml-lambda 'epsilon-indent-function 1)
(put 'e1:call 'epsilon-indent-function 1)
(put 'e1:call-closure 'epsilon-indent-function 1)
(put 'e1:call-nonclosure 'epsilon-indent-function 1)
(put 'e1:let 'epsilon-indent-function 'epsilon-let-indent)
(put 'e1:let* 'epsilon-indent-function 'epsilon-let-indent)
(put 'e0:let 'epsilon-indent-function 2)
(put 'e0:bundle 'epsilon-indent-function 0)
(put 'e1:bundle 'epsilon-indent-function 0)
(put 'e0:if-in 'epsilon-indent-function 2)
(put 'e1:if 'epsilon-indent-function 1)
(put 'e1:when 'epsilon-indent-function 1)
(put 'e1:unless 'epsilon-indent-function 1)
(put 'e1:unexec 'epsilon-indent-function 1)
(put 'record:define 'epsilon-indent-function 1)
(put 'sum:define 'epsilon-indent-function 1)
(put 'sum:define-open 'epsilon-indent-function 1)
(put 'sum:extend-open 'epsilon-indent-function 1)

;; A regexp matching form names beginning with "e1:", but which have
;; to be indented lile "and", which is different from the default.
(defconst epsilon-e1-forms-to-indent-a-la-and
  (concat
   "\\<\\(?:"
   (regexp-opt '("e1:and" "e1:or" "e1:xor"
                 "e1:list" "e1:value-list"))
   "\\|"
   "\\(?:e1:[^ \t\n\r]+\*\\)" ;; epsilon1 expression builders
   "\\)\\>"))


(setq auto-mode-alist
  (append '(("\\.e$" . epsilon-mode)) auto-mode-alist))

(provide 'epsilon)

;;; epsilon.el ends here
