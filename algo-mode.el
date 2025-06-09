;;; package --- algo mode for emacs
(require 'cl-lib)

;; Define syntax highlighting
(defvar algo-font-lock-keywords
  `(
    ;; Comments: // or /* ... */
    ("//.*" . font-lock-comment-face)
    ("/\\*[\\s\\S]*?\\*/" . font-lock-comment-face)
    ;; Keywords (customize these)
    (,(regexp-opt
       '("algorithme"
         "var" "variable" "variables"
         "fonction"
         "fin" "debut" "début"
         "si" "sinon" "sinonsi" "finsi"
         "dbg"
         "non" "et" "ou")
       'words) . font-lock-keyword-face)
    ;; Built-in types/functions
    (,(regexp-opt '("entier" "chaîne" "caractère" "booléen" "réel" "vrai" "faux")) . font-lock-builtin-face)
    ;; Function names: \bfunc\s+(\w+)
    ("\\bfonction\\s+\\(\\w+\\)" 1 font-lock-function-name-face)
    ;; Strings
    ("\".*?\"" . font-lock-string-face)
    ;; Numbers
    ("\\<-?[0-9]+\\.?[0-9]*\\>" . font-lock-constant-face)
    (,(regexp-opt
       '("<-" ":" "*" "/" "+" "-" "^" ":=" "=")
       'words) . font-lock-operator-face))
  "Syntax highlighting rules for Algo.")

;; Define indentation rules (adjust as needed)
(defvar algo-indent-offset 4
  "Indentation offset for Algo code.")

;; Syntax table
(defvar algo-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C/C++-style comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    table)
  "Syntax table for Algo mode.")

;; Define the major mode
(define-derived-mode algo-mode prog-mode "Algo"
  "Major mode for Algo programming language."
  :syntax-table algo-mode-syntax-table
  (setq-local font-lock-defaults '(algo-font-lock-keywords))
  (setq-local indent-line-function 'algo-indent-line)
  (setq-local comment-start "// ")
  (setq-local comment-end ""))

;; Indentation logic (simplified example)
(defun algo-indent-line ()
  "Indent current line according to Algo rules."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0) ; First line: no indent
    (let ((not-indented t) cur-indent)
      (save-excursion
        (while not-indented
          (forward-line -1)
          (if (looking-at "^\\s-*\\s<\\s-*end\\>")
              (setq cur-indent (- (current-indentation) algo-indent-offset))
            (if (looking-at "^\\s-*\\s<\\s-*\\begin\\>")
                (setq cur-indent (+ (current-indentation) algo-indent-offset))
              (setq cur-indent (current-indentation))))
          (setq not-indented nil)))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

;; Add to auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.algo\\'" . algo-mode))

(provide 'algo-mode)
;;; algo-mode.el ends here
