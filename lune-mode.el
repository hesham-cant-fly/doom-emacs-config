;; lune-mode.el - Major mode for editing Lune (Lua superset) code

(require 'lua-mode)

(defvar lune-keywords
  '("global" "var" "const" "auto" "as" "class"))

(defvar lune-type-keywords
  '("number" "string" "bool" "any"))

(defvar lune-font-lock-keywords
  `((,(regexp-opt lune-keywords 'words) . font-lock-keyword-face)
    (,(concat ":\\s-*\\(" (regexp-opt lune-type-keywords) "\\)\\>") 1 font-lock-type-face)))

;; Define the derived mode
(define-derived-mode lune-mode lua-mode "Lune"
  "Major mode for editing Lune code (superset of Lua).
Supports type annotations and additional keywords."
  ;; Add Lune-specific font-lock keywords
  (font-lock-add-keywords nil lune-font-lock-keywords)
  (with-eval-after-load 'lua-mode
    (defun lua-flymake-luacheck-init ()
      (unless (eq major-mode 'lune-mode)
        (lua-make-process "luacheck" "luacheck")))))

;; ;; Disable luacheck for Lune files
;; (when (fboundp 'lua-flymake-luacheck-init)
;;   (advice-add 'lua-flymake-luacheck-init :around
;;               (lambda (orig-fun &rest args)
;;                 (unless (eq major-mode 'lune-mode)
;;                   (apply orig-fun args))))))

;; Add file extension association
(add-to-list 'auto-mode-alist '("\\.lune\\'" . lune-mode))

(provide 'lune-mode)
