;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 20))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
(setq doom-theme 'catppuccin)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; (set-eglot-client! 'lua-mode `(,(concat "" "lua-language-server")))



(use-package! elcord
  :config
  (elcord-mode))

(map!
 :after centaur-tabs
 :nv "TAB"   #'centaur-tabs-forward
 :nv [backtab] #'centaur-tabs-backward
 :nv "S-TAB"   #'centaur-tabs-backward)


(use-package! lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable t))
(add-to-list 'company-backends 'company-glsl)
;; (use-package! org-bullets
;;   :after org
;; :hook (org-mode . org-bullets-mode))
(after! org
  ;; (add-hook! 'org-mode-hook #'org-modern-mode)
  ;; (add-hook! 'org-agenda-finalize-hook #'org-modern-agenda)
  (setq org-hide-emphasis-markers t
        org-link-descriptive t
        org-pretty-entities t
        org-hidden-keywords nil

        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t

        org-agenda-tags-column 0
        ))
(use-package! org-ref)
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t))
(use-package! org-roam-ui)
(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))

(use-package! ob-typescript
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((typescript . t)
     )))
;; (load! "mode-line")

;; (map!
;;  :after org
;;  :map org-mode
;;  :leader
;;  :prefix "t"
;;  :prefix ("o" . "org")
;;  :n "b" #'org-bullets-mode)
