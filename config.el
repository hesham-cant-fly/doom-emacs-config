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
;; (setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 24))
;; (setq doom-font (font-spec :family "GohuFont uni14 Nerd Font Mono" :size 24))
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 20))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq catppuccin-flavor 'mocha)
;; (setq doom-theme 'doom-solarized-dark)
(setq doom-theme 'doom-one)

(add-to-list 'default-frame-alist '(alpha-background . 100))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

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

;; (use-package! sublimity
;;   :config
;;   (require 'sublimity)
;;   (require 'sublimity-scroll)
;;   (require 'sublimity-attractive)
;;   (sublimity-mode 1))

(load! "./algo-mode.el")
(load! "./haste-mode.el")
(load! "./lune-mode.el")
(require 'algo-mode)
(require 'haste-mode)
(require 'lune-mode)

(use-package! elcord
  :config
  (elcord-mode))

;; (map!
;;  :after centaur-tabs
;;  :nv "TAB"   #'centaur-tabs-forward
;;  :nv [backtab] #'centaur-tabs-backward
;;  :nv "S-TAB"   #'centaur-tabs-backward)
(use-package! lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable t))
(add-to-list 'company-backends 'company-glsl)
(use-package! org-roam-ui)
(after! org-roam
  (setq org-directory (concat (getenv "HOME") "/Documents/org/roam/"))
  (setq org-roam-directory (file-truename org-directory))
  (setq org-roam-capture-templates
        '(
          ("d" "Default" plain
           (file "~/Documents/org/roam/Templates/Default.org")
           :if-new
           (file+head "${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("s" "Source" plain
           (file "~/Documents/org/roam/Templates/Source.org")
           :if-new
           (file+head "${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("l" "Linux" plain
           (file "~/Documents/org/roam/Templates/Default.org")
           :if-new
           (file+head "Linux/${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("p" "Projects" plain
           (file "~/Documents/org/roam/Templates/Default.org")
           :if-new
           (file+head "Projects/${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("i" "Index" plain
           (file "~/Documents/org/roam/Templates/Index.org")
           :if-new
           (file+head "Index/${slug}.org" "#+title: ${title}\n")
           :unnarrowed t))))

(after! org
  ;; (add-hook! 'org-mode-hook #'org-modern-mode)
  ;; (add-hook! 'org-agenda-finalize-hook #'org-modern-agenda)
  (add-hook! 'org-mode-hook #'visual-line-mode)
  (add-hook! 'org-mode-hook #'visual-fill-column-mode)
  (setq org-startup-with-inline-images t)
  (setq org-ellipsis "  [MORE]"
        org-hide-emphasis-markers t
        org-link-descriptive t
        org-pretty-entities t
        org-hidden-keywords nil

        org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t

        org-agenda-tags-column 0
        org-startup-folded 'content)
  ;; Font Display
  (setq org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)


  ;; Visual Elements
  (custom-set-faces!
    '(org-document-title :height 2.1 :weight bold)
    '(org-level-1 :foreground "#00b6ef" :height 1.5 :weight bold :slant normal)
    '(org-level-2 :foreground "#26fb91" :height 1.3 :weight bold :slant normal)
    '(org-level-3 :foreground "#fb8a26" :height 1.15 :weight bold :slant normal)
    `(org-code :background ,(doom-darken 'bg 0.1))
    `(org-quote :background ,(doom-darken 'bg 0.1))))

(after! org
  (add-hook 'org-mode-hook (lambda ()
                             (setq display-line-numbers nil)))
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t)
  (org-appear-inside-latex t))

                                        ; (after! lsp-mode
;; (add-hook! lsp-mode #'lsp-inlay-hints-mode)

;; (require 'dap-lldb)
(require 'dap-gdb)
;; (require 'dap-cpptools)
(after! dap-mode
  (dap-ui-mode)
  (dap-tooltip-mode)
  (tooltip-mode)
  (dap-ui-controls-mode 1))
;; (dap-auto-configure-mode)
;; (dap-mode)

;; (require 'lldb)
;; (require 'dap-lldb)
;; (require 'dap-gdb)
;; (require 'dap-cpptools))

;; (after! dap-gdb
;;   (setq dap-gdb-debug-program '("gdb" "-i" "dap"))

;;   ;; C configuration
;;   (dap-register-debug-template
;;    "C GDB Debug"
;;    (list :type "gdb"
;;          :request "launch"
;;          :name "C Debug"
;;          :program "${workspaceFolder}/<your-program>"
;;          ;; :args '()
;;          :cwd "${workspaceFolder}")))

(after! visual-fill-column
  (setq-default visual-fill-column-width 120
                visual-fill-column-center-text t
                visual-fill-column-finges-outside-margins t))


(use-package! odin-mode
  :mode ("\\.odin\\'" . odin-mode))

(with-eval-after-load 'lsp-mode
  (setq-default lsp-auto-guess-root t) ;; Helps find the ols.json file with Projectile or project.el
  (add-to-list 'lsp-language-id-configuration '(odin-mode . "ols"))
  ;; (add-to-list 'lsp-language-id-configuration '(d-mode . "d"))
  ;; (add-hook! 'zig-mode (lambda ()
  ;;                        (interactive)
  ;;                        (setq lsp-inlay-hint-enable t)))

  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "dcd-client")
  ;;                   :major-modes '(d-mode)
  ;;                   :server-id 'dcd
  ;;                   :multi-root t))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "ols") ;; Adjust the path here
                    :major-modes '(odin-mode)
                    :server-id 'ols)))
;; :multi-root t))) ;; Ensures lsp-mode sends "workspaceFolders" to the server

(add-hook 'odin-mode-hook #'lsp)
(add-hook 'odin-ts-mode-hook #'lsp)

(after! doom
  (blink-cursor-mode))

(use-package! d-mode
  :config
  (add-hook! d-mode #'lsp-deferred))


(setq-default indent-tabs-mode nil
              tab-width 2
              evil-shift-width 2)

(after! projectile
  (add-to-list 'projectile-globally-ignored-directories "*node_modules" "^.zig-cache$"))

(use-package restclient
  :mode (".rest" . restclient-mode))
(after! restclient
  (map! :map restclient-mode-map
        :localleader
        "s" #'restclient-http-send-current
        "r" #'restclient-http-send-last))

(after! dired
  (put 'dired-find-alternate-file 'disabled nil))

;; (use-package! whitespace
;;   :config
;;   (setq
;;    whitespace-style '(face tabs tab-mark spaces space-mark trailing newline newline-mark)
;;    whitespace-display-mappings '(
;;                                  (space-mark   ?\     [?\u00B7]     [?.])
;;                                  (space-mark   ?\xA0  [?\u00A4]     [?_])
;;                                  (newline-mark ?\n    [182 ?\n])
;;                                  (tab-mark     ?\t    [?\u00BB ?\t] [?\ ?\t])))
;;   (global-whitespace-mode +1))

;; (use-package! whitespace
;;   :config
;;   (global-whitespace-mode +1))

(add-hook 'compilation-mode-hook (lambda () (setq truncate-lines nil)))
