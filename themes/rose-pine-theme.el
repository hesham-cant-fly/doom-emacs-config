;;; rose-pine-theme.el --- A port of the Rose Pine theme for Doom Emacs -*- lexical-binding: t; -*-

;; Author: Claude
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (doom-themes "2.0.0"))
;; Keywords: faces, theme, rose-pine, doom
;; URL: https://github.com/yourusername/rose-pine-emacs

;;; Commentary:
;; A port of the Rose Pine theme optimized for Doom Emacs

;;; Code:
(require 'doom-themes)

;;
;;; Variables

(defgroup rose-pine-theme nil
  "Options for the `rose-pine' theme."
  :group 'doom-themes)

(defcustom rose-pine-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'rose-pine-theme
  :type 'boolean)

(defcustom rose-pine-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'rose-pine-theme
  :type 'boolean)

(defcustom rose-pine-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'rose-pine-theme
  :type '(choice integer boolean))

;;
;;; Theme definition

(def-doom-theme rose-pine
  "A dark theme inspired by Rose Pine"

  ;; name        default   256           16
  ((bg         '("#191724" "#191724"      "black"       ))
   (bg-alt     '("#1f1d2e" "#1f1d2e"      "black"       ))
   (base0      '("#26233a" "#26233a"      "black"       ))
   (base1      '("#21202e" "#21202e"      "brightblack" ))
   (base2      '("#403d52" "#403d52"      "brightblack" ))
   (base3      '("#524f67" "#524f67"      "brightblack" ))
   (base4      '("#6e6a86" "#6e6a86"      "brightblack" ))
   (base5      '("#908caa" "#908caa"      "brightblack" ))
   (base6      '("#e0def4" "#e0def4"      "brightblack" ))
   (base7      '("#e0def4" "#e0def4"      "brightblack" ))
   (base8      '("#e0def4" "#e0def4"      "white"       ))
   (fg         '("#e0def4" "#e0def4"      "brightwhite" ))
   (fg-alt     '("#908caa" "#908caa"      "white"       ))

   (grey       '("#6e6a86" "#6e6a86"      "brightblack" ))
   (red        '("#eb6f92" "#eb6f92"      "red"         ))
   (orange     '("#f6c177" "#f6c177"      "brightred"   ))
   (green      '("#31748f" "#31748f"      "green"       ))
   (teal       '("#9ccfd8" "#9ccfd8"      "brightgreen" ))
   (yellow     '("#f6c177" "#f6c177"      "yellow"      ))
   (blue       '("#31748f" "#31748f"      "brightblue"  ))
   (dark-blue  '("#31748f" "#31748f"      "blue"        ))
   (magenta    '("#c4a7e7" "#c4a7e7"      "brightmagenta"))
   (violet     '("#c4a7e7" "#c4a7e7"      "magenta"     ))
   (cyan       '("#9ccfd8" "#9ccfd8"      "brightcyan"  ))
   (dark-cyan  '("#9ccfd8" "#9ccfd8"      "cyan"        ))
   (pink       '("#ebbcba" "#ebbcba"      "magenta"     ))

   ;; face categories -- required for all themes
   (highlight      base3)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      base3)
   (builtin        teal)
   (comments       (if rose-pine-brighter-comments dark-cyan grey))
   (doc-comments   (doom-lighten (if rose-pine-brighter-comments dark-cyan grey) 0.25))
   (constants      orange)
   (functions      pink)
   (keywords       blue)
   (methods        pink)
   (operators      fg)
   (type           teal)
   (strings        yellow)
   (variables      fg)
   (numbers        orange)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright rose-pine-brighter-modeline)
   (-modeline-pad
    (when rose-pine-padded-modeline
      (if (integerp rose-pine-padded-modeline) rose-pine-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.05) ,@(cdr bg-alt))))

  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if rose-pine-brighter-comments (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background base3 :distant-foreground fg :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground violet)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))
   ;;;; org-mode
   (org-level-1 :foreground pink :weight 'bold :height 1.2)
   (org-level-2 :foreground teal :weight 'bold :height 1.1)
   (org-level-3 :foreground violet :weight 'bold)
   (org-level-4 :foreground yellow :weight 'bold)
   (org-level-5 :foreground blue :weight 'bold)
   (org-level-6 :foreground red :weight 'bold)
   (org-level-7 :foreground grey :weight 'bold)
   (org-level-8 :foreground fg-alt :weight 'bold)
   (org-todo :foreground red :weight 'bold)
   (org-done :foreground green :weight 'bold)
   (org-headline-done :foreground base5)
   (org-date :foreground teal :underline t)
   (org-code :foreground yellow)
   (org-verbatim :foreground blue)
   (org-special-keyword :foreground violet)
   (org-block :background base1 :extend t)
   (org-block-begin-line :foreground grey :background base1 :extend t)
   (org-block-end-line :foreground grey :background base1 :extend t)
   ;;;; magit
   (magit-section-heading :foreground violet :weight 'bold)
   (magit-section-highlight :background base1)
   (magit-diff-added :foreground green :background base1)
   (magit-diff-added-highlight :foreground green :background base2)
   (magit-diff-removed :foreground red :background base1)
   (magit-diff-removed-highlight :foreground red :background base2)
   ;;;; web-mode
   (web-mode-html-tag-face :foreground red)
   (web-mode-html-attr-name-face :foreground orange)
   (web-mode-html-attr-value-face :foreground green)
   (web-mode-css-property-name-face :foreground orange)
   (web-mode-css-selector-face :foreground blue)
   ;;;; treemacs
   (treemacs-root-face :foreground violet :weight 'bold)
   (treemacs-directory-face :foreground fg)
   (treemacs-file-face :foreground fg)
   (treemacs-git-modified-face :foreground orange)
   (treemacs-git-added-face :foreground green)
   (treemacs-git-untracked-face :foreground pink)
   ;;;; lsp-mode
   (lsp-face-highlight-textual :background base3 :foreground fg)
   ;;;; company
   (company-tooltip :background bg-alt :foreground fg)
   (company-tooltip-selection :background base3)
   (company-tooltip-common :foreground pink)
   (company-tooltip-annotation :foreground teal :slant 'italic)
   (company-scrollbar-fg :background highlight)
   (company-scrollbar-bg :background base1)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground pink)
   (rainbow-delimiters-depth-2-face :foreground teal)
   (rainbow-delimiters-depth-3-face :foreground violet)
   (rainbow-delimiters-depth-4-face :foreground yellow)
   (rainbow-delimiters-depth-5-face :foreground blue)
   (rainbow-delimiters-depth-6-face :foreground orange)
   (rainbow-delimiters-depth-7-face :foreground green)
   (rainbow-delimiters-depth-8-face :foreground red)
   (rainbow-delimiters-depth-9-face :foreground grey)
   ;;;; which-key
   (which-key-key-face :foreground pink)
   (which-key-group-description-face :foreground violet)
   (which-key-command-description-face :foreground fg)
   ;;;; evil
   (evil-ex-lazy-highlight :background base3 :foreground fg)
   (evil-ex-substitute-matches :background red :foreground base0)
   (evil-ex-substitute-replacement :background green :foreground base0)
   ;;;; ivy
   (ivy-current-match :background base3 :foreground fg :weight 'bold)
   (ivy-minibuffer-match-face-1 :foreground violet :weight 'bold)
   (ivy-minibuffer-match-face-2 :foreground teal :weight 'bold)
   (ivy-minibuffer-match-face-3 :foreground blue :weight 'bold)
   (ivy-minibuffer-match-face-4 :foreground yellow :weight 'bold)

  ;; Generic Treesitter Faces
  (tree-sitter-hl-face:attribute :foreground orange)
  (tree-sitter-hl-face:constant :foreground orange)
  (tree-sitter-hl-face:constructor :foreground pink)
      (tree-sitter-hl-face:function :foreground pink)
      (tree-sitter-hl-face:function.call :foreground pink)
      (tree-sitter-hl-face:method :foreground teal)
      (tree-sitter-hl-face:method.call :foreground teal)
      (tree-sitter-hl-face:property :foreground fg)
      (tree-sitter-hl-face:punctuation :foreground base5)
      (tree-sitter-hl-face:punctuation.bracket :foreground base5)
      (tree-sitter-hl-face:punctuation.delimiter :foreground base5)
      (tree-sitter-hl-face:string :foreground yellow)
      (tree-sitter-hl-face:type :foreground teal)
      (tree-sitter-hl-face:type.parameter :foreground violet)
      (tree-sitter-hl-face:variable :foreground fg)
      (tree-sitter-hl-face:variable.parameter :foreground orange)

      ;; Language-specific Treesitter Faces
      (tree-sitter-hl-face:type.builtin.python :foreground teal)
      (tree-sitter-hl-face:method.call.python :foreground pink)
      (tree-sitter-hl-face:type.builtin.rust :foreground teal)
      (tree-sitter-hl-face:constructor.rust :foreground pink)
      (tree-sitter-hl-face:lifetime.rust :foreground orange)
      (tree-sitter-hl-face:type.builtin.javascript :foreground teal)
      (tree-sitter-hl-face:type.builtin.typescript :foreground teal)
      (tree-sitter-hl-face:method.call.javascript :foreground pink)
      (tree-sitter-hl-face:method.call.typescript :foreground pink)
      (tree-sitter-hl-face:type.builtin.go :foreground teal)
      (tree-sitter-hl-face:method.call.go :foreground pink)
      (tree-sitter-hl-face:type.builtin.c :foreground teal)
      (tree-sitter-hl-face:type.builtin.cpp :foreground teal)
      (tree-sitter-hl-face:function.macro.c :foreground violet)
      (tree-sitter-hl-face:function.macro.cpp :foreground violet))
  ;;;; Base theme variable overrides-
  ()
  )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'rose-pine)
(provide 'rose-pine-theme)

;;; rose-pine-theme.el ends here

