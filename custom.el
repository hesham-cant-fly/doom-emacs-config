(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e2cf054069b44021a7c8f4c8561233443472d74692824c49aa5fecb9937b2553" default))
 '(doom-modeline-height 45)
 '(elcord-mode-text-alist
   '((agda-mode . "Agda") (assembly-mode . "Assembly") (bqn-mode . "BQN")
     (c-mode . "C  ") (c++-mode . "C++") (csharp-mode . "C#")
     (cperl-mode . "Perl") (elixir-mode . "Elixir") (enh-ruby-mode . "Ruby")
     (erlang-mode . "Erlang") (fsharp-mode . "F#") (gdscript-mode . "GDScript")
     (hy-mode . "Hy") (java-mode . "Java") (julia-mode . "Julia")
     (lisp-mode . "Common Lisp") (markdown-mode . "Markdown")
     (magit-mode . "It's Magit!") ("mhtml-mode" . "HTML") (nasm-mode . "NASM")
     (nim-mode . "Nim") (ocaml-mode . "OCaml") (pascal-mode . "Pascal")
     (prolog-mode . "Prolog") (puml-mode . "UML") (scala-mode . "Scala")
     (sh-mode . "Shell") (slime-repl-mode . "SLIME-REPL")
     (sly-mrepl-mode . "Sly-REPL") (solidity-mode . "Solidity")
     (terraform-mode . "Terraform") (typescript-mode . "Typescript")
     (php-mode "PHP") (odin-mode "Odin")))
 '(package-selected-packages
   '(vimrc-mode command-log-mode zig-ts-mode visual-fill-column gruber-darker-theme
     all-the-icons sublimity sublime-themes nimbus-theme))
 '(safe-local-variable-values
   '((dap-register-debug-template "C GDB Debug"
      (list :type "gdb" :request "launch" :name "C Debug" :program
       "${workspaceFolder}/build/jush" :cwd "${workspaceFolder}"))
     (require 'dap-gdb)))
 '(warning-suppress-types
   '((rustic-mode-local-vars-hook) (defvaralias) (lexical-binding))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'projectile-ripgrep 'disabled nil)
(put 'customize-group 'disabled nil)
