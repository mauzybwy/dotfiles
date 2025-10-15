;;; config/tools/treesit.el -*- lexical-binding: t; -*-

(after! treesit
        (setq treesit-font-lock-level 4)
        (setq treesit-language-source-alist
              '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                (astro "https://github.com/mauzybwy/tree-sitter-astro" "emacs")
                (cmake "https://github.com/uyha/tree-sitter-cmake")
                (css "https://github.com/tree-sitter/tree-sitter-css")
                (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
                (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                (go "https://github.com/tree-sitter/tree-sitter-go")
                (html "https://github.com/tree-sitter/tree-sitter-html")
                (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                (json "https://github.com/tree-sitter/tree-sitter-json")
                (jq "https://github.com/nverno/tree-sitter-jq")
                (make "https://github.com/alemuller/tree-sitter-make")
                (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
                (python "https://github.com/tree-sitter/tree-sitter-python")
                (nix "https://github.com/nix-community/tree-sitter-nix")
                (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
                (toml "https://github.com/tree-sitter/tree-sitter-toml")
                (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
                (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
                (heex "https://github.com/phoenixframework/tree-sitter-heex")
                (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
                (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

;;; config/tools/treesit.el ends here
