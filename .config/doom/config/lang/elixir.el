;;; config/lang/elixir.el -*- lexical-binding: t; -*-

(use-package! elixir-ts-mode
  :init
  (add-hook! elixir-ts-mode
    (eglot-add-server
     '(elixir-ts-mode "elixir-ls"))))

;;; config/lang/elixir.el ends here
