;;; config/lang/elixir.el -*- lexical-binding: t; -*-

(use-package! elixir-ts-mode
  :init
  (add-hook! elixir-ts-mode
             #'lsp
             (subword-mode 1)
             )

  :config
  (after! lsp-mode
    (setq lsp-elixir-server-command '("elixir-ls")
          lsp-elixir-suggest-specs nil)))

(use-package! heex-ts-mode
  :init
  (add-hook! elixir-ts-mode #'lsp))

(use-package! exunit
  :after elixir-ts-mode
  :init
  (map! :after elixir-ts-mode
        :localleader
        :map elixir-ts-mode-map
        :prefix ("t" . "ExUnit")
        "." #'exunit-verify-single
        "s" #'exunit-verify-single
        "f" #'exunit-verify ;; run all tests in the current file
        "p" #'exunit-verify-all ;; run all tests in the project
        "r" #'exunit-rerun
        "T" #'exunit-toggle-file-and-test
        "t" #'exunit-toggle-file-and-test-other-window))

;;; config/lang/elixir.el ends here
