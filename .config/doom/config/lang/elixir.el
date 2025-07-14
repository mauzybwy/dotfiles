;;; config/lang/elixir.el -*- lexical-binding: t; -*-

(use-package! elixir-ts-mode
  :init
  (add-hook! elixir-ts-mode
    (mauzy/eglot-add-server
     '(elixir-ts-mode "elixir-ls"))))

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
        "t" #'exunit-toggle-file-and-test-other-window)
  )

;;; config/lang/elixir.el ends here
