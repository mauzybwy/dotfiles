;;; config/lang/ruby.el -*- lexical-binding: t; -*-

(use-package! ruby-ts-mode
  :init
  (add-hook!
   ruby-ts-mode
   #'lsp
   (setq format-all-formatters
         '(("Ruby" (rubocop))))
   (format-all-mode)
   (minitest-mode)
   (apheleia-mode -1)))

(use-package! minitest
  :config
  (setq minitest--test-regexps
        '("\\(test\\) ['\"]\\([^\"]+?\\)['\"]"
          "\\(test\\)(['\"]\\([^\"]+?\\)['\"])"
          "def \\(test\\)_\\([_A-Za-z0-9]+\\)"
          "\\(it\\) \"\\([^\"]+?\\)\""
          "\\(it\\) '\\([^\"]+?\\)'")))

;;; config/lang/ruby.el ends here
