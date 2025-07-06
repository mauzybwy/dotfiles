;;; config/tools/formatters.el -*- lexical-binding: t; -*-

(use-package! apheleia
  :config
  (mauzy/add-to-list-multiple
   'apheleia-mode-alist
   '((rjsx-mode . prettier-typescript))))

;;; config/tools/formatters.el ends here
