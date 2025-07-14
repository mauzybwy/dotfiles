;;; config/lang/jq.el -*- lexical-binding: t; -*-

(use-package! jq-ts-mode
  :mode "\\.jq\\'"
  :init
  (add-hook! jq-ts-mode
    (mauzy/eglot-add-server
     '(jq-ts-mode "jq-lsp"))))

;;; config/lang/jq.el ends here
