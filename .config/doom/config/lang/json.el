;;; config/lang/json.el -*- lexical-binding: t; -*-

;; Typescript
(use-package! json-ts-mode
  :mode "\\.json\\'"
  :init
  (add-hook! json-ts-mode
    (setq json-ts-mode-indent-offset 4)))

;;; config/lang/json.el ends here
