;;; config/tools/eglot.el -*- lexical-binding: t; -*-

(defun eglot-add-server (s)
  (add-to-list 'eglot-server-programs s)
  (message "Added %s to eglot-server-programs" s)
  (eglot-ensure))

(use-package! eglot
  :config
  (eglot-booster-mode)
  ;; (setq completion-category-overrides '((eglot (styles orderless))
  ;;                                       (eglot-capf (styles orderless))))
  ;; (setq eglot-events-buffer-config '(:size 200 :format full))
  ;; (fset #'jsonrpc--log-event #'ignore)
  )

;;; config/tools/eglot.el ends here
