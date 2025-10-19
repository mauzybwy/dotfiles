;;; mauzy-eglot.el --- Terminal utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for managing eglot.

;;; Code:

(defvar mauzy/eglot-log-io nil
  "When non-nil, Eglot will log all LSP I/O messages in full.")

(defun mauzy/eglot-toggle-logging ()
  "Toggle detailed Eglot LSP I/O logging on and off.
When enabled, sets `eglot-log-io' to t and `eglot-events-buffer-size' to 0
to show all LSP messages in the *eglot-events* buffer."
  (interactive)
  (if mauzy/eglot-log-io
      (progn
        (setq mauzy/eglot-log-io nil)
        (setq eglot-events-buffer-size 0) ;; default truncation
        (setq eglot-events-buffer-config '(:size 0 :format short))
        (message "🔇 Eglot logging disabled"))
    (setq mauzy/eglot-log-io t)
    (setq eglot-events-buffer-size 20000)
    (setq eglot-events-buffer-config '(:size 20000 :format full))
    (message "🪵 Eglot logging enabled — use M-x eglot-events-buffer to view")))

(provide 'mauzy-eglot)
;;; mauzy-eglot.el ends here
