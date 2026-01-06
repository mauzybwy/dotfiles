;;; mauzy-downcase-only-mode.el --- Terminal utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Minor mode to only allow downcase typing

;;; Code:

(define-minor-mode mauzy/downcase-only-mode
  "Force all letters to downcase."
  :lighter " lc"
  (if mauzy/downcase-only-mode
      (progn
        (add-hook 'after-change-functions #'mauzy/downcase-only--after-change nil t)
        (add-hook 'before-save-hook #'mauzy/downcase-only--before-save nil t))
    (remove-hook 'after-change-functions #'mauzy/downcase-only--after-change t)
    (remove-hook 'before-save-hook #'mauzy/downcase-only--before-save t)))

(defvar-local mauzy/downcase-only--inhibit nil)

(defun mauzy/downcase-only--after-change (beg end _len)
  "Downcase any uppercase letters just inserted."
  (unless mauzy/downcase-only--inhibit
    (let ((mauzy/downcase-only--inhibit t))
      (save-excursion
        (downcase-region beg end)))))

(defun mauzy/downcase-only--before-save ()
  "Downcase the entire buffer before saving."
  (downcase-region (point-min) (point-max)))

(provide 'mauzy-downcase-only-mode)
;;; mauzy-downcase-only-mode.el ends here
