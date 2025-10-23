;;; mauzy-vertico.el --- Terminal utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for managing vertico.

;;; Code:

(defvar mauzy/vertico-transform-functions nil)

(defun mauzy/vertico-highlight-directory (file)
  "If FILE ends with a slash, highlight it as a directory."
  (if (string-suffix-p "/" file)
      ;;(propertize file 'face 'custom-face-tag)
      (propertize file 'face 'dired-directory)
    ;; (propertize file 'face 'dired-header)
    ;; (propertize file 'face 'marginalia-file-priv-dir)
    file))

;; function to highlight enabled modes similar to counsel-M-x
(defun mauzy/vertico-highlight-enabled-mode (cmd)
  "If MODE is enabled, highlight it as font-lock-constant-face."
  (let ((sym (intern cmd)))
    (if (or (eq sym major-mode)
            (and
             (memq sym minor-mode-list)
             (boundp sym)))
        (propertize cmd 'face 'font-lock-constant-face)
      cmd)))

(defun mauzy/sort-directories-first (files)
  ;; Still sort by history position, length and alphabetically
  (setq files (vertico-sort-history-length-alpha files))
  ;; But then move directories first
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

(provide 'mauzy-vertico)
;;; mauzy-vertico.el ends here
