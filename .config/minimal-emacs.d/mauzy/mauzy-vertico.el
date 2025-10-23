;;; mauzy-vertico.el --- Terminal utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for managing vertico.

;;; Code:

(defun mauzy/vertico-highlight-directory (candidate)
  "If FILE ends with a slash, highlight it as a directory."
  (if (string-suffix-p "/" candidate)
      (propertize candidate 'face 'dired-directory)
    candidate))

(defun mauzy/vertico-format-candidate (orig-fun candidate &rest args)
  (setq candidate (mauzy/vertico-highlight-directory candidate))
  (apply orig-fun candidate args))

(defun mauzy/sort-directories-first (files)
  ;; Still sort by history position, length and alphabetically
  (setq files (vertico-sort-history-length-alpha files))
  ;; But then move directories first
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

(provide 'mauzy-vertico)
;;; mauzy-vertico.el ends here
