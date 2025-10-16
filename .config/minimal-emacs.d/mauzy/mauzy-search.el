;;; mauzy-search.el --- Search and ripgrep utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for searching with consult-ripgrep and wgrep.

;;; Code:

(defun mauzy/consult-ripgrep-here ()
  "Run consult-ripgrep in the current directory only."
  (interactive)
  (consult-ripgrep default-directory))

(defun mauzy/consult-ripgrep-with-region ()
  "Run consult-ripgrep with region as initial input if active."
  (interactive)
  (let ((initial (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   nil)))
    (consult-ripgrep nil initial)))

(defun mauzy/consult-export-to-wgrep ()
  "Export consult results to grep buffer and enter wgrep mode."
  (interactive)
  (embark-export)
  ;; Give embark-export a moment to create the buffer
  (run-at-time 0.1 nil
               (lambda ()
                 (when (derived-mode-p 'grep-mode)
                   (wgrep-change-to-wgrep-mode)))))

(provide 'mauzy-search)
;;; mauzy-search.el ends here
