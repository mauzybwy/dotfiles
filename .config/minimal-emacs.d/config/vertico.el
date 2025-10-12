;;; vertico.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Code:

;;;;; HIGHTLIGHTING
(defvar +vertico-transform-functions nil)

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
  (dolist (fun (ensure-list +vertico-transform-functions))
    (setq cand (funcall fun cand)))
  (cl-call-next-method cand prefix suffix index start))

(defun +vertico-highlight-directory (file)
  "If FILE ends with a slash, highlight it as a directory."
  (if (string-suffix-p "/" file)
      ;;(propertize file 'face 'custom-face-tag)
      (propertize file 'face 'dired-directory)
    ;; (propertize file 'face 'dired-header)
    ;; (propertize file 'face 'marginalia-file-priv-dir)
    file))

;; function to highlight enabled modes similar to counsel-M-x
(defun +vertico-highlight-enabled-mode (cmd)
  "If MODE is enabled, highlight it as font-lock-constant-face."
  (let ((sym (intern cmd)))
    (if (or (eq sym major-mode)
            (and
             (memq sym minor-mode-list)
             (boundp sym)))
        (propertize cmd 'face 'font-lock-constant-face)
      cmd)))

(defun sort-directories-first (files)
  ;; Still sort by history position, length and alphabetically
  (setq files (vertico-sort-history-length-alpha files))
  ;; But then move directories first
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode)

  :init
  (setq vertico-count 17)
  (setq vertico-resize nil)

  (setq vertico-multiform-categories
        '((symbol (vertico-sort-function . vertico-sort-alpha))
          (file (vertico-sort-function . sort-directories-first))))

  (setq vertico-multiform-commands
        '((consult-line (vertico-sort-override-function . vertico-sort-alpha))))


  (vertico-multiform-mode t)
  (setq uniquify-buffer-name-style 'forward)

  (add-to-list 'vertico-multiform-categories
               '(file
                 ;; this is also defined in the wiki, uncomment if used
                 ;; (vertico-sort-function . sort-directories-first)
                 (+vertico-transform-functions . +vertico-highlight-directory)))

  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command
                 reverse
                 (+vertico-transform-functions . +vertico-highlight-enabled-mode)))
  )

;;; vertico.el ends here
