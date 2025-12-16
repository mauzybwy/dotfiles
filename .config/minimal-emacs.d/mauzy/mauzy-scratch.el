;;; mauzy-scratch.el --- Project scratch buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; Persistent scratch buffers for projects.

;;; Code:

(require 'mauzy-project)

(defcustom mauzy/project-scratch-dir
  (expand-file-name "project-scratch/" user-emacs-directory)
  "Directory where project scratch buffers are saved."
  :type 'directory
  :group 'mauzy)

(defun mauzy/project-scratch-file-name ()
  "Return the scratch file path for the current project."
  (when-let* ((proj (project-current)))
    (let* ((root (project-root proj))
           (hash (secure-hash 'md5 root))
           (name (file-name-nondirectory (directory-file-name root))))
      (expand-file-name (format "%s-%s.el" name hash)
                        mauzy/project-scratch-dir))))

(defun mauzy/project-scratch-buffer-name ()
  "Return the scratch buffer name for the current project."
  (when-let* ((name (mauzy/project-name)))
    (format "*scratch:%s*" name)))

(defun mauzy/project-scratch-save ()
  "Save the current project scratch buffer to disk."
  (when-let* ((proj (project-current))
              (buf-name (mauzy/project-scratch-buffer-name))
              (buf (get-buffer buf-name)))
    (with-current-buffer buf
      (let ((scratch-file (mauzy/project-scratch-file-name)))
        (unless (file-exists-p mauzy/project-scratch-dir)
          (make-directory mauzy/project-scratch-dir t))
        (write-region (point-min) (point-max) scratch-file nil 'quiet)))))

(defun mauzy/project-scratch-setup ()
  "Set up project scratch buffer settings."
  (when (string-match-p "^\\*scratch:" (buffer-name))
    (setq-local buffer-offer-save nil
                header-line-format (format "Project Scratch: %s (C-c C-c to save and close)" 
                                           (mauzy/project-name)))
    
    ;; Add keybindings
    (local-set-key (kbd "C-c C-c")
                   (lambda ()
                     (interactive)
                     (mauzy/project-scratch-save)
                     (quit-window t)))
    (local-set-key (kbd "C-x C-s")
                   (lambda ()
                     (interactive)
                     (mauzy/project-scratch-save)
                     (quit-window t)))))

(defun mauzy/project-scratch ()
  "Open or create a persistent scratch buffer for the current project.
Works like *scratch* but content is saved between sessions."
  (interactive)
  (if-let* ((proj (project-current)))
      (let* ((buf-name (mauzy/project-scratch-buffer-name))
             (scratch-file (mauzy/project-scratch-file-name))
             (buf (or (get-buffer buf-name)
                      (generate-new-buffer buf-name))))
        (with-current-buffer buf
          ;; Load content if file exists
          (when (and (zerop (buffer-size))
                     (file-exists-p scratch-file))
            (insert-file-contents scratch-file))
          
          ;; Set up the buffer like *scratch*
          (unless (eq major-mode 'lisp-interaction-mode)
            (lisp-interaction-mode))
          
          ;; Apply settings initially
          (mauzy/project-scratch-setup)
          
          ;; Reapply settings after any major mode change
          (add-hook 'after-change-major-mode-hook 
                    'mauzy/project-scratch-setup)
          
          ;; Save on buffer modifications
          (add-hook 'after-change-functions
                    (lambda (&rest _) (run-with-idle-timer 1 nil #'mauzy/project-scratch-save))
                    nil t)
          
          ;; Save when killing buffer
          (add-hook 'kill-buffer-hook #'mauzy/project-scratch-save nil t))
        (pop-to-buffer buf))
    (message "Not in a project")))

(provide 'mauzy-scratch)
;;; mauzy-scratch.el ends here
