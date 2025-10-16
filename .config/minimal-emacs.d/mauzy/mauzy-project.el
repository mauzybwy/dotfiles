;;; mauzy-project.el --- Project utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for working with projects.

;;; Code:

(defun mauzy/project-current-last (orig-fun &rest args)
  "Advice to move current project to end of list."
  (let* ((projects (apply orig-fun args))
         (current (project-current))
         (current-root (when current (project-root current))))
    (if current-root
        (append (remove current-root projects) (list current-root))
      projects)))

(defun mauzy/project-switch-to-recent-buffer ()
  "Switch to most recent buffer in project, or show buffer list if none open."
  (interactive)
  (let* ((project (project-current t))
         (buffers (project-buffers project))
         (recent-buffer (car buffers)))
    (if recent-buffer
        (switch-to-buffer recent-buffer)
      (consult-project-buffer))))

(defun mauzy/project-name ()
  "Return the name of the current project."
  (when-let ((proj (project-current)))
    (let ((name (file-name-nondirectory 
                 (directory-file-name (project-root proj)))))
      name)))

(provide 'mauzy-project)
;;; mauzy-project.el ends here
