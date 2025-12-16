;;; mauzy-vterm.el --- Terminal utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for managing named vterm buffers.

;;; Code:

(defun mauzy/vterm-named (name &optional startup-command)
  "Create or toggle a vterm buffer with NAME in project root.
The buffer will be named *vterm-NAME:PROJECT* where PROJECT is the project name.
If STARTUP-COMMAND is provided and this is a new buffer, execute it in the terminal."
  (if-let* ((project (project-current)))
      (let* ((project-root (project-root project))
             (project-name (file-name-nondirectory (directory-file-name project-root)))
             (default-directory project-root)
             (buffer-name (format "*vterm-%s:%s*" name project-name))
             (buffer-existed (get-buffer buffer-name))
             (target-window (get-buffer-window buffer-name))
             (in-vterm-p (derived-mode-p 'vterm-mode)))
        (cond
         ;; Buffer is visible in a window, delete that window
         (target-window
          (delete-window target-window))
         ;; Buffer exists but not visible
         (buffer-existed
          (if in-vterm-p
              ;; We're in a vterm buffer, replace it in the current window
              (switch-to-buffer buffer-name)
            ;; We're not in a vterm buffer, pop to it
            (pop-to-buffer buffer-name)))
         ;; Buffer doesn't exist, create it
         (t
          (if in-vterm-p
              ;; We're in a vterm buffer, replace it
              (progn
                (vterm buffer-name))
            ;; We're not in a vterm buffer, create normally
            (vterm buffer-name))
          (when startup-command
            ;; Wait a moment for vterm to fully initialize
            (run-with-timer 0.5 nil
                            (lambda (buf cmd)
                              (when (buffer-live-p buf)
                                (with-current-buffer buf
                                  (vterm-send-string cmd)
                                  (vterm-send-return))))
                            (current-buffer)
                            startup-command)))))
    (message "Not in a project")))

(defun mauzy/make-vterm-function (name key &optional startup-command)
  "Create a function that opens a named vterm and bind it to KEY.
If STARTUP-COMMAND is provided, run it when the terminal is first created."
  (let ((func-name (intern (format "mauzy/vterm-%s" name))))
    (defalias func-name
      (lambda ()
        (interactive)
        (mauzy/vterm-named name startup-command))
      (format "Create or switch to vterm buffer named '%s' in project root." name))
    (when key
      (global-set-key (kbd key) func-name))))

(provide 'mauzy-vterm)
;;; mauzy-vterm.el ends here
