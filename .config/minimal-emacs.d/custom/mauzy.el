;;; mauzy.el --- Custom crap -*- no-byte-compile: t; lexical-binding: t; -*-

(defgroup mauzy nil
  "Custom bullshit for my Emacs setup."
  :group 'environment
  :prefix "mauzy/")

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

;; Generic named vterm function
(defun mauzy/vterm-named (name &optional startup-command)
  "Create or toggle a vterm buffer with NAME in project root.
The buffer will be named *vterm-NAME:PROJECT* where PROJECT is the project name.
If STARTUP-COMMAND is provided and this is a new buffer, execute it in the terminal."
  (if-let ((project (project-current)))
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

;; Helper to create named vterm functions
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


(defcustom mauzy/project-scratch-dir
  (expand-file-name "project-scratch/" user-emacs-directory)
  "Directory where project scratch buffers are saved."
  :type 'directory
  :group 'mauzy)

(defun mauzy/project-scratch-file-name ()
  "Return the scratch file path for the current project."
  (when-let ((proj (project-current)))
    (let* ((root (project-root proj))
           (hash (secure-hash 'md5 root))
           (name (file-name-nondirectory (directory-file-name root))))
      (expand-file-name (format "%s-%s.el" name hash)
                        mauzy/project-scratch-dir))))

(defun mauzy/project-name ()
  "Return the name of the current project."
  (when-let ((proj (project-current)))
    (let ((name (file-name-nondirectory 
                 (directory-file-name (project-root proj)))))
      name)))

(defun mauzy/project-scratch-buffer-name ()
  "Return the scratch buffer name for the current project."
  (when-let ((name (mauzy/project-name)))
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
  (if-let ((proj (project-current)))
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

(provide 'mauzy)
;;; mauzy.el ends here
