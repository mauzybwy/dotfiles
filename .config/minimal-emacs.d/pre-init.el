;;; pre-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

(set-face-attribute 'default nil :family "JetBrains Mono" :weight 'regular :height 125)

(require 'splash)

;; Configure the splash screen
(setq splash-image-path (expand-file-name "bruiser.png" user-emacs-directory))
(setq splash-text '("welcome back :)"))

;; Enable the splash screen
(setq initial-buffer-choice #'splash-screen)

(provide 'pre-init)
;;; pre-init.el ends here
