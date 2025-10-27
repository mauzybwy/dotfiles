;;; pre-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "mauzy" user-emacs-directory))

(set-face-attribute 'default nil :family "JetBrains Mono" :weight 'regular :height 125)

(require 'mauzy-splash)

;; Configure the splash screen
(setq mauzy/splash-image-path (expand-file-name "bruiser.png" user-emacs-directory))
(setq mauzy/splash-text '("welcome back :)"))

;; Enable the splash screen
(setq initial-buffer-choice #'mauzy/splash-screen)

;; Straight bootstrap
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name
;;         "straight/repos/straight.el/bootstrap.el"
;;         (or (bound-and-true-p straight-base-dir)
;;             user-emacs-directory)))
;;       (bootstrap-version 7))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

(add-to-list 'default-frame-alist '(undecorated-round . t))

;; (setq exec-path
;;       '("/opt/homebrew/bin" "/opt/homebrew/sbin" "/Users/mauzy/.nix-profile/bin"
;;         "/run/current-system/sw/bin" "/nix/var/nix/profiles/default/bin"
;;         "/usr/local/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin"
;;         "/Applications/Emacs.app/Contents/MacOS/bin-arm64-11"
;;         "/Applications/Emacs.app/Contents/MacOS/libexec-arm64-11"
;;         "/Applications/Emacs.app/Contents/MacOS/libexec"))

(provide 'pre-init)
;;; pre-init.el ends here
