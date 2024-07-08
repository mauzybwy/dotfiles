(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-major-mode-icon t)
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(package-selected-packages '(eglot-fsharp))
 '(sql-postgres-login-params
   '((user :default "mauzy")
     server
     (database :default "mauzy" :completion
      #[771 "\211\242\302=\206\12\0\211\303=?\2053\0r\300\204\27\0p\202(\0\304 \305\1!\203%\0\306\1!\202&\0p\262\1q\210\307\1\301\5!\5\5$)\207"
            [nil
             #[257 "\300 \207"
                   [sql-postgres-list-databases]
                   2 "\12\12(fn _)"]
             boundaries metadata minibuffer-selected-window window-live-p window-buffer complete-with-action]
            8 "\12\12(fn STRING PRED ACTION)"]
      :must-match confirm)
     port)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:height 1.0 :foreground "#fbf8ef"))))
 '(org-archived ((t (:foreground "#DCDCCC" :weight bold :height 0.5))))
 '(org-block ((t (:background "#1C001E" :height 0.8 :family "Monospace" :foreground "#fbf8ef"))))
 '(org-block-begin-line ((t (:background "#022" :height 0.8 :family "Monospace" :foreground "#8FA1B3"))))
 '(org-block-end-line ((t (:background "#022" :height 0.8 :family "Monospace" :foreground "#8FA1B3"))))
 '(org-code ((t (:inherit nil :background "#1C001E" :family "Monospace" :height 0.9))))
 '(org-document-info ((t (:slant italic))))
 '(org-document-info-keyword ((t (:height 0.6 :family "Monospace" :foreground "#8FA1B3"))))
 '(org-document-title ((t (:height 1.4))))
 '(org-done ((t (:height 0.8))))
 '(org-ellipsis ((t (:underline nil :height 0.5 :foreground "#fbf8ef"))))
 '(org-headline-done ((t (:strike-through t))))
 '(org-hide ((t (:foreground "#404040" :height 0.7 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight normal :slant italic :underline nil :foreground "#A7CDC4" :height 1.5))))
 '(org-level-2 ((t (:inherit default :weight normal :slant italic :underline nil :foreground "#A7CDC4" :height 1.4))))
 '(org-level-3 ((t (:inherit default :weight normal :slant italic :underline nil :foreground "#A7CDC4" :height 1.3))))
 '(org-level-4 ((t (:inherit default :weight normal :slant italic :underline nil :foreground "#A7CDC4" :height 1.2))))
 '(org-level-5 ((t (:inherit default :weight normal :slant italic :underline nil :foreground "#A7CDC4" :height 1.0))))
 '(org-link ((t (:foreground "#F0DFAF"))))
 '(org-meta-line ((t (:background nil :height 0.6 :family "Monospace" :foreground "#8FA1B3"))))
 '(org-quote ((t (:background nil :slant italic))))
 '(org-special-keyword ((t (:family "Monospace Regular" :height 0.8))))
 '(org-src-block-faces ((t (:inherit nil :family "Monospace" :foreground "#525254" :height 0.8))))
 '(org-todo ((t (:height 0.8))))
 '(shadow ((t (:foreground "#7f7f7f"))))
 '(variable-pitch ((t (:font "EB Garamond" :height 1.0 :weight thin :slant normal :foreground "#fbf8ef"))))
 '(vertico-current ((t (:extend t :background "#5F5F5F")))))
