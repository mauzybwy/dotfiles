;;; theme.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

;; (use-package kanagawa-themes
;;   :ensure t
;;   :config
;;   (load-theme 'kanagawa-wave t))

;; (use-package zenburn-theme
;;   :config
;;   (load-theme 'zenburn t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mauzy/load-zenburn ()
  "Load doom-zenburn theme with custom overrides."
  (interactive)
  (load-theme 'doom-zenburn t)
  (custom-theme-set-faces
   'doom-zenburn
   `(default ((t (:foreground ,(doom-color 'fg) :background ,(doom-color 'base1)))))
   `(region ((t (:background ,(doom-color 'base4)))))
   `(vertico-current ((t (:background ,(doom-color 'base4))))))
  
  (enable-theme 'doom-zenburn))

(defun mauzy/load-monokai-pro ()
  "Load doom-monokai-pro theme with custom overrides."
  (interactive)
  (load-theme 'doom-monokai-pro t)
  (custom-theme-set-faces
   'doom-monokai-pro)
  ;; `(font-lock-function-name-face ((t (:foreground ,(doom-color 'blue)))))
  ;; `(font-lock-type-face ((t (:foreground ,(doom-color 'violet)))))
  ;; `(font-lock-constant-face ((t (:foreground ,(doom-color 'blue)))))
  
  (enable-theme 'doom-monokai-pro))

(defun mauzy/load-catppuccin ()
  "Load catppuccin theme with custom overrides."
  (interactive)
  (load-theme 'catppuccin t)
  (custom-theme-set-faces
   'catppuccin
   `(vertico-current ((t (:background "#454552")))))
  (enable-theme 'catppuccin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package colorful-mode
  ;; :diminish
  ;; :ensure t ; Optional
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

(use-package catppuccin-theme
  :ensure t
  :init
  (setq catppuccin-flavor 'mocha)
  :config
  ;; (load-theme 'catppuccin :no-confirm)
  (mauzy/load-catppuccin))



;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;; 
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;   ;; (load-theme 'doom-one t)
;;   ;; (load-theme 'doom-monokai-pro t)
;;   (mauzy/load-zenburn)
;;   ;; (mauzy/load-monokai-pro)
;; 
;;   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)
;;   ;; Enable custom neotree theme (nerd-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;;   (doom-themes-treemacs-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;;; theme.el ends here
