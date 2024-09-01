;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Commentary: Remember, you do not need to run 'doom sync' after modifying this file!

(require 'smooth-scroll)

;;; Load Local Packages
(load! "./pkgs/org-padding.el")

;;; Load configs
(load! "./conf/mauzy.el")
(load! "./conf/vertico.el")
(load! "./conf/bindings.el")
(load! "./conf/lang.el")
(load! "./conf/lang.el")
(load! "./conf/orgmode.el")

;;; Global Variables

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Brandon Wetzel"
      user-mail-address "mauzybwy@gmail.com")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; For magit forge (github)
(setq auth-sources '("~/.authinfo.gpg"))


;; Nyannnnnnnnnnn
(use-package! nyan-mode
  :after doom-modeline
  :init
  (nyan-mode))

(after! projectile
  (add-to-list 'projectile-ignored-projects "/Users/mauzy/")
  (add-to-list 'projectile-globally-ignored-directories "*venv")
  (add-to-list 'projectile-globally-ignored-directories "*dist")
  (add-to-list 'projectile-globally-ignored-directories "*obj")
  (add-to-list 'projectile-globally-ignored-directories "*bin")
  (add-to-list 'projectile-globally-ignored-directories "*out")
  (add-to-list 'projectile-globally-ignored-directories "*node_modules"))

(use-package! dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (when (eq system-type 'darwin)
    (setq insert-directory-program "/opt/homebrew/bin/gls"))
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-mode-line-height 10)
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-subtree-state-style 'nerd)
  (setq delete-by-moving-to-trash t)
  (setq dirvish-path-separators (list
                                 (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  )

(use-package! nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; ;; Use `consult-completion-in-region' if Vertico is enabled.
;; ;; Otherwise use the default `completion--in-region' function.
;; (setq completion-in-region-function
;;       (lambda (&rest args)
;;         (apply (if vertico-mode
;;                    #'consult-completion-in-region
;;                  #'completion--in-region)
;;                args)))

;; (use-package! company
;;   :config
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-show-numbers t)
;;   (setq company-idle-delay 0.2)
;;   (setq company-tooltip-idle-delay 0.2))
;; (add-to-list 'company-backends #'company-tabnine)

;;TAB-only configuration
(use-package! corfu
  :custom
  (corfu-auto t)
  (corfu-preselect t) 
  ;; (corfu-popupinfo-mode nil)
  ;; (corfu-terminal-disable-on-gui nil)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  ;; Free the RET key for less intrusive behavior.
  :bind
  (:map corfu-map
        ;; Option 1: Unbind RET completely
        ("<tab>" . nil)
        ("TAB" . nil)
        ("C-;" . 'corfu-quick-jump))

  :init
  (global-corfu-mode)
  ;; (corfu-terminal-mode t)
  ;; (corfu-doc-terminal-mode t)
  (setq corfu-auto-prefix 1)
  (setq corfu-auto-delay 0.1))

(use-package! kind-icon
  :ensure t
  :after corfu
                                        ;:custom
                                        ; (kind-icon-blend-background t)
                                        ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; (defun my/eglot-capf ()
;;   (setq-local completion-at-point-functions
;;               (list (cape-capf-super
;;                      #'eglot-completion-at-point
;;                      #'tempel-expand
;;                      #'cape-file))))

;; (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)


;; (after! rjsx-mode
;;   (set-company-backend! 'rjsx-mode nil)
;;   (set-company-backend! 'rjsx-mode '(company-tabnine)))

;; accept completion from copilot and fallback to company
(use-package! rainbow-mode
  :hook (prog-mode text-mode))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package! flycheck
  :config
  (setq flycheck-idle-change-delay 4)
  (setq
   flycheck-check-syntax-automatically
   '(save
     mode-enable
     ;; idle-change
     ;; new-line
     )))


(use-package! flymake
  :config
  (setq flymake-no-changes-timeout 4))

(use-package! avy
  :config
  (setq avy-timeout-seconds 0.1)
  (setq avy-all-windows t))

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-zenburn
      doom-font (font-spec :family "JetBrains Mono" :size 13 :weight 'regular)
      doom-variable-pitch-font "EB Garamond"
      doom-modeline-major-mode-color-icon t
      doom-modeline-project-detection 'auto)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
;; (add-hook! '+doom-dashboard-functions :append
;;   (setq fancy-splash-image (concat doom-user-dir "bruiser.png")))
(setq fancy-splash-image (concat doom-user-dir "bruiser.png"))

(tool-bar-mode)
(menu-bar-mode -1)
(tool-bar-mode 0)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
