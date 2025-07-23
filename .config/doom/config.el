;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Commentary: Remember, you do not need to run 'doom sync' after modifying this file!

;; -----------------------------------------------------------------------------
;; Global Config
;; -----------------------------------------------------------------------------

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

(setq auth-sources '("~/.authinfo.gpg"))

(require 'auth-source)
(let ((credential (auth-source-user-and-password "github")))
  (setq grip-github-user (car credential)
        grip-github-password (cadr credential)))

;; -----------------------------------------------------------------------------
;; Loads
;; -----------------------------------------------------------------------------

;;; Load Local Packages
(load! "pkgs/org-padding.el")

;;; Load configs
(load! "config/mauzy.el")
(load! "config/bindings.el")
(load! "config/lang.el")
(load! "config/tools.el")
;; (load! "config/org.el")

;; -----------------------------------------------------------------------------
;; Installed Packages
;; -----------------------------------------------------------------------------

(use-package! nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; -----------------------------------------------------------------------------

(use-package! kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; -----------------------------------------------------------------------------

(use-package! rainbow-delimiters
  :hook prog-mode)

;; -----------------------------------------------------------------------------

(use-package! colorful-mode
  :hook (prog-mode text-mode))

;; -----------------------------------------------------------------------------

(use-package! copilot
  :hook prog-mode
  :bind
  (:map copilot-completion-map
        ("<tab>" . 'copilot-accept-completion)
        ("TAB" . 'copilot-accept-completion)
        ("C-TAB" . 'copilot-accept-completion-by-word)
        ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; -----------------------------------------------------------------------------
;; Doom Config Extensions
;; -----------------------------------------------------------------------------

(after! eglot
  (eglot-booster-mode))

;; -----------------------------------------------------------------------------

(after! vterm
  (set-popup-rule! "*doom:vterm-popup:" :size 0.4 :vslot -4 :select t :quit nil :ttl t :side 'right))

(use-package! magit
  :custom
  (magit-list-refs-sortby "-creatordate"))

;; -----------------------------------------------------------------------------

(use-package! projectile
  :bind
  (:map projectile-command-map
        ("s" . '+vertico/project-search)
        ("S" . '+vertico/project-search-from-cwd)
        ("F" . 'projectile-find-file-in-directory)
        ("C-t" . 'projectile-run-vterm)
        ("C-x C-s" . 'projectile-save-project-buffers))

  :config
  (add-to-list 'projectile-ignored-projects "/Users/mauzy/")
  (add-to-list 'projectile-ignored-projects "~/")
  (add-to-list 'projectile-globally-ignored-directories "*venv")
  (add-to-list 'projectile-globally-ignored-directories "*dist")
  (add-to-list 'projectile-globally-ignored-directories "*obj")
  (add-to-list 'projectile-globally-ignored-directories "*bin")
  (add-to-list 'projectile-globally-ignored-directories "*out")
  (add-to-list 'projectile-globally-ignored-directories "*node_modules"))

(after! projectile
  (defun +streamline/setup-project-commands ()
    "Setup commands specific to the Streamline project."
    (when (and (projectile-project-p)
               (string-match-p "streamline" (projectile-project-root)))

      ;; Define project-specific search functions
      (defun +streamline/search-debug-logs ()
        "Search for IO.inspect in apps/phoenix/lib."
        (interactive)
        (+vertico/project-search nil "\\(console.logger.debug\\|IO.inspect\\) -- -g *.ex -g *.exs -g *.ts -g *.tsx -g *.js -g *.jsx" nil))

      ;; Project-specific keybindings using Doom conventions
      ;; Using localleader (SPC m) for mode-specific bindings
      (map! :localleader
            (:prefix ("p" . "project-search")  ; Custom prefix for project searches
             :desc "Search for debug logs"         "s d" #'+streamline/search-debug-logs))))

  ;; Hook to run when switching projects
  (add-hook! 'projectile-after-switch-project-hook #'+streamline/setup-project-commands))


;; -----------------------------------------------------------------------------

(use-package! dirvish
  :init
  (dirvish-override-dired-mode)

  :config
  (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (when (eq system-type 'darwin)
    (setq insert-directory-program "/opt/homebrew/bin/gls"))

  :custom
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))
  (dirvish-mode-line-height 10)
  (dirvish-attributes
   '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (dirvish-subtree-state-style 'nerd)
  (delete-by-moving-to-trash t)
  (dirvish-path-separators (list
                            (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                            (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                            (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (dired-listing-switches
   "-l --almost-all --human-readable --group-directories-first --no-group")
  )

;; -----------------------------------------------------------------------------

(use-package! orderless
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

;; -----------------------------------------------------------------------------

(use-package! corfu
  :custom
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.1)
  (corfu-auto t)
  (corfu-preselect t)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)

  :bind
  (:map corfu-map
        ("<tab>" . nil)
        ("TAB" . nil)
        ("SPC" . corfu-insert-separator)
        ("C-;" . 'corfu-quick-jump))

  :init
  (global-corfu-mode))

;; -----------------------------------------------------------------------------

(use-package! flymake
  :custom
  (flymake-no-changes-timeout 4))

;; -----------------------------------------------------------------------------

(use-package! avy
  :custom
  (avy-timeout-seconds 0.1)
  (avy-all-windows t))

;; -----------------------------------------------------------------------------

(after! doom-modeline
  (nyan-mode))

;; -----------------------------------------------------------------------------
;; Fonts and Themes
;; -----------------------------------------------------------------------------

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
