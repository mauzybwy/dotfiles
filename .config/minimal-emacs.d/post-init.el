;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Code:

;; (use-package vertico
;;   ;; (Note: It is recommended to also enable the savehist package.)
;;   :ensure t
;;   :defer t
;;   :commands vertico-mode
;;   :hook (after-init . vertico-mode))

(load "~/.config/minimal-emacs.d/post-init/theme.el")

;;; ----------------------------------------------------------------------------

(load "~/.config/minimal-emacs.d/post-init/vertico.el")

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;; ----------------------------------------------------------------------------

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode)

  :hook
  (marginalia-mode #'nerd-icons-completion-marginalia-setup))

;;; ----------------------------------------------------------------------------

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;;; ----------------------------------------------------------------------------

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

;;; ----------------------------------------------------------------------------

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-M-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :custom
  (prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;; ----------------------------------------------------------------------------

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; ----------------------------------------------------------------------------

(use-package consult
  :ensure t
  
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-project-buffer)
         ("C-x C-b" . consult-project-buffer)
         ("C-x B" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  ;; Optionally configure the register formatting. This improves the register
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")

  :init
  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)))

;;; ----------------------------------------------------------------------------

(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  (corfu-auto t)
  (corfu-preselect t)
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.1)
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  ;; (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

;;; ----------------------------------------------------------------------------

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;; ----------------------------------------------------------------------------

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))

;;; ----------------------------------------------------------------------------

(use-package avy
  :config
  (global-unset-key (kbd "C-;"))
  (global-set-key (kbd "C-;")   'avy-goto-word-0)
  (global-unset-key (kbd "M-;"))
  (global-set-key (kbd "M-;")   'avy-goto-line)
  
  :custom
  (avy-timeout-seconds 0.1)
  (avy-all-windows t))

;;; ----------------------------------------------------------------------------

(use-package direnv
  :config
  (direnv-mode))

;;; ----------------------------------------------------------------------------

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-/"))
  (global-set-key (kbd "C-/")   'undo-fu-only-undo)
  (global-unset-key (kbd "M-/"))
  (global-set-key (kbd "M-/") 'undo-fu-only-redo))

;; ;;; ----------------------------------------------------------------------------

(use-package undo-fu-session
  :defer t
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))

;;; ----------------------------------------------------------------------------

(use-package emacs
  :init
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

  :bind
  (
   ("C-c l e b" . eval-buffer)
   ("C-c r" . revert-buffer))
  
  :hook
  (after-init . global-auto-revert-mode)
  (after-init . recentf-mode)
  (after-init . savehist-mode)
  (after-init . save-place-mode)
  (after-init . electric-pair-mode)
  (after-init . show-paren-mode)
  (after-init . display-line-numbers-mode)
  (kill-emacs . recentf-cleanup)

  :custom
  ;; Splash screen
  (fancy-splash-image "~/.config/doom/bruiser.png")
  (font-lock-maximum-decoration t)
  (display-line-numbers-type t)
  (display-line-numbers-mode t)
  (line-number-mode t)
  (column-number-mode t)

  :config
  ;; Tree-sitter
  (setq treesit-font-lock-level 4)
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (astro "https://github.com/mauzybwy/tree-sitter-astro" "emacs")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (jq "https://github.com/nverno/tree-sitter-jq")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          ;; (sql "https://github.com/m-novikov/tree-sitter-sql")
          (nix "https://github.com/nix-community/tree-sitter-nix")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

;;; ----------------------------------------------------------------------------

(use-package eglot
  :ensure nil
  :defer t
  :hook (((
           elixir-ts-mode
           tsx-ts-mode
           typescript-ts-mode
           )
          . eglot-ensure))
  :commands (eglot
             eglot-ensure
             eglot-rename
             eglot-format-buffer)
  :config
  (add-to-list
   'eglot-server-programs '(elixir-ts-mode "elixir-ls"))

  (setq-default
   eglot-workspace-configuration
   ;; NOTE: this pylsp config is just in here for reference
   `(:pylsp (:plugins
             (;; Fix imports and syntax using `eglot-format-buffer`
              :isort (:enabled t)
              :autopep8 (:enabled t)

              ;; Syntax checkers (works with Flymake)
              :pylint (:enabled t)
              :pycodestyle (:enabled t)
              :flake8 (:enabled t)
              :pyflakes (:enabled t)
              :pydocstyle (:enabled t)
              :mccabe (:enabled t)

              :yapf (:enabled :json-false)
              :rope_autoimport (:enabled :json-false)))))
  )

(use-package project
  :ensure t

  ;; :bind-keymap ("C-c p" . project-prefix-map)
  :bind
  ("C-x p v" . magit-project-status)
  ("C-x p s" . consult-ripgrep)

  :custom
  (project-switch-commands
   '(
     (project-find-file "Find file")
     (consult-ripgrep "Search (ripgrep)")
     (consult-project-buffer "Find buffer")
     (magit-project-status "VC: magit")
     (project-find-dir "Find directory")
     (project-eshell "Eshell")
     (project-any-command "Other")
     ))
  )

;;; ----------------------------------------------------------------------------

(use-package which-key
  :ensure nil ; builtin
  :defer t
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
  ;; versions, except for emacs-mac.
  ;;
  ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
  ;; this version of Emacs natively supports smooth scrolling.
  ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
  (setq pixel-scroll-precision-use-momentum nil) ; Precise/smoother scrolling
  (pixel-scroll-precision-mode 1))

;;; ----------------------------------------------------------------------------

(use-package multiple-cursors)

;;; ----------------------------------------------------------------------------

(use-package nix-ts-mode)

;;; ----------------------------------------------------------------------------

(use-package elixir-ts-mode)

;;; ----------------------------------------------------------------------------

(use-package typescript-ts-mode)

;;; ----------------------------------------------------------------------------

;; (use-package tsx-ts-mode
;;   :ensure nil)

;;; ----------------------------------------------------------------------------

(use-package json-ts-mode
  :ensure nil)

;;; ----------------------------------------------------------------------------

(use-package magit
  :custom
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration))

;;; ----------------------------------------------------------------------------

(use-package apheleia
  :ensure t
  :defer t
  :commands (apheleia-mode
             apheleia-global-mode)
  :hook ((prog-mode . apheleia-mode)))

;;; ----------------------------------------------------------------------------

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;;; ----------------------------------------------------------------------------

(provide 'post-init)
;;; post-init.el ends here
