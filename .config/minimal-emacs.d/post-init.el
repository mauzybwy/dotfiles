;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Code:

;; Setup
(require 'mauzy)
(load (expand-file-name "config/theme" user-emacs-directory))

(setq use-package-always-ensure t)
(setq use-package-always-defer nil)
(setq use-package-verbose t)
(setq use-package-compute-statistics t)

;; Safe local variable directories
(add-to-list 'safe-local-variable-directories "/Users/mauzy/code/streamline/")

;;; ---------------------------------------------------------------------------

(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :defer t
  :commands vertico-mode
  :hook
  (after-init . vertico-mode)
  (after-init . vertico-multiform-mode)

  :custom
  (vertico-cycle t)
  (vertico-count 17)
  (vertico-resize nil)

  :init
  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not mauzy/vertico-transform-functions) null))
    (dolist (fun (ensure-list mauzy/vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))
  
  :config
  (setq uniquify-buffer-name-style 'forward)
  
  (setq vertico-multiform-categories
        '((symbol (vertico-sort-function . vertico-sort-alpha))
          (file (vertico-sort-function . mauzy/sort-directories-first))))

  (setq vertico-multiform-commands
        '((consult-line (vertico-sort-override-function . vertico-sort-alpha))))

  (add-to-list 'vertico-multiform-categories
               '(file
                 ;; this is also defined in the wiki, uncomment if used
                 ;; (vertico-sort-function . mauzy/sort-directories-first)
                 (mauzy/vertico-transform-functions . mauzy/vertico-highlight-directory)))

  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command
                 reverse
                 (mauzy/vertico-transform-functions . mauzy/vertico-highlight-enabled-mode))))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil ; built-in with vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  
  :hook
  ;; Tidy shadowed file names
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;; ----------------------------------------------------------------------------

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode)

  :hook
  (marginalia-mode #'nerd-icons-completion-marginalia-setup))

;;; ----------------------------------------------------------------------------

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))


;;; ----------------------------------------------------------------------------

(use-package marginalia
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

;;; ----------------------------------------------------------------------------

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer 1)) 


;;; ----------------------------------------------------------------------------

;; NOTE: not using aggressive-indent at the moment, but keeping this here for reference
;; (use-package aggressive-indent
;;   :hook ((emacs-lisp-mode
;;           lisp-mode
;;           elixir-ts-mode
;;           clojure-mode) . aggressive-indent-mode))

(use-package parinfer-rust-mode
  :hook emacs-lisp-mode
  :custom
  (parinfer-rust-disable-troublesome-modes t)
  (parinfer-rust-auto-download t))

;;; ----------------------------------------------------------------------------

(use-package embark
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)
   ("C-M-;" . embark-dwim)
   ("C-h B" . embark-bindings))

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
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; ----------------------------------------------------------------------------

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)

         ;; General bindings
         ([remap Info-search] . consult-info)
         ([remap isearch-forward] . consult-line-literal)
         ("C-x C-b" . consult-project-buffer)
         ("C-x b" . consult-project-buffer)
         ("C-x p b" . consult-project-buffer)
         ("M-y" . consult-yank-pop)

         :map minibuffer-local-map
         ("C-c C-e" . mauzy/consult-export-to-wgrep)
         ("M-s" . consult-history)
         ("M-r" . consult-history)

         ;; M-g bindings in `goto-map'
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi))

  
  :hook
  ;; Enable automatic preview at point in the *Completions* buffer.
  (completion-list-mode . consult-preview-at-point-mode)

  :custom
  ;; Optionally configure the register formatting. This improves the register
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "<")
  (consult-line-start-from-top nil)
  (consult-ripgrep-args (string-join
                         '("rg"
                           "--null"
                           "--line-buffered"
                           "--color=never"
                           "--max-columns=1000"
                           "--path-separator /"
                           "--smart-case"
                           "--no-heading"
                           "--with-filename"
                           "--line-number"
                           "--search-zip"
                           "--hidden") " "))

  :init
  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  (defun consult-line-literal ()
    (interactive)
    (let ((completion-styles '(orderless))
          (orderless-matching-styles '(orderless-literal))
          (completion-category-defaults nil)
          (completion-category-overrides nil))
      (consult-line)))
  
  :config
  (defvar my-consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map))
  
  (consult-customize
   consult-line :prompt "🔎 " :group nil :sort nil :keymap my-consult-line-map
   consult-line-literal :prompt "🔎 " :group nil :sort nil :keymap my-consult-line-map
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any)))

;;; ----------------------------------------------------------------------------

(use-package corfu
  :defer t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :config
  (global-corfu-mode)
  
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
  (text-mode-ispell-word-completion nil))

;;; ----------------------------------------------------------------------------

(use-package cape
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;; ----------------------------------------------------------------------------

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

;;; ----------------------------------------------------------------------------

(use-package avy
  :config
  (global-unset-key (kbd "C-;"))
  (global-set-key (kbd "C-;")   'avy-goto-word-0)
  (global-unset-key (kbd "M-;"))
  (global-set-key (kbd "M-;")   'avy-goto-line)
  
  :custom
  (avy-timeout-seconds 0.3)
  (avy-all-windows t))

;;; ----------------------------------------------------------------------------

(use-package direnv
  :config
  (direnv-mode))

;;; ----------------------------------------------------------------------------

;; Basic vterm setup
(use-package vterm
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t))

;; vterm-toggle for quick access
(use-package vterm-toggle
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  
  (global-set-key (kbd "C-c t") 'vterm-toggle)
  (global-set-key (kbd "C-c T") 'vterm-toggle-cd)
  
  (define-key vterm-mode-map (kbd "C-c t") 'vterm-toggle)
  (define-key vterm-mode-map (kbd "C-c T") 'vterm-toggle-cd))

;; multi-vterm for managing multiple terminals
(use-package multi-vterm
  :config
  (setq multi-vterm-buffer-name "vterm")
  
  (global-set-key (kbd "C-c n t") 'multi-vterm)
  (global-set-key (kbd "C-c p t") 'multi-vterm-project)
  
  (define-key vterm-mode-map (kbd "C-c C-n") 'multi-vterm-next)
  (define-key vterm-mode-map (kbd "C-c C-p") 'multi-vterm-prev))

;;; ----------------------------------------------------------------------------

;; Basic undo-fu setup
(use-package undo-fu
  :config
  (global-unset-key (kbd "C-/"))
  (global-set-key (kbd "C-/")   'undo-fu-only-undo)
  (global-unset-key (kbd "M-/"))
  (global-set-key (kbd "M-/") 'undo-fu-only-redo))

;; Persist undo history across sessions
(use-package undo-fu-session
  :defer t
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))

;; Visualize undo history with vundo
(use-package vundo
  :commands (vundo)
  :bind (("C-M-/" . vundo))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

;;; ----------------------------------------------------------------------------

(use-package mwim
  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

;;; ----------------------------------------------------------------------------

(use-package emacs
  :init
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

  :bind
  (("C-h F" . describe-face)
   ("C-c r" . revert-buffer))
  
  :hook
  ;; init hooks
  (after-init . global-auto-revert-mode)
  (after-init . recentf-mode)
  (after-init . savehist-mode)
  (after-init . save-place-mode)
  (after-init . show-paren-mode)

  ;; prog hooks
  (prog-mode . display-line-numbers-mode)
  (prog-mode . subword-mode)

  ;; cleanup hooks
  (kill-emacs . recentf-cleanup)

  :custom
  (font-lock-maximum-decoration t)
  (line-number-mode t)
  (column-number-mode t)
  (confirm-kill-emacs 'yes-or-no-p)

  :config  
  ;; Display all starred buffers at the bottom
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-name action)
                   (or (string-match-p "\\*Embark.*\\*" buffer-name)
                       (string-match-p "\\*scratch:.*\\*" buffer-name)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 20)
                 (reusable-frames . visible))
               t))

;;; ----------------------------------------------------------------------------

(use-package treesit
  :ensure nil ; builtin
  :config
  (setq treesit-font-lock-level 4)
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (astro "https://github.com/mauzybwy/tree-sitter-astro" "emacs")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
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
  :ensure nil ; builtin

  :hook (((elixir-ts-mode
           tsx-ts-mode
           typescript-ts-mode)
          . eglot-ensure))

  :commands (eglot
             eglot-ensure
             eglot-rename
             eglot-format-buffer)
  :bind
  (("C-c l s r" . eglot-rename)
   ("C-c l s d" . eldoc-doc-buffer)
   ("C-c l s D" . eldoc-box-eglot-help-at-point))

  :config
  (add-to-list 'eglot-server-programs
               '(elixir-ts-mode "elixir-ls"))

  (add-to-list 'eglot-server-programs
               '((tsx-ts-mode typescript-ts-mode)
                 . ("/Users/mauzy/Library/pnpm/typescript-language-server" "--stdio")))

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
              :rope_autoimport (:enabled :json-false))))))


;; Eldoc box for better eldoc display
(use-package eldoc-box)

;;; ----------------------------------------------------------------------------

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook prog-mode
  :bind
  (:map copilot-completion-map
        ("<tab>" . 'copilot-accept-completion)
        ("TAB" . 'copilot-accept-completion)
        ("C-TAB" . 'copilot-accept-completion-by-word)
        ("C-<tab>" . 'copilot-accept-completion-by-word)))

;;; ----------------------------------------------------------------------------

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

;;; ----------------------------------------------------------------------------

(use-package project
  :bind
  ("C-x p v" . magit-project-status)
  ("C-x p x" . mauzy/project-scratch)
  ("C-x p s" . mauzy/consult-ripgrep-with-region)
  ("C-x p S" . mauzy/consult-ripgrep-here)

  :config
  (unbind-key "o" project-prefix-map)
  (advice-add 'project-known-project-roots :around #'mauzy/project-current-last)
  
  :custom
  (project-switch-commands
   'mauzy/project-switch-to-recent-buffer))

;; NOTE: keeping this here for reference
;; (project-switch-commands
;;  '(
;;    (project-find-file "Find file")
;;    (consult-ripgrep "Search (ripgrep)")
;;    (consult-project-buffer "Find buffer")
;;    (magit-project-status "VC: magit")
;;    (project-find-dir "Find directory")
;;    (project-eshell "Eshell")
;;    (project-any-command "Other")))


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

;;; ----------------------------------------------------------------------------

(use-package multiple-cursors
  :bind-keymap ("C-c m" . multiple-cursors-map)
  :config
  (defvar multiple-cursors-map (make-sparse-keymap)
    "Keymap for multiple-cursors commands.")
  
  (let ((map multiple-cursors-map))
    (define-key map (kbd "e") '("edit lines" . mc/edit-lines))
    (define-key map (kbd "p") '("mark previous" . mc/mark-previous-like-this))
    (define-key map (kbd "n") '("mark next" . mc/mark-next-like-this))
    (define-key map (kbd "C-p") '("mark previous word" . mc/mark-previous-like-this-word))
    (define-key map (kbd "C-n") '("mark next word" . mc/mark-next-like-this-word))
    (define-key map (kbd "a") '("mark all" . mc/mark-all-like-this))
    (define-key map (kbd "r") '("mark region" . mc/mark-all-in-region))
    (define-key map (kbd "d") '("mark in defun" . mc/mark-all-like-this-in-defun))
    (define-key map (kbd "t") '("mark dwim" . mc/mark-all-like-this-dwim))))

;;; ----------------------------------------------------------------------------

;; Expand-region, but with treesitter!
(use-package expreg  
  :bind (("C-M-SPC" . expreg-expand)
         ("M-SPC" . expreg-contract)))

;;; ----------------------------------------------------------------------------

(use-package dirvish 
  :init
  (add-to-list 'load-path 
               (expand-file-name "extensions" 
                                 (file-name-directory (locate-library "dirvish"))))

  :hook
  (after-init . dirvish-override-dired-mode)
  (after-init . dirvish-peek-mode)  

  :custom
  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (dirvish-large-directory-threshold 20000)
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("s" "~/code/streamline"           "Streamline")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))

  (dirvish-mode-line-format
   '(:left (sort symlink) :right (omit yank index)))

  (dirvish-attributes           ; The order *MATTERS* for some attributes
   '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size))

  (dirvish-side-attributes
   '(vc-state nerd-icons collapse file-size))

  :config
  (require 'dirvish-vc)
  (require 'dirvish-fd)
  (require 'dirvish-peek)
  (require 'dirvish-icons)
  (require 'dirvish-extras)
  (require 'dirvish-quick-access)
  
  (custom-set-faces
   '(dired-directory ((t (:foreground "#8CD0D3" :weight bold)))))

  
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dired-cheatsheet)  ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

;;; ----------------------------------------------------------------------------

(use-package nix-ts-mode
  :mode (("\\.nix\\'" . nix-ts-mode)))

;;; ----------------------------------------------------------------------------

(use-package elixir-ts-mode)

(use-package mix
  :hook (elixir-ts-mode . mix-minor-mode))

(use-package exunit
  :after elixir-ts-mode
  :bind-keymap ("C-c e" . exunit-mode-map)
  :bind
  (:map exunit-mode-map
        ("." . exunit-verify-single)
        ("s" . exunit-verify-single)
        ("f" . exunit-verify) ;; run all tests in the current file
        ("p" . exunit-verify-all) ;; run all tests in the project
        ("r" . exunit-rerun)
        ("T" . exunit-toggle-file-and-test)
        ("t" . exunit-toggle-file-and-test-other-window)))

;;; ----------------------------------------------------------------------------

(use-package typescript-ts-mode)

;;; ----------------------------------------------------------------------------

(use-package jtsx
  :mode (("\\.jsx\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.js\\'" . jtsx-typescript-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))

  :commands jtsx-install-treesit-language)


;;; ----------------------------------------------------------------------------

(use-package json-ts-mode
  :mode (("\\.json\\'" . json-ts-mode)))

;;; ----------------------------------------------------------------------------

(use-package dockerfile-ts-mode
  :mode (("Dockerfile" . dockerfile-ts-mode)))

;;; ----------------------------------------------------------------------------

(use-package yaml-ts-mode
  :mode (("\\.ya?ml\\'" . yaml-ts-mode)))

;;; ----------------------------------------------------------------------------

(use-package magit
  :custom
  (magit-list-refs-sortby "-creatordate")
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration))

;; NOTE - not really using forge right now, but keeping this here for future reference
;; (use-package forge
;;   :after magit
;;   :config
;;   (setq auth-sources '("~/.authinfo.gpg"))
;;   (setq magit-list-refs-sortby "-creatordate"))

;;; ----------------------------------------------------------------------------

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

;;; ----------------------------------------------------------------------------

(use-package hl-todo
  
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   '(("TODO"   . (:foreground "#FF6C6B" :weight bold))
     ("FIXME"  . (:foreground "#ECBE7B" :weight bold))
     ("DEBUG"  . (:foreground "#51AFEF" :weight bold))
     ("GOTCHA" . (:foreground "" :weight bold))
     ("HACK"  . (:foreground "#a6e3a1" :weight bold))
     ("NOTE"  . (:foreground ,(face-attribute 'font-lock-comment-face :foreground nil t) :weight bold)))))

;;; ----------------------------------------------------------------------------

(use-package apheleia
  :defer t
  :commands (apheleia-mode
             apheleia-global-mode)
  :hook ((prog-mode . apheleia-mode)))

;;; ----------------------------------------------------------------------------

(use-package uniquify
  :ensure nil ; built-in
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;;; ----------------------------------------------------------------------------

(use-package minions
  :init (minions-mode 1))

;;; ----------------------------------------------------------------------------

(use-package doom-modeline
  :after minions
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-project-name t)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-position-column-line-format '("%c"))
  (doom-modeline-percent-position nil))

;;; ----------------------------------------------------------------------------

(use-package nyan-mode
  :after doom-modeline
  :config
  (nyan-mode 1)
  :custom
  (nyan-minimum-window-width 90)
  (nyan-bar-length 20))

(provide 'post-init)
;;; post-init.el ends here
