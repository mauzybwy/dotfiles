;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Code:

(load (expand-file-name "config/vertico" user-emacs-directory))
(load (expand-file-name "config/theme" user-emacs-directory))

;;; ---------------------------------------------------------------------------

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

(use-package wgrep
  :ensure t)

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
         ("C-c C-e" . mauzy/consult-export-to-wgrep)
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

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

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
                     (wgrep-change-to-wgrep-mode))))))

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

(use-package vterm
  :ensure t)

(use-package multi-vterm
  :ensure t)

;;; ----------------------------------------------------------------------------

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-/"))
  (global-set-key (kbd "C-/")   'undo-fu-only-undo)
  (global-unset-key (kbd "M-/"))
  (global-set-key (kbd "M-/") 'undo-fu-only-redo))

;;; ----------------------------------------------------------------------------

(use-package undo-fu-session
  :defer t
  :commands undo-fu-session-global-mode
  :hook (after-init . undo-fu-session-global-mode))

;;; ----------------------------------------------------------------------------

(use-package vundo
  :commands (vundo)
  :bind (("C-M-/" . vundo))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

;;; ----------------------------------------------------------------------------

(use-package mwim
  :ensure t

  :bind (("C-a" . mwim-beginning)
         ("C-e" . mwim-end)))

;;; ----------------------------------------------------------------------------

(use-package emacs
  :init
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta))

  :bind
  (("C-c l e b" . eval-buffer)
   ("C-h F" . describe-face)
   ("C-c r" . revert-buffer))
  
  :hook
  ;; init hooks
  (after-init . global-auto-revert-mode)
  (after-init . recentf-mode)
  (after-init . savehist-mode)
  (after-init . save-place-mode)
  (after-init . electric-pair-mode)
  (after-init . show-paren-mode)

  ;; prog hooks
  (prog-mode . display-line-numbers-mode)

  ;; cleanup hooks
  (kill-emacs . recentf-cleanup)

  :custom
  (font-lock-maximum-decoration t)
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
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  
  ;; Display all starred buffers at the bottom
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-name action)
                   (and (string-match-p "\\*.*\\*" buffer-name)
                        (not (string-match-p "\\*dirvish" buffer-name))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 20)
                 (reusable-frames . visible))
               t))

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
  :bind
  (("C-c l s r" . eglot-rename)
   ("C-c l s d" . eldoc-doc-buffer)
   ("C-c l s D" . eldoc-box-eglot-help-at-point))
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

(use-package eldoc-box
  :ensure t)

(use-package project
  :ensure t

  :bind
  ("C-x p v" . magit-project-status)
  ("C-x p s" . mauzy/consult-ripgrep-with-region)
  ("C-x p S" . mauzy/consult-ripgrep-here)

  :custom
  (project-switch-commands
   '(
     (project-find-file "Find file")
     (consult-ripgrep "Search (ripgrep)")
     (consult-project-buffer "Find buffer")
     (magit-project-status "VC: magit")
     (project-find-dir "Find directory")
     (project-eshell "Eshell")
     (project-any-command "Other"))))

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

(use-package multiple-cursors
  :ensure t
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

(use-package expand-region
  :ensure t
  :bind (("M-SPC" . er/expand-region)
         ("C-M-SPC" . er/contract-region))
  :config
  (setq expand-region-fast-keys-enabled t)
  (setq er/try-expand-list
        '(er/mark-word
          er/mark-symbol
          er/mark-inside-quotes
          er/mark-outside-quotes
          er/mark-inside-pairs
          er/mark-outside-pairs
          er/mark-method-call
          er/mark-comment
          er/mark-defun)))

;;; ----------------------------------------------------------------------------

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  (add-to-list 'load-path 
               (expand-file-name "extensions" 
                                 (file-name-directory (locate-library "dirvish"))))

  :config
  (require 'dirvish-vc)
  (require 'dirvish-fd)
  (require 'dirvish-icons)
  (require 'dirvish-extras)
  (require 'dirvish-quick-access)
  
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("s" "~/code/streamline"           "Streamline")
     ("m" "/mnt/"                       "Drives")
     ("t" "~/.local/share/Trash/files/" "TrashCan")))
  :config
  (custom-set-faces
   '(dired-directory ((t (:foreground "#8CD0D3" :weight bold)))))
  
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (setq dirvish-large-directory-threshold 20000)
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

(use-package nix-ts-mode)

;;; ----------------------------------------------------------------------------

(use-package elixir-ts-mode)

(use-package
  exunit
  :diminish t
  :bind
  (("C-c e ." . exunit-verify-single)
   ("C-c e ." . exunit-debug)
   ("C-c e b" . exunit-verify) ;; buffer-only
   ("C-c e a" . exunit-verify-all)
   ("C-c e l" . exunit-rerun)))

;;; ----------------------------------------------------------------------------

(use-package typescript-ts-mode)

;;; ----------------------------------------------------------------------------

(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode)))

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

;; NOTE - not really using forge right now, but keeping this here for future reference
;; (use-package forge
;;   :after magit
;;   :config
;;   (setq auth-sources '("~/.authinfo.gpg"))
;;   (setq magit-list-refs-sortby "-creatordate"))

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

(use-package nyan-mode
  :config
  (nyan-mode 1))

(provide 'post-init)
;;; post-init.el ends here
