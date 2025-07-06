;;; conf/lang.conf.el --- languages config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Treesitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! treesit
  :demand
  :init
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

;; (use-package! treesit-auto
;;   :after treesit
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   ;; (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Prettier
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! apheleia
  :config
  (mauzy/add-to-list-multiple
   'apheleia-mode-alist
   '((rjsx-mode . prettier-typescript))))

;; (use-package! lsp-tailwindcss
;;   :when (modulep! +lsp)
;;   :init
;;   (setq! lsp-tailwindcss-add-on-mode t)
;;   :config
;;   (add-to-list 'lsp-tailwindcss-major-modes 'astro-ts-mode))


(add-to-list 'auto-mode-alist '("\\.g\\'" . gnuplot-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :init
  ;; (setq lsp-use-plists t)
  ;; (defun my/lsp-mode-setup-completion ()
  ;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
  ;;         '(orderless))) ;; Configure orderless

  :config
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection '("vscode-eslint-language-server --stdio"))
  ;;                   :major-modes '(jtsx-tsx-mode jtsx-jsx-mode jtsx-typescript-mode)
  ;;                   :priority -1
  ;;                   :server-id 'eslint-ls))

  ;; :hook
  ;; (lsp-completion-mode . my/lsp-mode-setup-completion)

  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; we use Corfu!
  (lsp-diagnostics-provider :flymake)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'

  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big

  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet nil)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind nil)                   ; Optional

  ;; headerline
  (lsp-headerline-breadcrumb-enable nil)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)

  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable t)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  ;;(lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting

  ;; ruby
  (lsp-solargraph-use-bundler t)        ; Use bundler to find solargraph
  (lsp-ruby-lsp-use-bundler t)         ; Use bundler to find ruby-lsp

  ;; lens
  ;;(lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

  ;; Typescript
  (lsp-clients-typescript-prefer-use-project-ts-server t)

  ;; DISABLED
  (lsp-disabled-clients
   '(
     ;; ruby-lsp-ls ; ruby-lsp ( when in use, make sure the :add-on patch is in place )
     ;; ruby-ls ; solargraph
     typeprof-ls
     rubocop-ls
     ruby-lsp-mzy-ls
     semgrep-ls
     ))

  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Eglot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eglot-add-server (s)
  (add-to-list 'eglot-server-programs s)
  (message "Added %s to eglot-server-programs" s)
  ;; (message "eglot-server-programs: %s" eglot-server-programs)
  (eglot-ensure)
  )

(use-package! eglot
  :defer t
  :hook (elixir-ts-mode eglot-ensure)
  
  :config
  (eglot-booster-mode)
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "elixir-ls"))
  ;; (setq completion-category-overrides '((eglot (styles orderless))
  ;;                                       (eglot-capf (styles orderless))))
  ;; (setq eglot-events-buffer-config '(:size 200 :format full))
  ;; (fset #'jsonrpc--log-event #'ignore)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist
      (append '(("\\.el\\'" . emacs-lisp-mode))
              auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Specific
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Prog Mode
(add-hook! prog-mode
           ;; (avy-linum-mode)
           (rainbow-delimiters-mode))

;; Elisp
;;(use-package! elisp-mode
;;  :mode "\\.el\\'")

;; Typescript
(use-package! json-ts-mode
  :mode "\\.json\\'"
  :init
  (add-hook! json-ts-mode
    (setq json-ts-mode-indent-offset 4)))

;; Javascript
(use-package! jtsx-jsx-mode
  :mode "\\.[mc]?jsx?\\'"
  :init
  (add-hook!
   jtsx-jsx-mode
   #'lsp
   ;; (eglot-add-server
   ;;  '((jtsx-jsx-mode :language-id "javascript")
   ;;    "typescript-language-server" "--stdio"))
   ))

;; Typescript
(use-package! jtsx-typescript-mode
  :mode "\\.[mc]?ts\\'"
  :init
  (add-hook!
   jtsx-typescript-mode
   #'lsp
   ;; (eglot-add-server
   ;;  '((jtsx-typescript-mode :language-id "typescript")
   ;;    "typescript-language-server" "--stdio"))
   ))

;; Typescript[TSX]
(use-package! jtsx-tsx-mode
  :mode "\\.tsx\\'"
  :init
  (add-hook!
   jtsx-tsx-mode
   #'lsp
   ;; (eglot-add-server
   ;;  '((jtsx-tsx-mode :language-id "typescriptreact")
   ;;    "typescript-language-server" "--stdio"
   ;;    :initializationOptions
   ;;    (:preferences
   ;;     (
   ;;      ;; :includeInlayFunctionParameterTypeHints t
   ;;      ;; :includeInlayFunctionLikeReturnTypeHints t
   ;;      :allowRenameOfImportPath t
   ;;      :lint t
   ;;      )
   ;;     )
   ;;    ))
   ))

;; Deno
(use-package! deno-ts-mode
  :init
  (add-hook! deno-ts-mode
    (eglot-add-server
     '((deno-ts-mode :language-id "typescript")
       "deno" "lsp"
       :initializationOptions
       (
        :enable t
        :suggest.names t
        :suggest.autoImports t
        :suggest.imports.autoDiscover t
        ;; :internalDebug t
        :lint t
        )
       ))))


;; Deno [TSX]
;; (use-package! deno-tsx-ts-mode
;;   :init
;;   (add-hook! deno-tsx-ts-mode
;;     (eglot-add-server
;;      '((deno-ts-mode :language-id "typescriptreact")
;;        "deno" "lsp"
;;        :initializationOptions
;;        (
;;         :enable t
;;         :suggest.names t
;;         :suggest.autoImports t
;;         :suggest.imports.autoDiscover t
;;         ;; :internalDebug t
;;         :lint t
;;         )
;;        ))))

;; Ruby
(use-package! ruby-ts-mode
  :init
  (add-hook!
   ruby-ts-mode
   #'lsp
   (setq format-all-formatters
         '(("Ruby" (rubocop))))
   (format-all-mode)
   (minitest-mode)
   (apheleia-mode -1)))

(use-package! minitest
  :config
  (setq minitest--test-regexps
        '("\\(test\\) ['\"]\\([^\"]+?\\)['\"]"
          "\\(test\\)(['\"]\\([^\"]+?\\)['\"])"
          "def \\(test\\)_\\([_A-Za-z0-9]+\\)"
          "\\(it\\) \"\\([^\"]+?\\)\""
          "\\(it\\) '\\([^\"]+?\\)'")))

;; ------------------------------------------------------------------------------
;; NOTE - make sure you set up a dir-locals.el with the enabled paths
;; ------------------------------------------------------------------------------
;; ((deno-ts-mode
;;   . ((eglot-workspace-configuration
;;       . (:deno (
;;                 :enablePaths ["./supabase/functions"]
;;                 :importMap "./supabase/functions/import_map.json"
;;                 ))))))
;; ------------------------------------------------------------------------------

;; ------------------------------------------------------------------------------
;; NOTE - if the project has Node AND Deno, add this to the Deno directory
;; ------------------------------------------------------------------------------
;; ((auto-mode-alist . (("\\.[mc]?[jt]s\\'" . deno-ts-mode)
;;                      ("\\.[jt]sx\\'" . deno-tsx-ts-mode))))
;; ------------------------------------------------------------------------------

;; Astro
(load! "../pkgs/astro-ts-mode.el")
(use-package! astro-ts-mode
  :mode "\\.astro\\'"
  :init
  (set-formatter! 'prettier-astro
    '("pnpx" "prettier" "--parser=astro"
      (apheleia-formatters-indent "--use-tabs" "--tab-width" 'astro-ts-mode-indent-offset))
    :modes '(astro-ts-mode))
  (add-hook! astro-ts-mode
    (eglot-add-server
     '(astro-ts-mode "astro-ls" "--stdio"
       :initializationOptions
       (:typescript (:tsdk "/Users/mauzy/Library/pnpm/global/5/node_modules/typescript/lib/"))))))

;; jq
(use-package! jq-ts-mode
  :mode "\\.jq\\'"
  :init
  (add-hook! jq-ts-mode
    (eglot-add-server
     '(jq-ts-mode "jq-lsp"))))

(use-package! elixir-ts-mode)
;; (use-package! elixir-ts-mode
;;   :init
;;   (add-hook! elixir-ts-mode
;;     (eglot-add-server
;;      '(elixir-ts-mode "elixir-ls"))))

;; SQL
(use-package! sqlformat
  :config
  (setq sqlformat-command 'sqlfluff)
  (setq sqlformat-args '("--config" "/Users/mauzy/.sqlfluff.postgres.cfg")))

(use-package! jinja2-mode
  :mode "\\.jinja\\'")

;; fsharp
(use-package! fsharp-mode
  :defer t)

;; Nix
(use-package! poly-nix-mode
  :mode "\\.nix\\'"
  :init
  (set-formatter! 'alejandra '("alejandra" "--quiet") :modes '(nix-mode))
  )


;; (after! sql
;;   (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Polymode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ensure polymode is loaded
(use-package! polymode
  :init

  ;; nix
  (define-hostmode poly-nix-hostmode :mode 'nix-ts-mode)
  (define-innermode poly-nix-shell-innermode
    :mode 'bash-ts-mode
    :head-matcher "= '' #sh"
    :tail-matcher "'';"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-nix-mode
    :hostmode 'poly-nix-hostmode
    :innermodes '(poly-nix-shell-innermode))

  ;; typescript
  (define-hostmode poly-deno-ts-hostmode :mode 'deno-ts-mode)
  (define-innermode poly-typescript-template-innermode
    :mode 'deno-ts-mode
    :head-matcher "\\${"
    :tail-matcher "}"
    :allow-nested t
    :keep-in-mode 'host
    :fallback-mode 'host
    :head-mode 'host
    :tail-mode 'host)
  (define-innermode poly-deno-ts-sql-innermode
    :mode 'sql-mode
    :head-matcher "sql\\(<.*>\\)?`"
    :tail-matcher "`;"
    :allow-nested t
    :keep-in-mode 'host
    :fallback-mode 'host
    :head-mode 'host
    :tail-mode 'host)
  (define-innermode poly-deno-ts-sql-manual-innermode
    :mode 'sql-mode
    :head-matcher "--sql_start"
    :tail-matcher "--sql_end"
    :allow-nested t
    :keep-in-mode 'host
    :fallback-mode 'host
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-deno-ts-mode
    :hostmode 'poly-deno-ts-hostmode
    :innermodes
    '(poly-deno-ts-sql-innermode
      poly-deno-ts-sql-manual-innermode
      poly-typescript-template-innermode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; lang.conf.el ends here
