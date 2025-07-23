;;; config/tools/lsp-mode.el -*- lexical-binding: t; -*-

(use-package! lsp-mode
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

  ;; typescript
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

;;; config/tools/lsp-mode.el ends here
