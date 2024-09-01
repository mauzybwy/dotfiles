;;; conf/lang.conf.el --- languages config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
  (define-innermode poly-deno-ts-sql-innermode
    :mode 'sql-mode
    :head-matcher "sql`"
    :tail-matcher "`;"
    :head-mode 'host
    :tail-mode 'host)
  (define-polymode poly-deno-ts-mode
    :hostmode 'poly-deno-ts-hostmode
    :innermodes '(poly-deno-ts-sql-innermode))
  )


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
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src")
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
;;;; Eglot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eglot-add-server (s)
  (add-to-list 'eglot-server-programs s)
  (message "Added %s to eglot-server-programs" s)
  ;; (message "eglot-server-programs: %s" eglot-server-programs)
  (eglot-ensure)
  )

(use-package! eglot
  :config
  (eglot-booster-mode)
  ;; (setq eglot-events-buffer-size 0)
  ;; (fset #'jsonrpc--log-event #'ignore)
  (mauzy/add-to-list-multiple
   'eglot-server-programs
   '(((js2-mode :language-id "javascript") "typescript-language-server" "--stdio")
     ((rjsx-mode :language-id "typescriptreact") "typescript-language-server" "--stdio")
     ;; ((deno-ts-mode :language-id "typescript") "deno" "lsp")
     ;; ((deno-tsx-ts-mode :language-id "typescriptreact") "deno" "lsp")
     )))

(after! rjsx-mode (set-company-backend! 'rjsx-mode '(company-files)))


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
  (add-hook! jtsx-jsx-mode
    (eglot-add-server
     '((jtsx-jsx-mode :language-id "javascript")
       "typescript-language-server" "--stdio"))))

;; Typescript
(use-package! jtsx-typescript-mode
  :mode "\\.[mc]?ts\\'"
  :init
  (add-hook! jtsx-typescript-mode
    (eglot-add-server
     '((jtsx-typescript-mode :language-id "typescript")
       "typescript-language-server" "--stdio"))))

;; Typescript[TSX]
(use-package! jtsx-tsx-mode
  :mode "\\.tsx\\'"
  :init
  (add-hook! jtsx-tsx-mode
    (eglot-add-server
     '((jtsx-tsx-mode :language-id "typescriptreact")
       "typescript-language-server" "--stdio"
       :initializationOptions
       (:preferences
        (
         ;; :includeInlayFunctionParameterTypeHints t
         ;; :includeInlayFunctionLikeReturnTypeHints t
         :allowRenameOfImportPath t
         :lint t
         )
        )
       ))))

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

;; Python
(use-package! python-ts-mode
  :mode "\\.py[iw]?\\'"
  :init
  (add-hook! python-ts-mode
    (setq python-indent-offset 4)
    (add-to-list
     'eglot-server-programs
     '(python-ts-mode "pyright-langserver" "--stdio"))
    (eglot-ensure)))

;; SQL
(use-package! sqlformat
  :config
  (setq sqlformat-command 'sqlfluff)
  (setq sqlformat-args '("--config" "/Users/mauzy/.sqlfluff.postgres.cfg"))
  ;; (setq sqlformat-command 'pgformatter)
  ;;(setq sqlformat-args '("-s2" "-g" "-w80" "-W1"))
  )
(use-package! sql-mode
  :mode "\\.sql\\'"
  :init
  (defvar my/eglot/sqls/current-connection nil)
  (defvar my/eglot/sqls/current-database nil)

  (cl-defmethod eglot-execute
    :around
    (server action)

    (pcase (plist-get action :command)
      ("executeQuery"
       (if (use-region-p)
           (let* ((begin (region-beginning))
                  (end (region-end))
                  (begin-lsp (eglot--pos-to-lsp-position begin))
                  (end-lsp (eglot--pos-to-lsp-position end))
                  (action (plist-put action :range `(:start ,begin-lsp :end ,end-lsp)))
                  (result (cl-call-next-method server action)))
             (my/eglot/sqls/show-result result))
         (message "No region")))

      ((or
        "showConnections"
        "showDatabases"
        "showSchemas"
        "showTables")
       (my/eglot/sqls/show-result (cl-call-next-method)))

      ("switchConnections"
       (let* ((connections (eglot--request server :workspace/executeCommand
                                           '(:command "showConnections")))
              (collection (split-string connections "\n"))
              (connection (completing-read "Switch to connection: " collection nil t))
              (index (number-to-string (string-to-number connection)))
              (action (plist-put action :arguments (vector index))))
         (cl-call-next-method server action)
         (setq my/eglot/sqls/current-connection connection)))

      ("switchDatabase"
       (let* ((databases (eglot--request server :workspace/executeCommand
                                         '(:command "showDatabases")))
              (collection (split-string databases "\n"))
              (database (completing-read "Switch to database: " collection nil t))
              (action (plist-put action :arguments (vector database))))
         (cl-call-next-method server action)
         (setq my/eglot/sqls/current-database database)))

      (_
       (cl-call-next-method))))

  (defun my/eglot/sqls/show-result (result)
    (with-current-buffer (get-buffer-create "*sqls result*")
      (setq-local header-line-format
                  '(:eval (my/eglot/sqls/show-result/header-line-format)))
      (erase-buffer)
      (insert result)
      (display-buffer (current-buffer))))

  (defun my/eglot/sqls/show-result/header-line-format ()
    (let* ((connection (or my/eglot/sqls/current-connection ""))
           (parts (split-string connection " "))
           (driver (nth 1 parts))
           (alias (nth 2 parts))
           (result (format "[%s] %s/%s"
                           (or driver "?")
                           (or alias "?")
                           (or my/eglot/sqls/current-database "?"))))
      (propertize result
                  'face 'my/eglot/sqls/show-result/header-line-face)))

  (defface my/eglot/sqls/show-result/header-line-face
    '((t (:inherit 'magit-header-line)))
    "*sqls result* header-line face")
  (add-hook! sql-mode
    (add-to-list
     'eglot-server-programs
     '(sql-mode "/Users/mauzy/go/bin/sqls" "--trace"))
    (eglot-ensure)
    (sqlformat-on-save-mode))
  )

(use-package! jinja2-mode
  :mode "\\.jinja\\'")

;; fsharp
(use-package! fsharp-mode
  :defer t)
(use-package! eglot-fsharp)

;; Nix
(use-package! poly-nix-mode
  :mode "\\.nix\\'"
  :init
  (set-formatter! 'alejandra '("alejandra" "--quiet") :modes '(nix-mode))
  )


;; (after! sql
;;   (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; lang.conf.el ends here
