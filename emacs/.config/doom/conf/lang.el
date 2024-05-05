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
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (python "https://github.com/tree-sitter/tree-sitter-python")
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

(set-formatter! 'prettier-astro
  '("npx" "prettier" "--parser=astro"
    (apheleia-formatters-indent "--use-tabs" "--tab-width" 'astro-ts-mode-indent-offset))
  :modes '(astro-ts-mode))

;; Eglot Server overrides

;; (add-to-list 'eglot-server-programs '((js-mode typescript-mode (typescript-ts-base-mode :language-id "typescript")) . (eglot-deno "deno" "lsp")))
;; (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))

;; (use-package! tsx-mode
;;   :hook (typescript-mode . tsx-mode)
;;   :config
;;   ;; Additional configuration if needed
;;   )


;;;;;
;;;;; Web Dev (js/ts/html/css/etc)
;;;;;

;; (use-package! rjsx-mode
;;   :mode ())

;; (use-package! tsx-mode
;;   :after (eglot)
;;   :config
;;   (setq auto-mode-alist (delete '("\\.tsx\\'" . typescript-tsx-mode) auto-mode-alist))
;;   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-mode))
;;   )


;;;;;; Astro
;; (define-derived-mode astro-mode web-mode "Astro 🚀")
(load! "../pkgs/astro-ts-mode.el")

;; (use-package! lsp-tailwindcss
;;   :when (modulep! +lsp)
;;   :init
;;   (setq! lsp-tailwindcss-add-on-mode t)
;;   :config
;;   (add-to-list 'lsp-tailwindcss-major-modes 'astro-ts-mode))


;;;;;; Deno

(use-package! deno-ts-mode
  :ensure t)

(defclass eglot-deno (eglot-lsp-server) ()
  :documentation "A custom class for deno lsp.")

(cl-defmethod eglot-initialization-options ((server eglot-deno))
  "Passes through required deno initialization options"
  (list :enable nil
        :suggest.names t
        :suggest.autoImports t
        :suggest.imports.autoDiscover t
        :lint t))


(add-to-list 'auto-mode-alist '("\\.g\\'" . gnuplot-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Eglot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode javascript-ts-mode typescript-ts-mode "Javascript")

(use-package! eglot
  ;; :ensure t
  :hook
  '((deno-ts-mode . eglot-ensure)
    (deno-tsx-ts-mode . eglot-ensure)
    (js2-mode . eglot-ensure)
    (js-ts-mode . eglot-ensure)
    (tsx-ts-mode . eglot-ensure)
    (rjsx-mode . eglot-ensure)
    (astro-ts-mode . eglot-ensure)
    (typescript-ts-mode . eglot-ensure))

  :config
  (mauzy/add-to-list-multiple
   'eglot-server-programs
   '(((js2-mode :language-id "javascript") "typescript-language-server" "--stdio")
     ;; ((ts2-mode :language-id "typescript") "vtsls" "--stdio")
     ;; ((rjsx-mode :language-id "typescriptreact") "vtsls" "--stdio"))
     ((rjsx-mode :language-id "typescriptreact") "typescript-language-server" "--stdio")
     ((javascript-ts-mode :language-id "javascript") "typescript-language-server" "--stdio")
     ((typescript-ts-mode :language-id "typescript") "typescript-language-server" "--stdio")
     ((tsx-ts-mode :language-id "typescriptreact") "typescript-language-server" "--stdio")
     ((deno-ts-mode :language-id "typescript") "deno" "lsp")
     ((deno-tsx-ts-mode :language-id "typescriptreact") "deno" "lsp")
     (astro-ts-mode "astro-ls" "--stdio"
                    :initializationOptions
                    (:typescript (:tsdk "/Users/mauzy/Library/pnpm/global/5/node_modules/typescript/lib/")))
     )))

(use-package! fsharp-mode
  :defer t)
(use-package! eglot-fsharp)
;; (use-package! eglot-fsharp
;;   :config
;;   (setq eglot-fsharp-fsautocomplete-args
;;         '(:automaticWorkspaceInit t
;;           :abstractClassStubGeneration t
;;           :abstractClassStubGenerationMethodBody
;;           "failwith \"Not Implemented\""
;;           :abstractClassStubGenerationObjectIdentifier "this"
;;           :addFsiWatcher nil
;;           :codeLenses (:references (:enabled t)
;;                        :signature (:enabled t))
;;           :disableFailedProjectNotifications nil
;;           :dotnetRoot ""
;;           :enableAdaptiveLspServer t
;;           :enableAnalyzers nil
;;           :enableMSBuildProjectGraph nil
;;           :enableReferenceCodeLens t
;;           :excludeProjectDirectories [".git" "paket-files" ".fable" "packages" "node_modules"]
;;           :externalAutocomplete nil
;;           :fsac (:attachDebugger nil
;;                  :cachedTypeCheckCount 200
;;                  :conserveMemory nil
;;                  :dotnetArgs nil
;;                  :netCoreDllPath ""
;;                  :parallelReferenceResolution nil
;;                  :silencedLogs nil)
;;           :fsiExtraParameters nil
;;           :fsiSdkFilePath ""
;;           :generateBinlog nil
;;           :indentationSize 4
;;           :inlayHints (:disableLongTooltip t
;;                        :enabled :json-false
;;                        :parameterNames :json-false
;;                        :typeAnnotations :json-false)
;;           :inlineValues (:enabled t
;;                          :prefix "//")
;;           :interfaceStubGeneration t
;;           :interfaceStubGenerationMethodBody "failwith \"Not Implemented\""
;;           :interfaceStubGenerationObjectIdentifier "this"
;;           :keywordsAutocomplete t
;;           :lineLens (:enabled "always"
;;                      :prefix " // ")
;;           :linter t
;;           :pipelineHints (:enabled t
;;                           :prefix " // ")
;;           :recordStubGeneration t
;;           :recordStubGenerationBody "failwith \"Not Implemented\""
;;           :resolveNamespaces t
;;           :saveOnSendLastSelection nil
;;           :simplifyNameAnalyzer t
;;           :smartIndent nil
;;           :suggestGitignore t
;;           :suggestSdkScripts t
;;           :unionCaseStubGeneration t
;;           :unionCaseStubGenerationBody "failwith \"Not Implemented\""
;;           :unusedDeclarationsAnalyzer t
;;           :unusedOpensAnalyzer t
;;           :verboseLogging t
;;           :workspaceModePeekDeepLevel 4
;;           :workspacePath ""))
;;   )


;; (use-package! eglot-fsharp
;;   :config
;;   (setq eglot-fsharp-fsautocomplete-args
;;         '(:codeLenses (:references (:enabled t)
;;                        :signature (:enabled t))
;;           :indentationSize 4
;;           :inlayHints (:disableLongTooltip t
;;                        :enabled nil
;;                        :parameterNames t
;;                        :typeAnnotations nil)
;;           :lineLens (:enabled "replaceCodeLens"
;;                      :prefix " // ")
;;           :verboseLogging t
;;           :workspaceModePeekDeepLevel 4
;;           :workspacePath ""))
;;   )


(after! rjsx-mode (set-company-backend! 'rjsx-mode '(company-files)))

;;; lang.conf.el ends here


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))
(setq auto-mode-alist
      (append '(("\\.[jt]sx\\'" . rjsx-mode)
                ("\\.[mc]?js\\'" . javascript-ts-mode)
                ("\\.[mc]?ts\\'" . typescript-ts-mode)
                ("\\.el\\'" . emacs-lisp-mode)
                (".*\\.astro\\'" . astro-ts-mode))
              auto-mode-alist))

(use-package! sqlformat
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-g")))

(after! sql
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))
