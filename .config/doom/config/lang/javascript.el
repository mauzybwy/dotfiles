;;; config/lang/javascript.el -*- lexical-binding: t; -*-

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

;;; config/lang/javascript.el ends here
