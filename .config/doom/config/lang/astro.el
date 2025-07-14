;;; config/lang/astro.el -*- lexical-binding: t; -*-

(use-package! astro-ts-mode
  :mode "\\.astro\\'"
  :init
  ;; NOTE: loading this mode is causing strange unrelated HEEX render issues
  ;;(load! "../pkgs/astro-ts-mode.el")
  (set-formatter! 'prettier-astro
    '("pnpx" "prettier" "--parser=astro"
      (apheleia-formatters-indent "--use-tabs" "--tab-width" 'astro-ts-mode-indent-offset))
    :modes '(astro-ts-mode))
  (add-hook! astro-ts-mode
    (mauzy/eglot-add-server
     '(astro-ts-mode "astro-ls" "--stdio"
       :initializationOptions
       (:typescript (:tsdk "/Users/mauzy/Library/pnpm/global/5/node_modules/typescript/lib/"))))))


;;; config/lang/astro.el ends here
