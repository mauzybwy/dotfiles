;;; config/lang/nix.el -*- lexical-binding: t; -*-

(use-package! poly-nix-mode
  :mode "\\.nix\\'"
  :init
  (set-formatter! 'alejandra '("alejandra" "--quiet") :modes '(nix-mode))
  )

;;; config/lang/nix.el ends here
