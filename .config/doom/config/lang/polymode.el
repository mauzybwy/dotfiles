;;; config/lang/polymode.el -*- lexical-binding: t; -*-

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
    :innermodes '(poly-nix-shell-innermode)))

;;; config/lang/polymode.el ends here
