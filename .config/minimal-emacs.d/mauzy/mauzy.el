;;; mauzy.el --- Custom utilities for Emacs -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Main entry point for mauzy utilities.
;; Loads all submodules.

;;; Code:

(defgroup mauzy nil
  "Custom utilities for Emacs setup."
  :group 'environment
  :prefix "mauzy/")

;; Load submodules
(require 'mauzy-eglot)
(require 'mauzy-project)
(require 'mauzy-search)
(require 'mauzy-vterm)
(require 'mauzy-scratch)
(require 'mauzy-splash)
(require 'mauzy-vertico)
(require 'mauzy-modal)
(require 'mauzy-downcase-only-mode)

(provide 'mauzy)
;;; mauzy.el ends here
