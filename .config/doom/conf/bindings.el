;;; conf/bindings.el -*- lexical-binding: t; -*-

;;; Meta Key
(setq mac-command-modifier 'meta)

;;; Mega-Overrides
(map! :map 'override-global-map
      "C-c p s" #'+vertico/project-search
      "C-c p S" #'+vertico/project-search-from-cwd
      "C-c f r" #'rename-visited-file
      "C-;" #'avy-goto-char-timer
      )

;;; More overrides
(map! "C-x C-b" #'+vertico/switch-workspace-buffer

      "C-s" #'+default/search-buffer
      ;; "C-s" #'+vertico/search-symbol-at-point

      "M-/" #'undo-fu-only-redo
      "M-q" #'query-replace
      ;; "M-." #'+lookup/implementations

      ;; C-c commands
      "C-c r" #'revert-buffer
      "C-c p g" #'projectile-vc
      "C-c p v" #'projectile-run-vterm
      "C-c j q" #'counsel-jq

      ;; Motion commands
      "M-p" #'beginning-of-defun
      "M-n" #'end-of-defun
      )

;;; Mode overrides
(map! :after rjsx-mode
      :map rjsx-mode-map
      "M-." #'+lookup/implementations)

(use-package! dirvish
  :init
  (dirvish-override-dired-mode)
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (;; ("C-x C-f" . dirvish)
   :map dirvish-mode-map
   ;; ("f" . dirvish-fd)
   ;; ("DEL" . dired-tree-up)
   ))
