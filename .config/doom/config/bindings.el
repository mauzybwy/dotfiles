;;; config/bindings.el -*- lexical-binding: t; -*-

;;; Meta Key
(setq mac-command-modifier 'meta)

(undefine-key! general-override-mode-map "C-c p F" "C-c p S" "C-c p s")

;;; Mega-Overrides
(map! :map 'override-global-map
      "C-c f r" #'rename-visited-file
      "C-x C-;" #'comment-dwim
      "C-c m s" #'mc/mark-all-symbols-like-this

      ;; Navigation
      ;; "M-f" #'forward-to-word
      ;; "M-b" #'backward-to-word
      "M-f" #'forward-word
      "M-b" #'backward-to-word

      "M-<backspace>" #'doom/delete-backward-word
      )

(map! "C-;" #'avy-goto-word-0)
(map! "M-;" #'avy-goto-line)
(map! "C-M-;" #'avy-goto-char-timer)


;;; More overrides
(map! "C-x C-b" #'+vertico/switch-workspace-buffer

      "C-s" #'+default/search-buffer

      "M-/" #'undo-fu-only-redo
      "M-q" #'query-replace

      ;; C-c commands
      "C-c r" #'revert-buffer
      "C-c j q" #'counsel-jq

      ;; Motion commands
      "M-p" #'beginning-of-defun
      "M-n" #'end-of-defun
      )

;;; Mode overrides
(map! :after rjsx-mode
      :map rjsx-mode-map
      "M-." #'+lookup/implementations)

;;; Mode overrides
(map! :after lsp-mode
      :map lsp-mode-map
      "C-c l d" #'lsp-ui-doc-toggle)

(use-package! dirvish
  :init
  (dirvish-override-dired-mode)
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (;; ("C-x C-f" . dirvish)
   :map dirvish-mode-map
   ;; ("f" . dirvish-fd)
   ;; ("DEL" . dired-tree-up)
   ))
