;;; config/org.el -*- lexical-binding: t; -*-
;;; Code:

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(after! org
  (set-company-backend! 'org-mode nil)
  (set-company-backend! 'org-mode '(:separate company-yasnippet company-capf)))

(defun mauzy/set-margins ()
  "Set margins in current buffer."
  (text-scale-set 1.0)
  (setq left-margin-width 2
	right-margin-width 4
	left-fringe-width 0
        right-fringe-width 0))

(defun mauzy/org-mode-star-face ()
  (font-lock-add-keywords
   nil
   '(("^\\**\\(*\\) "
      (0
       (prog1 nil
         (put-text-property (match-beginning 1) (match-end 1)
                            'face (list :inherit 'org-level-5 :slant 'normal))))))))

;;;;
;;; Use numbers in place of stars for all org-mode buffers
;;
(require 'cl-lib)
(require 'dash)
(defun org-outline-numbering-overlay ()
  "Put numbered overlays on the headings."
  (interactive)
  (cl-loop for (p lv) in
           (let* ((info (org-combine-plists
                         ;; (org-export--get-export-attributes)
                         ;; (org-export--get-buffer-attributes)
                         (org-export-get-environment)
                         '(:section-numbers t)))
                  (tree (org-element-parse-buffer))
                  numberlist)
             (org-export--prune-tree tree info)
             (setq numberlist
                   (org-export--collect-headline-numbering tree info))
             (cl-loop for hl in numberlist
                      collect (cons
                               (org-element-property :begin (car hl))
                               (list (cdr hl)))))
           do
           (let ((ov (make-overlay p (+ (length lv) p))))
             (overlay-put ov 'display (concat (mapconcat 'number-to-string lv ".") ". "))
             (overlay-put ov 'numbered-heading t)
             (overlay-put ov 'face 'org-level-5)
             )))

(define-minor-mode numbered-org-mode
  "Minor mode to number org headings."
  :init-value nil
  (if numbered-org-mode
      (progn
	(add-hook 'post-self-insert-hook 'org-outline-numbering-overlay)
	(setq line-spacing 0.1)
	(org-outline-numbering-overlay))
    (ov-clear 'numbered-heading)))

(use-package! org
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . olivetti-mode)
   ;; (org-mode . variable-pitch-mode)
   (org-mode . org-padding-mode)
   (org-mode . org-superstar-mode)
   (org-mode . mauzy/org-mode-star-face)
   ;; (org-mode . numbered-org-mode)
   ;; (org-mode . mauzy/set-margins)
   )

  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (setq org-startup-indented t
        org-startup-with-inline-images t
        org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
        org-indent-indentation-per-level 2
        org-adapt-indentation nil
        org-ellipsis " ↴ " ;; folding symbol
        org-superstar-headline-bullets-list '("󰯈" "󰯈" "󰯈" "󰯈" "󰯈")


        org-pretty-entities t
        org-hide-emphasis-markers t
        ;; show actually italicized text instead of /italicized text/
        org-agenda-block-separator ""
        org-fontify-whole-heading-line t
        ;; org-fontify-meta-lines-and-blocks t
        org-fontify-done-headline nil
        org-fontify-quote-and-verse-blocks t
        ;; syntax highlighting in src blocks
        org-src-fontify-natively t
                                        ; less shitty indentation in src blocks
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-clock-in-switch-to-state "ACTV"
        org-clock-out-switch-to-state "TODO"

        org-blank-before-new-entry '((heading . always)(plain-list-item . nil))
        ;; org-cycle-separator-lines 10

        org-confirm-babel-evaluate nil

        org-padding-block-begin-line-padding '(0.1 . nil)
        org-padding-block-end-line-padding '(nil . 0.1)
        org-padding-heading-padding-alist
        '((3.0 . 0.3) (2.5 . 0.2) (2.5 . 0.1) (2.0 . 0.0) (2.0 . 0.0) (1.5 . 0.0) (1.0 . 0.0) (0.0 . 0.0))
        ))

(let
    (;;(et-font "ETBembo")
     (et-font "EB Garamond")
     (bg-dark "#fbf8ef")
     (bg-white "#1c1e1f")
     (comment "#525254")
     ;; (lite-code "#5E2750")
     (lite-code "#1C001E")
     ;; (lite-code "#2b2b2b")
     (doc "#727280")
     (slate "#8FA1B3")
     (gray "#bbb")
     (hidden "#404040")
     (serif-mono-font "Monospace")
     (sans-mono-font "Monospace Regular")
     (headline `(:inherit default :weight normal :slant italic :underline nil :foreground "#A7CDC4")))

  (custom-set-faces
   `(variable-pitch ((t (:font ,et-font
                         :height 1.0
                         :weight thin
                         :slant normal
        		 :foreground ,bg-dark))))

   `(fixed-pitch ((t (:height 1.0
        	      :foreground ,bg-dark))))

   `(org-level-1 ((t (,@headline :height 1.5))))
   `(org-level-2 ((t (,@headline :height 1.4))))
   `(org-level-3 ((t (,@headline :height 1.3))))
   `(org-level-4 ((t (,@headline :height 1.2))))
   `(org-level-5 ((t (,@headline :height 1.0))))

   `(org-document-title ((t (:height 1.4))))
   `(org-document-info ((t (:slant italic))))

   `(org-archived ((t (:foreground "#DCDCCC" :weight bold :height 0.5))))
   `(org-hide ((t (:foreground ,hidden
                   :height 0.7
                   :underline nil))))
   ;; `(org-indent ((t (:inherit (org-hide fixed-pitch)))))


   `(org-ellipsis ((t (:underline nil
                       :height 0.5
                       :foreground ,bg-dark))))

   ;; Blocks/Code
   `(org-quote ((t (:background nil
		    :slant italic))))
   `(org-block ((t (:background ,lite-code
                    :height 0.8
                    :family ,serif-mono-font
                    :foreground ,bg-dark))))
   `(org-block-begin-line ((t (:background "#022"
                               :height 0.8
                               :family ,serif-mono-font
                               :foreground ,slate))))
   `(org-block-end-line ((t (:background "#022"
                             :height 0.8
                             :family ,serif-mono-font
                             :foreground ,slate))))
   `(org-src-block-faces ((t (:inherit nil
                              :family ,serif-mono-font
                              :foreground ,comment
                              :height 0.8))))
   `(org-code ((t (:inherit nil
                   :background ,lite-code
                   :family ,serif-mono-font
                   :height 0.9))))


   `(org-meta-line ((t (:background nil
                        :height 0.6
                        :family ,serif-mono-font
                        :foreground ,slate))))

   `(org-document-info-keyword ((t (:height 0.6
				    :family ,serif-mono-font
				    :foreground ,slate))))
   `(org-link ((t (:foreground "#F0DFAF"))))
   `(org-special-keyword ((t (:family ,sans-mono-font
			      :height 0.8))))

   ;; Todo Stuff
   `(org-todo ((t (:height 0.8))))
   `(org-done ((t (:height 0.8))))
   `(org-headline-done ((t (:strike-through t)))))
  `(org-date ((t (:family ,sans-mono-font
		  :height 0.8))))

  `(org-agenda-date ((t (:inherit org-default
			 :height 1.1))))

  `(org-tag ((t (:foreground ,doc))))
  `(org-table ((t (:family ,serif-mono-font
		   :height 0.9
		   :background ,bg-white))))

  ;; `(org-agenda-current-time ((t (nil))))
  ;; `(org-time-grid ((t (nil))))
  ;; `(org-warning ((t (nil))))
  ;; `(org-agenda-structure ((t (nil))))
  ;; `(org-agenda-date-today ((t (nil))))
  ;; `(org-agenda-date-weekend ((t (nil))))
  ;; `(org-scheduled ((t (nil))))
  ;; `(org-upcoming-deadline ((t (nil))))
  ;; `(org-scheduled-today ((t (nil))))
  ;; `(org-scheduled-previously ((t (nil))))
  ;; `(org-agenda-done ((t (nil))))
  )

(use-package! olivetti
  :after org
                                        ;:hook (olivetti-mode . double-header-line-mode)
  :config
  (setq olivetti-min-body-width 80
        olivetti-body-width 230
        olivetti-style 'fancy ; fantastic new layout
        olivetti-margin-width 12)
  ;; (add-hook! 'olivetti-mode-hook (window-divider-mode -1))
  ;; (add-hook! 'olivetti-mode-hook (set-face-attribute 'window-divider nil :foreground (face-background 'fringe) :background (face-background 'fringe)))
  ;; (add-hook! 'olivetti-mode-hook (set-face-attribute 'vertical-border nil :foreground (face-background 'fringe) :background (face-background 'fringe)))
  )

(load! "../pkgs/org-krita.el")

(use-package! org-krita
  :hook
  (org . 'org-krita-mode)
  :config
  (setq org-krita-executable "/Applications/krita.app/Contents/MacOS/krita"))

;;; config/org.el ends here
