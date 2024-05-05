;;; conf/mauzy.el --- custom functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun mauzy/add-to-list-multiple (list to-add)
  "Adds multiple items to LIST.
Allows for adding a sequence of items to the same list, rather
than having to call `add-to-list' multiple times."
  (interactive)
  (dolist (item to-add)
    (add-to-list list item)))

(defun mauzy/comment-section-block ()
  (interactive)
  (if (member major-mode '(rjsx-mode tsx-ts-mode typescript-ts-mode deno-mode deno-ts-mode deno-tsx-ts-mode astro-mode))
      (progn
	(insert "/*****************************************************************************")
	(electric-newline-and-maybe-indent)
	(insert "*")
	(electric-newline-and-maybe-indent)
	(insert "*****************************************************************************/")
	(forward-line -1)
	(indent-according-to-mode)
	(forward-line 1)
	(indent-according-to-mode)
	(forward-line -1)
	(move-end-of-line 1)
	(insert " "))
    (message "major mode not supported")))

(global-set-key (kbd "C-c ;") 'mauzy/comment-section-block)

(defun mauzy/comment-section-line ()
  (interactive)
  (if (member major-mode '(rjsx-mode tsx-ts-mode typescript-ts-mode deno-mode deno-ts-mode deno-tsx-ts-mode astro-mode))
      (progn
	(insert "/****************************************************************************/")
        (electric-newline-and-maybe-indent))
    (message "major mode not supported")))

(global-set-key (kbd "C-c C-;") 'mauzy/comment-section-line)


;;; mauzy.el ends here
