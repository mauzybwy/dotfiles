;;; splash.el --- Custom splash screen with image and text -*- no-byte-compile: t; lexical-binding: t; -*-

;; Author: Your Name
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience, frames
;; URL: https://github.com/yourusername/splash

;;; Commentary:

;; This package provides a customizable splash screen with a centered image
;; and optional text below it.
;;
;; Usage:
;;   (require 'splash)
;;   (setq splash-image-path "~/path/to/image.png")
;;   (setq splash-text '("Welcome back!" "Have a great day :)"))
;;   (setq initial-buffer-choice #'splash-screen)
;;   (setq inhibit-startup-screen t)

;;; Code:

;;; Customization

(defgroup splash nil
  "Customizable splash screen with image and text."
  :group 'environment
  :prefix "splash-")

(defcustom splash-image-path nil
  "Path to the splash screen image.
If nil, only text will be displayed."
  :type '(choice (const :tag "No image" nil)
                 (file :tag "Image file"))
  :group 'splash)

(defcustom splash-text '("Welcome to Emacs!")
  "Lines of text to display below the splash image.
Each string in the list will be displayed as a separate centered line."
  :type '(repeat string)
  :group 'splash)

;;; Internal functions

(defun splash--calculate-padding (content-width window-width)
  "Calculate padding to center CONTENT-WIDTH in WINDOW-WIDTH."
  (max 0 (/ (- window-width content-width) 2)))

(defun splash--calculate-vertical-offset (image-height text-lines window-height)
  "Calculate vertical offset to center IMAGE-HEIGHT and TEXT-LINES in WINDOW-HEIGHT."
  (let* ((char-height (frame-char-height))
         (image-rows (/ image-height char-height))
         (total-rows (+ image-rows text-lines)))
    (max 0 (/ (- window-height total-rows) 2))))

(defun splash--insert-centered-image (image window-width)
  "Insert IMAGE centered in WINDOW-WIDTH."
  (let* ((img-width (car (image-size image t)))
         (char-width (frame-char-width))
         (img-cols (/ img-width char-width))
         (padding (splash--calculate-padding img-cols window-width)))
    (insert (make-string padding ?\s))
    (insert-image image)))

(defun splash--insert-centered-text-lines (text-lines window-width)
  "Insert TEXT-LINES, each centered in WINDOW-WIDTH."
  (dolist (line text-lines)
    (let ((padding (splash--calculate-padding (length line) window-width)))
      (insert (make-string padding ?\s))
      (insert line "\n"))))

(defun splash--cleanup-buffer ()
  "Remove trailing newline to prevent overscroll."
  (when (> (point) 1)
    (goto-char (point-max))
    (when (eq (char-before) ?\n)
      (delete-char -1))))

(defun splash--draw-content ()
  "Draw the splash screen content with centered image and text."
  (let ((inhibit-read-only t))
    (erase-buffer)
    
    ;; Configure buffer appearance
    (setq mode-line-format nil
          cursor-type nil)
    
    (if (and splash-image-path (file-exists-p splash-image-path))
        ;; Draw with image
        (let* ((img (create-image splash-image-path))
               (img-size (image-size img t))
               (img-width (car img-size))
               (img-height (cdr img-size))
               (win-width (window-width))
               (win-height (window-height))
               (v-offset (splash--calculate-vertical-offset 
                          img-height (length splash-text) win-height)))
          
          ;; Vertical spacing
          (insert (make-string v-offset ?\n))
          
          ;; Insert image
          (splash--insert-centered-image img win-width)
          (insert "\n\n")
          
          ;; Insert text
          (splash--insert-centered-text-lines splash-text win-width)
          
          ;; Cleanup
          (splash--cleanup-buffer))
      
      ;; Draw text only (no image)
      (let* ((win-width (window-width))
             (win-height (window-height))
             (v-offset (max 0 (/ (- win-height (length splash-text)) 2))))
        
        ;; Vertical spacing
        (insert (make-string v-offset ?\n))
        
        ;; Insert text
        (splash--insert-centered-text-lines splash-text win-width)
        
        ;; Cleanup
        (splash--cleanup-buffer)))
    
    (goto-char (point-min))))

;;; Public interface

;;;###autoload
(defun splash-screen ()
  "Create and return the custom splash screen buffer.
This function is suitable for use with `initial-buffer-choice'."
  (let ((splash-buffer (get-buffer-create "*splash*")))
    (with-current-buffer splash-buffer
      (splash--draw-content)
      
      ;; Handle window resizing
      (add-hook 'window-size-change-functions
                (lambda (_frame)
                  (when (eq (current-buffer) splash-buffer)
                    (splash--draw-content)))
                nil t)
      
      (setq buffer-read-only t))
    splash-buffer))

(provide 'splash)

;;; splash.el ends here
