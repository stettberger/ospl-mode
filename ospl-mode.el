;;; ospl-mode.el --- One Sentence Per Line Mode

;; Copyright (C) 2018 Christian Dietrich
;; Copyright (C) 2015 Scot Weldon
;; Copyright (C) 2014 Franceso

;; Author: Christian Dietrich <stettberger@dokucode.de>
;; URL: https://github.com/stettberger/ospl-mode.el
;; Version: 1.0
;; Keywords: line break, ospl
;; Package-Requires: ((visual-fill-column "1.9"))

;;; Commentary:
;; see https://emacs.stackexchange.com/questions/443/editing-files-with-one-sentence-per-line

;;; Code:

(defgroup ospl nil
  "One Sentence Per Line Mode."
  :prefix "ospl-"
  :group 'visual-line)


(defcustom ospl-adaptive-wrap-prefix t
  "Enable adaptive-wrap-prefix-mode with OSPL mode."
  :type 'boolean
  :group 'ospl)

(require 'visual-fill-column)

(defun ospl/unfill-paragraph ()
  "Unfill the paragraph at point.

This repeatedly calls `join-line' until the whole paragraph does
not contain hard line breaks any more."
  (let ((fill-column 100000))
    (fill-paragraph)))


(defun ospl/fill-paragraph ()
  "Fill the current paragraph until there is one sentence per line.

This unfills the paragraph, and places hard line breaks after each sentence."
  (interactive)
  (save-excursion
    (fill-paragraph)
    (ospl/unfill-paragraph)  ; remove hard line breaks
    (beginning-of-line)

    ;; insert line breaks again
    (let ((end-of-paragraph (make-marker)))
      (set-marker end-of-paragraph (line-end-position))
      (forward-sentence)
      (while (< (point) end-of-paragraph)
        (just-one-space)
        (delete-backward-char 1)
        (newline)
        (forward-sentence))
      (set-marker end-of-paragraph nil))))

(defvar-local ospl/old-modes nil)

(defun ospl/push-mode (mode &optional enabled)
  "Save the state of an old mode."
  (add-to-list 'ospl/old-modes
               (cons mode (if (eq enabled nil)
                              (if (boundp mode) (symbol-value mode) -1)
                            enabled))))

(defun ospl/pop-mode (mode)
  "Get the state of an old mode."
  (if (alist-get mode ospl/old-modes) 1 -1))

(defun ospl/update-margin (&rest optional)
  "Update the fill margins"
  ;; This is an ugly hack, until visual-fill-column gets fixed
  (when ospl-mode
      (visual-fill-column-mode -1)
      (visual-fill-column-mode 1)))


;;;###autoload
(define-minor-mode ospl-mode
  "One Sentence Per Line"
  :init-value nil
  :lighter " ospl"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-q") 'ospl/fill-paragraph)
            map)

  (if ospl-mode
      (progn
        (add-hook 'text-scale-mode-hook #'ospl/update-margin)
        (add-hook 'window-size-change-functions  'ospl/update-margin)
        ;; Enable visual-line-mode
        (ospl/push-mode 'visual-line-mode)
        (visual-line-mode 1)
        ;; Enable Visual-Fill-Column-Mode
        (ospl/push-mode 'visual-fill-column-mode)
        (visual-fill-column-mode 1)
        ;; Disable auto-fill-mode, as it really conflicts
        (ospl/push-mode 'auto-fill-mode
                        (not (eq auto-fill-function nil)))
        (auto-fill-mode -1)
        ;; Adaptive Wrap for nicer wrapping
        (when ospl-adaptive-wrap-prefix
          (require 'adaptive-wrap)
          (ospl/push-mode 'adaptive-wrap-prefix-mode)
          (adaptive-wrap-prefix-mode 1)
          (setq adaptive-wrap-extra-indent 2))
        )
    (progn
      (remove-hook 'text-scale-mode-hook #'ospl/update-margin)
      (remove-hook 'window-size-change-functions  'ospl/update-margin)
      (visual-line-mode (ospl/pop-mode 'visual-line-mode))
      (visual-fill-column-mode (ospl/pop-mode 'visual-fill-column-mode))
      (auto-fill-mode (ospl/pop-mode 'auto-fill-mode))
      (if ospl-adaptive-wrap-prefix
          (adaptive-wrap-prefix-mode (ospl/pop-mode 'adpative-wrap-prefix-mode)))
      ;; (setq ospl/old-modes nil)
      )))

(provide 'ospl-mode)

;;; ospl-mode.el ends here
