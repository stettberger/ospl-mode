;;; ospl-mode.el --- One Sentence Per Line Mode

;; Copyright (C) 2018 Christian Dietrich
;; Copyright (C) 2015 Scot Weldon
;; Copyright (C) 2014 Franceso
;; see https://emacs.stackexchange.com/questions/443/editing-files-with-one-sentence-per-line

;; Author: Christian Dietrich <stettberger@dokucode.de>
;; Keywords: line break, ospl
;; Version: 1.0
;;; Code:

(defgroup ospl nil
  "One Sentence Per Line Mode."
  :prefix "ospl-"
  :group 'visual-line)

(defcustom ospl-text-width 100
  "Number of characters after that lines in One Sentence Per Line mode are broken softly."
  :type 'integer
  :group 'ospl)

(defcustom ospl-adaptive-wrap-prefix t
  "Enable adaptive-wrap-prefix-mode with OSPL mode"
  :type 'boolean
  :group 'ospl)

(defun ospl/buffer-body-width (&optional buffer)
  "How many characters does the current window hold in X direction?"
  (let ((width (window-total-width buffer)))
    (floor (cond
            ((eq text-scale-mode-amount 0)
             width)
            ((> text-scale-mode-amount 0)
             (/ width (* text-scale-mode-step text-scale-mode-amount)))
            ((< text-scale-mode-amount 0)
             (* width (* -1 text-scale-mode-step text-scale-mode-amount)))))))

(defun ospl/right-margin-update (&optional args )
  "Set the right margin in OSPL mode to break the line at ospl-text-width"
  (let ((old-width right-margin-width))
    (setq right-margin-width
          (if ospl-mode
              (max 0 (- (ospl/buffer-body-width) ospl-text-width))
            0))
    (when (not (eq old-width right-margin-width))
      (set-window-buffer nil (current-buffer)))))


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
  "get  the state of an old mode."
  (if (alist-get mode ospl/old-modes) 1 -1))


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
        (add-hook 'text-scale-mode-hook #'ospl/right-margin-update)
        (add-hook 'window-size-change-functions  'ospl/right-margin-update)
        ;; Enable visual-line-mode
        (ospl/push-mode 'visual-line-mode)
        (visual-line-mode 1)
        ;; Disable auto-fill-mode
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
      (remove-hook 'text-scale-mode-hook 'ospl/right-margin-update)
      (remove-hook 'window-size-change-functions  'ospl/right-margin-update)
      (visual-line-mode (ospl/pop-mode 'visual-line-mode))
      (auto-fill-mode (ospl/pop-mode 'auto-fill-mode))
      (if ospl-adaptive-wrap-prefix
          (adaptive-wrap-prefix-mode (ospl/pop-mode 'adpative-wrap-prefix-mode)))
      ;; (setq ospl/old-modes nil)
      ))
  (ospl/right-margin-update))

(provide 'ospl-mode)
