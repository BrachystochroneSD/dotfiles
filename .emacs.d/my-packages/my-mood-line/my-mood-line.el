;;; my-mood-line.el --- A minimal mode-line inspired by doom-modeline -*- lexical-binding: t; -*-

;; Author: Jessie Hildebrandt <jessieh.net> -> Changed by Samuel Dawant
;; Homepage: https://gitlab.com/jessieh/my-mood-line
;; Keywords: mode-line faces
;; Package-Version: 20211003.2113
;; Package-Commit: ef1c752679a8f92faa7b4828adbbb300b6942f22
;; Version: 1.2.5
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; my-mood-line is a minimal mode-line configuration that aims to replicate
;; some of the features of the doom-modeline package.
;;
;; Features offered:
;; * Clean, minimal design
;; * Anzu and multiple-cursors counter
;; * Version control status indicator
;; * Flycheck status indicator
;; * Flymake support
;; * Lightweight with no dependencies
;;
;; To enable my-mood-line:
;; (my-mood-line-mode)

;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code:

;;
;; Variable declarations
;;

(defvar flycheck-current-errors)
(defvar flymake--mode-line-format)
(defvar anzu-cons-mode-line-p)
(defvar anzu--state)
(defvar anzu--cached-count)
(defvar anzu--overflow-p)
(defvar anzu--current-position)
(defvar anzu--total-matched)
(defvar multiple-cursors-mode)

;;
;; Function prototypes
;;

(declare-function flycheck-count-errors "flycheck" (errors))
(declare-function mc/num-cursors "multiple-cursors" ())

;;
;; Config
;;

(defgroup my-mood-line nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defcustom my-mood-line-show-eol-style nil
  "If t, the EOL style of the current buffer will be displayed in the mode-line."
  :group 'my-mood-line
  :type 'boolean)

(defcustom my-mood-line-show-encoding-information nil
  "If t, the encoding format of the current buffer will be displayed in the mode-line."
  :group 'my-mood-line
  :type 'boolean)

(defcustom my-mood-line-show-cursor-point nil
  "If t, the value of `point' will be displayed next to the cursor position in the mode-line."
  :group 'my-mood-line
  :type 'boolean)

(defface my-mood-line-buffer-name
  '((t (:inherit (mode-line-buffer-id))))
  "Face used for major mode indicator in the mode-line."
  :group 'my-mood-line)

(defface my-mood-line-major-mode
  '((t (:inherit (bold))))
  "Face used for major mode indicator in the mode-line."
  :group 'my-mood-line)

(defface my-mood-line-status-neutral
  '((t (:inherit (shadow))))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'my-mood-line)

(defface my-mood-line-status-info
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line."
  :group 'my-mood-line)

(defface my-mood-line-status-success
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line."
  :group 'my-mood-line)

(defface my-mood-line-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line."
  :group 'my-mood-line)

(defface my-mood-line-status-error
  '((t (:inherit (error))))
  "Face for error stauts indicators in the mode-line."
  :group 'my-mood-line)

(defface my-mood-line-unimportant
  '((t (:inherit (shadow))))
  "Face used for less important mode-line elements."
  :group 'my-mood-line)

(defface my-mood-line-modified
  '((t (:inherit (error))))
  "Face used for the 'modified' indicator symbol in the mode-line."
  :group 'my-mood-line)

;;
;; Helper functions
;;

(defun my-mood-line--string-trim-left (string)
  "Remove whitespace at the beginning of STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string))

(defun my-mood-line--string-trim-right (string)
  "Remove whitespace at the end of STRING."
  (if (string-match "[ \t\n\r]+\\'" string)
      (replace-match "" t t string)
    string))

(defun my-mood-line--string-trim (string)
  "Remove whitespace at the beginning and end of STRING."
  (my-mood-line--string-trim-left (my-mood-line--string-trim-right string)))

(defun my-mood-line--format (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT, aligned respectively."
  (let ((reserve (length right)))
    (concat left
            " "
            (propertize " "
                        'display `((space :align-to (- right (- 0 right-margin) ,reserve))))
            right)))

;;
;; Update functions
;;

(defvar-local my-mood-line--vc-text nil)
(defun my-mood-line--update-vc-segment (&rest _)
  "Update `my-mood-line--vc-text' against the current VCS state."
  (setq my-mood-line--vc-text
        (when (and vc-mode buffer-file-name)
          (let ((backend (vc-backend buffer-file-name))
                (state (vc-state buffer-file-name (vc-backend buffer-file-name))))
            (let ((face 'mode-line-neutral))
              (concat (cond ((memq state '(edited added))
                             (setq face 'my-mood-line-status-info)
                             (propertize "+ " 'face face))
                            ((eq state 'needs-merge)
                             (setq face 'my-mood-line-status-warning)
                             (propertize "⟷ " 'face face))
                            ((eq state 'needs-update)
                             (setq face 'my-mood-line-status-warning)
                             (propertize "↑ " 'face face))
                            ((memq state '(removed conflict unregistered))
                             (setq face 'my-mood-line-status-error)
                             (propertize "✖ " 'face face))
                            (t
                             (setq face 'my-mood-line-status-neutral)
                             (propertize "✔ " 'face face)))
                      (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                  'face face
                                  'mouse-face face)
                      "  "))))))

(defvar-local my-mood-line--flycheck-text nil)
(defun my-mood-line--update-flycheck-segment (&optional status)
  "Update `my-mood-line--flycheck-text' against the reported flycheck STATUS."
  (setq my-mood-line--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat "⚑ Issues: "
                                                 (number-to-string sum)
                                                 "  ")
                                         'face (if .error
                                                   'my-mood-line-status-error
                                                 'my-mood-line-status-warning))))
                       (propertize "✔ Good  " 'face 'my-mood-line-status-success)))
          ('running (propertize "Δ Checking  " 'face 'my-mood-line-status-info))
          ('errored (propertize "✖ Error  " 'face 'my-mood-line-status-error))
          ('interrupted (propertize "⏸ Paused  " 'face 'my-mood-line-status-neutral))
          ('no-checker ""))))

;;
;; Segments
;;

(defun my-mood-line-segment-modified ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  (if (not (string-match-p "\\*.*\\*" (buffer-name)))
      (if (buffer-modified-p)
          (propertize " ● " 'face 'my-mood-line-modified)
        (if (and buffer-read-only (buffer-file-name))
            (propertize " ■ " 'face 'my-mood-line-modified)
          (propertize "   " 'face 'my-mood-line-modified)))
    (propertize "   " 'face 'my-mood-line-modified)))

(defun my-mood-line-segment-buffer-name ()
  "Displays the name of the current buffer in the mode-line."
  (propertize "%b  " 'face 'my-mood-line-buffer-name))

(defun my-mood-line-segment-anzu ()
  "Displays color-coded anzu status information in the mode-line (if available)."
  (when (and (boundp 'anzu--state) anzu--state)
    (cond ((eq anzu--state 'replace-query)
           (format #("Replace: %d  " 0 11 (face my-mood-line-status-warning)) anzu--cached-count))
          (anzu--overflow-p
           (format #("%d/%d+  " 0 3 (face my-mood-line-status-info) 3 6 (face my-mood-line-status-error)) anzu--current-position anzu--total-matched))
          (t
           (format #("%d/%d  " 0 5 (face my-mood-line-status-info)) anzu--current-position anzu--total-matched)))))

(defun my-mood-line-segment-multiple-cursors ()
  "Displays the number of active multiple-cursors in the mode-line (if available)."
  (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
    (concat " MC"
            (format #("×%d  " 0 3 (face my-mood-line-status-warning)) (mc/num-cursors)))))

(defun my-mood-line-segment-position ()
  "Displays the current cursor position in the mode-line."
  (concat " %l:%c "
          (when my-mood-line-show-cursor-point (propertize (format ":%d" (point)) 'face 'my-mood-line-unimportant))
          (propertize " %p%%  " 'face 'my-mood-line-unimportant)))

(defun my-mood-line-segment-eol ()
  "Displays the EOL style of the current buffer in the mode-line."
  (when my-mood-line-show-eol-style
    (pcase (coding-system-eol-type buffer-file-coding-system)
      (0 "LF  ")
      (1 "CRLF  ")
      (2 "CR  "))))

(defun my-mood-line-segment-encoding ()
  "Displays the encoding and EOL style of the buffer in the mode-line."
  (when my-mood-line-show-encoding-information
    (concat (let ((sys (coding-system-plist buffer-file-coding-system)))
              (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                     "UTF-8")
                    (t (upcase (symbol-name (plist-get sys :name))))))
            "  ")))

(defun my-mood-line-segment-vc ()
  "Displays color-coded version control information in the mode-line."
  my-mood-line--vc-text)

(defun my-mood-line-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (concat (format-mode-line mode-name 'my-mood-line-major-mode) "  "))

(defun my-mood-line-segment-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (let ((misc-info (format-mode-line mode-line-misc-info 'my-mood-line-unimportant)))
    (unless (string= (my-mood-line--string-trim misc-info) "")
      (concat (my-mood-line--string-trim misc-info) "  "))))

(defun my-mood-line-segment-flycheck ()
  "Displays color-coded flycheck information in the mode-line (if available)."
  my-mood-line--flycheck-text)

(defun my-mood-line-segment-flymake ()
  "Displays information about the current status of flymake in the mode-line (if available)."
  (when (and (boundp 'flymake-mode) flymake-mode)
    ;; Depending on Emacs version, flymake stores the mode-line segment using one of two variable names
    (let ((flymake-segment-format (if (boundp 'flymake-mode-line-format)
                                      flymake-mode-line-format
                                    flymake--mode-line-format)))
      (concat (my-mood-line--string-trim (format-mode-line flymake-segment-format)) "  "))))

(defun my-mood-line-segment-process ()
  "Displays the current value of `mode-line-process' in the mode-line."
  (let ((process-info (format-mode-line mode-line-process)))
    (unless (string= (my-mood-line--string-trim process-info) "")
      (concat (my-mood-line--string-trim process-info) "  "))))

;;
;; Activation function
;;

(defvar-local my-mood-line--default-mode-line mode-line-format)
(defvar-local my-mood-line--anzu-cons-mode-line-p nil)

;;;###autoload
(define-minor-mode my-mood-line-mode
  "Toggle my-mood-line on or off."
  :group 'my-mood-line
  :global t
  :lighter nil
  (if my-mood-line-mode
      (progn

        ;; Setup flycheck hooks
        (add-hook 'flycheck-status-changed-functions #'my-mood-line--update-flycheck-segment)
        (add-hook 'flycheck-mode-hook #'my-mood-line--update-flycheck-segment)

        ;; Setup VC hooks
        (add-hook 'find-file-hook #'my-mood-line--update-vc-segment)
        (add-hook 'after-save-hook #'my-mood-line--update-vc-segment)
        (advice-add #'vc-refresh-state :after #'my-mood-line--update-vc-segment)

        ;; Disable anzu's mode-line segment setting, saving the previous setting to be restored later (if present)
        (when (boundp 'anzu-cons-mode-line-p)
          (setq my-mood-line--anzu-cons-mode-line-p anzu-cons-mode-line-p))
        (setq-default anzu-cons-mode-line-p nil)

        ;; Save previous mode-line-format to be restored later
        (setq my-mood-line--default-mode-line mode-line-format)

        ;; Set the new mode-line-format
        (setq-default mode-line-format
                      '((:eval
                         (my-mood-line--format
                          ;; Left
                          (format-mode-line
                           '((:eval (my-mood-line-segment-modified))
                             (:eval (my-mood-line-segment-buffer-name))
                             (:eval (my-mood-line-segment-anzu))
                             (:eval (my-mood-line-segment-position))
                             (:eval (my-mood-line-segment-multiple-cursors))))

                          ;; Right
                          (format-mode-line
                           '((:eval (my-mood-line-segment-eol))
                             (:eval (my-mood-line-segment-encoding))
                             (:eval (my-mood-line-segment-vc))
                             (:eval (my-mood-line-segment-major-mode))
                             (:eval (my-mood-line-segment-misc-info))
                             (:eval (my-mood-line-segment-flycheck))
                             (:eval (my-mood-line-segment-flymake))
                             (:eval (my-mood-line-segment-process)))))))))
    (progn

      ;; Remove flycheck hooks
      (remove-hook 'flycheck-status-changed-functions #'my-mood-line--update-flycheck-segment)
      (remove-hook 'flycheck-mode-hook #'my-mood-line--update-flycheck-segment)

      ;; Remove VC hooks
      (remove-hook 'file-find-hook #'my-mood-line--update-vc-segment)
      (remove-hook 'after-save-hook #'my-mood-line--update-vc-segment)
      (advice-remove #'vc-refresh-state #'my-mood-line--update-vc-segment)

      ;; Restore anzu's mode-line segment setting
      (setq-default anzu-cons-mode-line-p my-mood-line--anzu-cons-mode-line-p)

      ;; Restore the original mode-line format
      (setq-default mode-line-format my-mood-line--default-mode-line))))

;;
;; Provide my-mood-line
;;

(provide 'my-mood-line)

;;; my-mood-line.el ends here
