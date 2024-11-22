(setq dired-listing-switches "-alh --group-directories-first")
(require 'browse-url)

(defvar my-org-extension-list
  '( "xlsm" "xlsx" "exe" "docx" "pptx" "doc" "msg")
  "List of file extension to be opened with the `org-open-file-system' function")

(defun my-dired-open-choice ()
  "Let me choose between the way to open the file"
  (interactive)
  (let ((find-file-run-dired t)
        (switch-to-buffer-preserve-window-point
         (if dired-auto-revert-buffer
             nil
           switch-to-buffer-preserve-window-point)))
    (funcall (intern (completing-read "Open with : " '("org-open-file" "org-open-file-with-system" "find-file") nil t)) (dired-get-file-for-visit))))

(defun my-dired-find-file-internal (file &optional bm)
  "Used by `my-dired-open-file'"
  (let ((file-extension (or (file-name-extension file) "FILEEXT"))) ;; just to have a string to match when there's none
    (cond ((string-match "\\(xcf\\|pdf\\|djvu\\|ps\\|dvi\\)$" file-extension)
           (org-open-file file))
          ((string-match "^https?://.*" file)
           (browse-url-default-browser file))
          ((string-match (regexp-opt my-org-extension-list) file-extension)
           (org-open-file-with-system file))
          ((and
            (not bm)
            (file-directory-p (dired-get-file-for-visit)))
           (dired-maybe-insert-subdir file)
           (recenter-top-bottom (line-number-at-pos)))
          ((string-match "JPG$" file-extension)
           (let ((currbuff (current-buffer)))
             (other-window 1)
             (set-buffer-modified-p nil)
             (kill-buffer-and-window)
             (find-file file)
             (split-window-horizontally (max 15 (min 20 (/ (window-width) 3.))))
             (switch-to-buffer currbuff)))
          ((and
            (equal major-mode 'dired-mode)
            (not bm)
            (not (window-left (car (window-list)))))
           (delete-other-windows)
           (find-file file)
           (split-window-horizontally (max 15 (min 20 (/ (window-width) 3.))))
           (previous-buffer))
          (t
           (find-file file)))))

(defun my-dired-open-file ()
  "Based on `dired-find-file'"
  (interactive)
  (let ((find-file-run-dired t)
        (switch-to-buffer-preserve-window-point
         (if dired-auto-revert-buffer
             nil
           switch-to-buffer-preserve-window-point)))
    (my-dired-find-file-internal (dired-get-file-for-visit))))

(defun search-is-that-right-buffer (filename)
  (let ((file (concat (file-name-base filename) "." (file-name-extension filename)))
        (dir (file-name-directory filename)))
    (and (equal major-mode 'dired-mode)
         (and (re-search-forward dir (beginning-of-buffer) t)
              (re-search-forward file nil t )))))

(defun search-for-filename-in-dired-buffers (filename)
  (message "TODO")
  (interactive)
  (let ((buffres)
        (buffname (buffer-name))
        (file (concat (file-name-base filename) "." (file-name-extension filename)))
        (dir (file-name-directory filename)))
    (next-buffer)
    (while (and (or (re-search-forward dir (beginning-of-buffer) t)
                    (not (equal major-mode 'dired-mode)))
                (not (equal buffname (buffer-name))))
      (next-buffer))
    (setq buffres (buffer-name))
    (switch-to-buffer buffname)
    buffres))

(defun my-dired-jump ()
  (interactive)
  (split-window-horizontally (max 15 (min 20 (/ (window-width) 3.))))
  (dired-jump nil (magit-toplevel)))

(global-set-key (kbd "C-à") 'my-dired-jump)
(global-set-key (kbd "M-à") 'dired-jump)
(global-set-key (kbd "C-!") (lambda () (interactive)(dired "~")))

(defun my-dired-kill-subdir ()
  (interactive)
  (if (equal (dired-current-directory) (expand-file-name default-directory))
      (kill-buffer-and-window)
    (dired-kill-subdir)
    (pop-to-mark-command)
    (recenter-top-bottom (line-number-at-pos))))

(defun my-dired-jump-dir ()
  (interactive)
  (let (( dirname (dired-current-directory) )
        ( buffer (current-buffer)))
    (if (not (equal dirname (expand-file-name default-directory)))
        (pop-to-mark-command)
      (dired-jump)
      (save-excursion
        (dired-insert-subdir dirname))
      (kill-buffer buffer))))

;; dired-hide-dotfiles minor mode (No working properly)
;; TODO : make it work with unhidden files inside dotfolder

(define-minor-mode dired-hide-dotfiles-mode
  "Toggle `dired-hide-dotfiles-mode'"
  :init-value nil
  :lighter " !."
  :group 'dired
  (if dired-hide-dotfiles-mode
      (dired-hide-dotfiles--hide)
    (revert-buffer)))

(defun dired-hide-dotfiles--hide ()
  "Hide all dot-files in the current `dired' buffer."
  (when dired-hide-dotfiles-mode
    (dired-mark-files-regexp "^\\.")
    (dired-do-kill-lines)))

(add-hook 'dired-after-readin-hook 'dired-hide-dotfiles--hide)

(defun my-dired-find-file-read-args (prompt mustmatch)
  (list (read-file-name prompt (dired-current-directory) nil mustmatch)
        t))

(defun my-dired-find-file (filename &optional wildcards)
  "find file with the current directory"
  (interactive
   (my-dired-find-file-read-args "Find file: "
                                 (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
        (mapcar 'pop-to-buffer-same-window (nreverse value))
      (pop-to-buffer-same-window value))))

(defun my-dired-mode-hook ()
  (interactive)
  ;; (dired-hide-dotfiles-mode)
  (define-key dired-mode-map "." #'dired-hide-dotfiles-mode)
  (dired-hide-details-mode)
  (display-line-numbers-mode 0)
  (local-unset-key (kbd "C-M-n"))
  (local-set-key (kbd "C-x C-f") 'my-dired-find-file)
  (local-set-key (kbd "/") 'isearch-forward)
  (local-set-key (kbd "o") 'dired-find-file)
  (local-set-key (kbd "j") 'dired-next-line)
  (local-set-key (kbd "k") 'dired-previous-line)
  (local-set-key (kbd "h") 'my-dired-jump-dir)
  (local-set-key (kbd "J") 'dired-next-subdir)
  (local-set-key (kbd "<tab>") 'dired-hide-subdir)
  (local-set-key (kbd "K") 'dired-prev-subdir)
  (local-set-key (kbd "q") 'my-dired-kill-subdir)
  (local-set-key (kbd "Q") 'kill-buffer-and-window)
  (local-set-key (kbd "O") 'dired-find-file-other-window)
  (local-set-key (kbd "M-o") 'my-dired-open-choice)
  (local-set-key (kbd "l") 'my-dired-open-file)
  (local-set-key (kbd "<C-return>") 'dired-find-file-other-window)
  (local-set-key (kbd "C-h") 'describe-mode)
  (local-set-key (kbd "C-j") 'dired-goto-file)
  (local-set-key (kbd "C-k") 'dired-do-kill-lines))

(add-hook 'dired-mode-hook 'my-dired-mode-hook)
