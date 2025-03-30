(require 'counsel)

(defvar my-bookmarks-default-file "~/.emacs.d/my-bookmarks")
(defvar my-bookmarks-alist `())

(defun my-save-bookmarks ()
  (interactive)
  (let ((file my-bookmarks-default-file))
    (with-current-buffer (find-file-noselect my-bookmarks-default-file)
      (delete-region (point-min) (point-max))
      (dolist (bm my-bookmarks-alist)
        (insert (format "%s %s\n" (car bm) (cdr bm))))
      (write-file file)
      (kill-buffer (current-buffer)))))

(defun my-load-bookmarks ()
  (let ((file my-bookmarks-default-file))
    (with-current-buffer (find-file-noselect my-bookmarks-default-file)
      (goto-char (point-min))
      (dolist (line (split-string (substring (thing-at-point 'page) 0 -1) "\n"))
        (let ((bm (split-string line " ")))
          (my-bookmarks-add-bm (car bm) (cadr bm) t))))))

(defun my-bookmarks (alias)
  (interactive
   (list
    (completing-read "My-bookmarks goto: " my-bookmarks-alist nil t)))
  (let ((file-path (cdr (assoc alias my-bookmarks-alist))))
    (if (directory-name-p file-path)
        (counsel-fzf nil file-path)
      (my-dired-find-file-internal file-path t))))

(defun my-bookmarks-add-bm (alias path &optional no-save)
  (interactive
   (list
    (read-string "Add bookmark alias: ")
    (read-file-name "Add bookmak pathway: " nil nil t)))
  (unless (assoc alias my-bookmarks-alist)
    (add-to-list 'my-bookmarks-alist (cons alias path)))
  (unless no-save (my-save-bookmarks)))

(defun my-bookmarks-remove-bm (alias)
  (interactive
   (list
    (completing-read
     "My-bookmarks remove: "
     my-bookmarks-alist nil t)))
  (setq my-bookmarks-alist
        (delq (assoc alias my-bookmarks-alist)
              my-bookmarks-alist))
  (my-save-bookmarks))

(global-set-key (kbd "C-ç") 'my-bookmarks)
(global-set-key (kbd "C-M-ç") 'my-bookmarks-remove-bm)
(global-set-key (kbd "M-ç") 'my-bookmarks-add-bm)

(my-load-bookmarks)
