(use-package c-mode :hook (c-mode . outline-minor-mode))

(use-package lua-mode :config (setq lua-indent-level 4))

(use-package django-mode)

(use-package django-manage
  :ensure t
  :after django-mode
  :bind (:map django-mode-map ("<f5>" . django-manage-runserver))
  :config
  (defun django-manage-root ()
    "Override the shitty defcustom in django-manage by something that search for the manage.py,
taken the same algorythm than gdscript-util"
    (let* ((base-path  default-directory)
           (dominating-file
            (locate-dominating-file base-path
                                    (lambda (parent)
                                      (directory-files parent t "manage.py")))))
      (when dominating-file (expand-file-name dominating-file)))))

(use-package nxml-mode
  :config
  (defun my-nxml-inlinify-code-to-kill ()
    (interactive)
    (let ((buffstr (buffer-string)))
      (with-temp-buffer
        (insert buffstr)
        (while (re-search-forward "\n *" (beginning-of-buffer) t)
          (replace-match " "))
        (when (re-search-forward ".*<svg " (beginning-of-buffer) t)
          (replace-match "<svg "))
        (kill-new (buffer-string)))))

  (defun my-sgml-mark-whole-tag ()
    (interactive)
    (sgml-skip-tag-forward 1)
    (end-of-line)
    (set-mark (point))
    (forward-char)
    (sgml-skip-tag-backward 1))

  (defun my-sgml-tag (tag)
    (interactive
     (list (read-string "which tag (default div)" nil nil "div")))
    (if (looking-back "\\\n[\t ]*<[^>]*")
        (progn
          (beginning-of-line-text)
          (setq pointbeg (point))
          (insert (format "<%s>\n" tag))
          (save-excursion
            (sgml-skip-tag-forward 1)
            (insert (format "\n</%s>" tag))
            (setq pointend (point))
            (indent-region pointbeg pointend))
          (backward-char 2))
      (insert (format "<%s></%s>" tag tag))
      (re-search-backward "<")))

  (defun my-sgml-delete-tag ()
    (interactive)
    (sgml-delete-tag 1)
    (indent-region (point-min) (point-max)))

  (defun sgml-skip-toggle-tag ()
    (interactive)
    (unless (looking-at "<")
      (re-search-backward "<")
      )
    (forward-char 2)
    (if (looking-back "</")
        (sgml-skip-tag-backward 1)
      (sgml-skip-tag-forward 1))
    )

  (defun my-html-mode-hook ()
    (interactive)
    (local-set-key (kbd "<tab>") 'my-html-smart-tab)
    (evil-define-key 'insert 'local (kbd "<tab>") 'my-html-smart-tab)
    (local-set-key (kbd "C-c C-n")
                   (lambda () (interactive) (counsel-fzf nil (magit-toplevel))))
    (local-set-key (kbd "C-c C-p") 'sgml-skip-toggle-tag)
    (local-set-key (kbd "C-c C-p") 'sgml-skip-toggle-tag)
    (local-set-key (kbd "C-c C-t") 'my-sgml-tag)
    (local-set-key (kbd "C-c C-d") 'my-sgml-delete-tag))

  (add-hook 'html-mode-hook 'my-html-mode-hook)
  (add-hook 'mhtml-mode-hook 'my-html-mode-hook)

  (defun my-html-shite-cond (shite)
    (and (looking-back (format "<%s" shite))
         (looking-at ">")))

  (defun my-html-shite (shite tag)
    (delete-char 1)
    (delete-backward-char (1+ (length shite)))
    (insert (format "<%s>\n\n</%s>" tag tag))
    (next-logical-line -1)
    (indent-for-tab-command))

  (defun my-html-smart-tab ()
    (interactive)
    (cond
     ((my-html-shite-cond "he")
      (my-html-shite "he" "header"))
     ((and (looking-at ">")
           (looking-back "<\\([^ ]*\\).*"))
      (end-of-line)
      (newline 2)
      (insert (format "</%s>" (match-string 1)))
      (next-logical-line -1)
      (indent-for-tab-command))
     (t
      (indent-for-tab-command)))))
