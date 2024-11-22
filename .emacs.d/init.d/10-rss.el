(use-package elfeed
  :ensure t
  :init
  (setq elfeed-search-filter "@3-weeks-ago +unread")
  :bind (:map elfeed-search-mode-map
              ("b" . elfeed-visit-or-play)
              ("B" . elfeed-search-browse-url)
              ("d" . my-elfeed-read-regex)
              ("D" . my-elfeed-mark-all-author-as-read)
              ("j" . next-line)
              ("k" . previous-line)
              ("l" . my-elfeed-open)
              ("+" . my-elfeed-add-week)
              ("-" . my-elfeed-sub-week)
              ("t" . my-elfeed-toggle-unread)
              ("h" . delete-other-windows)
              ("/" . isearch-forward)
              ("?" . isearch-backward)
              ("q" . delete-frame)
              ("C-b" . (lambda () (interactive) (elfeed-play-with-mpv-at-quality 720 (elfeed-search-selected :single))))
              ("C-n" . my-elfeed-scroll-up)
              ("C-p" . my-elfeed-scroll-down))
  :config
  (defun my-elfeed-toggle-unread ()
    (interactive)
    (let ((filter elfeed-search-filter))
      (if (string-match " \\+unread" filter)
          (setq filter (replace-regexp-in-string " \\+unread" "" filter))
        (setq filter (replace-regexp-in-string "$" " +unread" filter)))
      (elfeed-search-set-filter filter)))

  (defun my-elfeed-add-week ()
    (interactive)
    (let (( curr-filter elfeed-search-filter ))
      (string-match "\\@\\([0-9]+\\)-weeks-ago" curr-filter)
      (elfeed-search-set-filter (replace-regexp-in-string "[0-9]+" (number-to-string (1+ (string-to-number (match-string 1 curr-filter)))) curr-filter))))

  (defun my-elfeed-sub-week ()
    (interactive)
    (let (( curr-filter elfeed-search-filter ))
      (string-match "\\@\\([0-9]+\\)-weeks-ago" curr-filter)
      (elfeed-search-set-filter (replace-regexp-in-string "[0-9]+" (number-to-string (1- (string-to-number (match-string 1 curr-filter)))) curr-filter))))

  (defun my-elfeed-scroll-up ()
    (interactive)
    (other-window 1)
    (unless (eobp) (scroll-up-command))
    (other-window 1))

  (defun my-elfeed-scroll-down ()
    (interactive)
    (other-window 1)
    (unless (bobp) (scroll-down-command))
    (other-window 1))

  (defun my-elfeed-open ()
    (interactive)
    (split-window-horizontally)
    (other-window 1)
    (elfeed-search-show-entry (elfeed-search-selected t))
    (enlarge-window-horizontally 5)
    (other-window 1))

  (defun elfeed-play-with-mpv-at-quality (quality-val entry)
    (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
    (if (< 0 quality-val)
        (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val))
      (setq quality-arg ""))
    (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry)))

  (defun elfeed-play-with-mpd ()
    "Play entry link with mpv."
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
      (message "Opening %s with mpd..." (elfeed-entry-link entry))
      (start-process "elfeed-mpd" nil (expand-file-name "podcast") (elfeed-entry-link entry))))

  (defun elfeed-play-with-mpv ()
    "Play entry link with mpv."
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
          (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "1080" "720" "360" "480"))))
      (elfeed-play-with-mpv-at-quality (string-to-number quality-val) entry)))

  (defvar elfeed-video-patterns
    '("youtu\\.?be")
    "List of regexp to match against elfeed entry link to know
whether to use mpv to visit the link.")

  (defvar elfeed-podcast-patterns
    '("acast\\.?com")
    "List of regexp to match podcast")

  (defun elfeed-visit-or-play ()
    "Play video, podcast or visit entry based on `elfeed-video-patterns'."
    (interactive)
    (let ((entry
           (if (eq major-mode 'elfeed-show-mode)
               elfeed-show-entry
             (elfeed-search-selected :single)))
          (video-patterns elfeed-video-patterns)
          (podcast-patterns elfeed-podcast-patterns))
      (while (and video-patterns
                  (not (string-match (car video-patterns) (elfeed-entry-link entry))))
        (setq video-patterns (cdr video-patterns)))
      (while (and podcast-patterns
                  (not (string-match (car podcast-patterns) (elfeed-entry-link entry))))
        (setq podcast-patterns (cdr podcast-patterns)))
      (message "%s %s" podcast-patterns video-patterns)
      (cond
       (podcast-patterns
        (elfeed-play-with-mpd)
        (elfeed-search-untag-all-unread))
       (video-patterns
        (elfeed-play-with-mpv)
        (elfeed-search-untag-all-unread))
       ((eq major-mode 'elfeed-search-mode)
        (elfeed-search-browse-url))
       (t
        (elfeed-show-visit)))))

  (defun my-elfeed-read-regex (regex)
    (interactive
     (list
      (read-string
       "Mark as read: "
       (let ((feed (elfeed-entry-feed (car (elfeed-search-selected)))))
         (when feed
           (or (elfeed-meta feed :title) (elfeed-feed-title feed)))))))
    (setq cur_line (line-number-at-pos))
    (beginning-of-buffer)
    (while (re-search-forward regex nil t)
      (elfeed-search-untag-all-unread))
    (elfeed-search-update--force)
    (message "Done")
    (goto-line cur_line))

  (defun my-elfeed-mark-all-author-as-read ()
    (interactive)
    (my-elfeed-read-regex
     (elfeed-meta (car (elfeed-search-selected)) :author)))
  (elfeed-org))

(use-package elfeed-org :ensure t :after elfeed :config (elfeed-org))
