(set-frame-parameter (selected-frame) 'alpha '(85 85))
(add-to-list 'default-frame-alist '(alpha 85 85))
;; ajout de nombres dans la marge
(global-linum-mode t)
;;désactive le menu-bar "file edit" inutile en haut
(menu-bar-mode -1)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;melpa and marmalade packages rep
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.milkbox.net/packages/") t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (gruvbox-dark-medium)))
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" default)))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(org-agenda-files (quote ("~/Dropbox/a_faire.org")))
 '(package-selected-packages
   (quote
    (base16-theme undo-tree tide php-mode org-bullets nginx-mode multiple-cursors multi-web-mode md-readme jdee gruvbox-theme gnuplot-mode elfeed-org auctex android-mode ace-window)))
 '(send-mail-function (quote mailclient-send-it)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;dir packages ajouté
(add-to-list 'load-path "~/.emacs.d/packages/")

(load-theme 'base16-wal t)
;;Highlited replacement
(delete-selection-mode 1)

;;Binding custom début et fin de ligne
(global-set-key (kbd "M-<left>") 'move-beginning-of-line)
(global-set-key (kbd "M-<right>") 'move-end-of-line)

;;resoudre le probleme des terminaux et du backspace (inutile en GUI mode)
;; (global-set-key (kbd "C-h") 'backward-kill-word)
;; (global-set-key (kbd "M-?") 'help)

;; outline-magic

;; (add-hook 'outline-mode-hook
;;           (lambda ()
;;             (require 'outline-cycle)))

;; (add-hook 'outline-minor-mode-hook
;;           (lambda ()
;;             (require 'outline-magic)
;;             (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))

;; (add-hook 'LaTeX-mode-hook
;; 	  (lambda ()
;; 	    (setq outline-promotion-headings
;; 		  '("\\section" "\\chapter" "\\subsection" "\\subsubsection" ))))

;;move-lineS
(require 'move-lines)
;; (move-lines-binding)
(global-set-key (kbd "M-<up>") 'move-lines-up)
(global-set-key (kbd "M-<down>") 'move-lines-down)
(global-set-key (kbd "C-M-:") 'move-lines-up)
(global-set-key (kbd "C-M-;") 'move-lines-down)

;;undo tree, c'est pété
(require 'undo-tree)
(global-undo-tree-mode 1)
(setq undo-tree-enable-undo-in-region nil)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-M-z") 'undo-tree-redo)

;;auto pair with electric pair mode + adding curly brackets
(electric-pair-mode 1)  
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            ) )

;;binding windows
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-<down>") 'windmove-down)
(global-set-key (kbd "C-M-<up>") 'windmove-up)

(global-set-key (kbd "C-M-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-S-<down>") 'shrink-window)
(global-set-key (kbd "C-M-S-<up>") 'enlarge-window)

(global-set-key (kbd "C-M-<backspace>") 'delete-window)
(global-set-key (kbd "C-M-S-<backspace>") 'kill-buffer-and-window)

(global-set-key (kbd "C-M-<return>") 'split-window-horizontally)
(global-set-key (kbd "C-M-S-<return>") 'split-window-vertically)

;; ;;binding ace-window
;; (require 'ace-window)
;; (global-set-key (kbd "C-x C-<down>") 'other-window)
;; (global-set-key (kbd "C-x C-<up>") 'ace-window)

;;mark-multiple/multiple cursor
(add-to-list 'load-path "~/.emacs.d/packages/mark-multiple.el-master/")
(require 'multiple-cursors)
(global-set-key (kbd "M-l") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-m") 'mc/mark-next-like-this)
(global-set-key (kbd "M-o") 'mc/mark-all-like-this)
;; (global-set-key (kbd "C-*") 'mc/edit-lines)

;;mark-multiple pour le mode html
(add-hook 'sgml-mode-hook
          (lambda ()
            (require 'rename-sgml-tag)
            (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))

;;multiple-web-mode
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;;visual-line-mode for latex et raccourcis
(add-hook 'LaTeX-mode-hook #'visual-line-mode)

(global-set-key (kbd "<f6>") 'visual-line-mode)

;;AucteX "me fait pas chier avec tes messages de confirmation quand je compile"
(setq TeX-save-query nil)
(setq TeX-clean-confirm nil)

;; electric $

;; (setq TeX-electric-math (cons "$" "$"))

;;nouveau tex-command Compile&Clean, qui compile deux fois et supprime les fichiers .log .aux et .toc
;; (eval-after-load "tex"
;;   '(add-to-list 'TeX-command-list
;; 		'("Compile&Clean" "%`%l%(mode)%' %t && %`%l%(mode)%' %t && rm *.aux *.toc *.log *.out *.snm *.nav" TeX-run-command nil t :help "compile et clean") t))

;;nouveau tex-command Xelatex
(add-hook 'LaTeX-mode-hook 
          (lambda()
             (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))))

;;Binding pour le heading
(add-hook 'TeX-mode-hook 
	  (lambda () 
	    (outline-minor-mode)
	    (local-set-key (kbd "M-v") 'outline-previous-visible-heading)
	    (local-set-key (kbd "C-v") 'outline-next-visible-heading)
	    ))

(add-hook 'LaTeX-mode-hook 
	  (lambda () 
	    (outline-minor-mode)
	    (local-set-key (kbd "M-v") 'outline-previous-visible-heading)
	    (local-set-key (kbd "C-v") 'outline-next-visible-heading)
	    ))

;;auto-insert pour des templates lors de la création de nouveaux fichiers
(auto-insert-mode)
(setq auto-insert-directory "~/.mytemplates/") ;; dossier custom à créer
(setq auto-insert-query nil) ;; supprimer la demande de confirmation pour l'insertion

;;les auto-insert custom du dossier .mytemplates/ pour des extensions prédéfinies, kile est mort, vive emacs.
(define-auto-insert "\.tex" "tex_template.tex")

;;auctex binding
(add-hook 'TeX-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<C-return>") 'LaTeX-close-environment)
	    (local-set-key (kbd "M-RET") 'LaTeX-insert-item)))


(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<C-return>") 'LaTeX-close-environment)
	    (local-set-key (kbd "M-RET") 'LaTeX-insert-item)))

;;put the autocompletion at Shift-TaB (apparemment appelé backtab)
(global-set-key (kbd "<backtab>") 'completion-at-point)

;;New font et tool-bar-mode de merde
(set-default-font "Inconsolata-14:regular")

(add-to-list 'default-frame-alist
	     '(font . "Inconsolata-14:regular"))

(tool-bar-mode 0)
(scroll-bar-mode 0)

;;Next-buffer et previous-buffer customization (mon premier code lisp bordel, c'est beau)
(defun my-next-buffer ()
  "next-buffer, only skip buffer nuls"
  (interactive)
  (next-buffer)
  (if (not (numberp (compare-strings "*" 0 1 (buffer-name) 0 1))) (my-next-buffer))
  )

(global-set-key (kbd "C-x C-<right>") 'my-next-buffer)
(global-set-key (kbd "C-c C-<right>") 'next-buffer)
(global-set-key (kbd "C-x C-l") 'my-next-buffer)
(global-set-key (kbd "C-c C-l") 'next-buffer)

(defun my-previous-buffer ()
  "previous-buffer, only skip buffer nuls"
  (interactive)
  (previous-buffer)
  (if (not (numberp (compare-strings "*" 0 1 (buffer-name) 0 1))) (my-previous-buffer))
  )
(global-set-key (kbd "C-x C-<left>") 'my-previous-buffer)
(global-set-key (kbd "C-c C-<left>") 'previous-buffer)
(global-set-key (kbd "C-x C-j") 'my-previous-buffer)
(global-set-key (kbd "C-c C-j") 'previous-buffer)

;;better bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda ()
			   (org-bullets-mode 1)
			   (local-set-key (kbd "C-c <C-return>") 'org-open-at-point)
			   (local-set-key (kbd "C-c C-l") 'next-buffer)))


;; ouvre automatiquement les choses à faire de la dropbox synchronisée avec mes autres pc
;;(find-file "~/Dropbox/a_faire.org")


(show-paren-mode 1)

;;calendar
(global-set-key (kbd "<f8>") 'calendar)

(setq diary-file "~/Dropbox/diary")


;;autorefresh sur le docview pdf
(add-hook 'DocView-mode-hook
	  (lambda ()
	    (auto-revert-mode)))

;;Org-mode agenda
(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-include-diary t)

;;comment line
(global-set-key (kbd "C-c ;") 'comment-line)



;; Elfeed
(global-set-key (kbd "<f5>") 'elfeed)
(require 'elfeed)
(require 'elfeed-org)
(elfeed-org)
(setq rmh-elfeed-org-files (list "~/.emacs.d/rss/rss.org"))
(setq-default elfeed-search-filter "@1-week-ago +unread")

(defun elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (quality-arg "")
        (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720") nil nil)))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
    (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry))))

(defvar elfeed-mpv-patterns
  '("youtu\\.?be" "facebo\\.?ok")
  "List of regexp to match against elfeed entry link to know
whether to use mpv to visit the link.")

(defun elfeed-visit-or-play-with-mpv ()
  "Play in mpv if entry link matches `elfeed-mpv-patterns', visit otherwise.
See `elfeed-play-with-mpv'."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (patterns elfeed-mpv-patterns))
    (while (and patterns (not (string-match (car patterns) (elfeed-entry-link entry))))
      (setq patterns (cdr patterns)))
    (if patterns
        (elfeed-play-with-mpv)
      (if (eq major-mode 'elfeed-search-mode)
          (elfeed-search-browse-url)
        (elfeed-show-visit)))))

(define-key elfeed-search-mode-map "b" 'elfeed-visit-or-play-with-mpv)


;; Mu4e

(set-language-environment "UTF-8")

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(require 'mu4e)

(global-set-key (kbd "S-<f5>") 'mu4e)

(setq message-kill-buffer-on-exit t)
(setq mu4e-context-policy 'pick-first)
(setq mu4e-confirm-quit nil)

(setq
 mu4e-view-show-images t
 mu4e-view-image-max-width 1000)

;; default
(setq mu4e-maildir (expand-file-name "~/.mail"))

;; don't save message to Sent Messages, GMail/IMAP will take care of this
;; (setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/outlook/Inbox"     . ?O)
        ("/umons/Inbox"       . ?U)
        ("/gmail/INBOX"       . ?G)
        ("/sent"              . ?s)
        ("/trash"	      . ?t)
        ("/drafts"	      . ?d)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")


;; html mail
(require 'shr)

(defun shr-render-current-buffer ()
  (shr-render-region (point-min) (point-max)))

(setq mu4e-compose-dont-reply-to-self t)
(setq mu4e-html2text-command 'shr-render-current-buffer)

;; I have my "default" parameters from Gmail
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it)
(setq mu4e-sent-folder "/sent"
      mu4e-sent-messages-behavior 'delete ;; Unsure how this should be configured
      mu4e-drafts-folder "/drafts"
      user-mail-address "samrenfou@hotmail.com"
      smtpmail-default-smtp-server "smtp.office365.com"
      smtpmail-smtp-server "smtp.office365.com"
      smtpmail-smtp-service 587)


;; Now I set a list of 
(defvar my-mu4e-account-alist
  '(("gmail"
     (mu4e-sent-folder "/gmail/Sent")
     (user-mail-address "mrsamrenfou@gmail.com")
     (smtpmail-smtp-user "mrsamrenfou@gmail.com")
     (smtpmail-local-domain "gmail.com")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-service 587)
     )
    ("outlook"
     (mu4e-sent-folder "/outlook/Sent")
     (user-mail-address "samrenfou@hotmail.com")
     (smtpmail-smtp-user "samrenfou@hotmail.com")
     (smtpmail-local-domain "office365.com")
     (smtpmail-default-smtp-server "outlook.office365.com")
     (smtpmail-smtp-server "outlook.office365.com")
     (smtpmail-smtp-service 587)
     )
    ("umons"
     (mu4e-sent-folder "/umons/Sent")
     (user-mail-address "120781@umons.ac.be")
     (smtpmail-smtp-user "120781@umons.ac.be")
     (smtpmail-local-domain "office365.com")
     (smtpmail-default-smtp-server "outlook.office365.com")
     (smtpmail-smtp-server "outlook.office365.com")
     (smtpmail-smtp-service 587)
     )
    ))

(defun my-mu4e-set-account ()
  "Set the account for composing a message.
   This function is taken from: 
     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
  (let* ((account
    (if mu4e-compose-parent-message
        (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
    (string-match "/\\(.*?\\)/" maildir)
    (match-string 1 maildir))
      (completing-read (format "Compose with account: (%s) "
             (mapconcat #'(lambda (var) (car var))
            my-mu4e-account-alist "/"))
           (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
           nil t nil nil (caar my-mu4e-account-alist))))
   (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
  (mapc #'(lambda (var)
      (set (car var) (cadr var)))
        account-vars)
      (error "No email account found"))))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

;; Include a bookmark to open all of my inboxes
(add-to-list 'mu4e-bookmarks
       (make-mu4e-bookmark
        :name "sents"
        :query "maildir:/outlook/Sent OR maildir:/umons/Sent OR maildir:/gmail/Sent"
        :key ?i))

(define-key mu4e-main-mode-map "u" 'mu4e-update-index)

(setq shr-color-visible-luminance-min 80)
(add-hook 'mu4e-view-mode-hook
	  (lambda()
	    ;; try to emulate some of the eww key-bindings
	    (local-set-key (kbd "<tab>") 'shr-next-link)
	    (local-set-key (kbd "<backtab>") 'shr-previous-link)))

;;text scale adjust

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; unset keys hbn, to be the arrow keys
(global-set-key (kbd "C-,") 'forward-char)
(define-key key-translation-map (kbd "C-?") (kbd "C-S-f"))
(define-key key-translation-map (kbd "M-?") (kbd "M-S-f"))
(global-set-key (kbd "M-,") 'forward-word)

(define-key key-translation-map (kbd "C-h") (kbd "C-p"))
(define-key key-translation-map (kbd "C-S-h") (kbd "C-S-p"))
(global-set-key (kbd "M-h") 'backward-paragraph)

(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "C-M-,") 'windmove-right)
(global-set-key (kbd "C-M-b") 'windmove-left)
(global-set-key (kbd "C-M-h") 'windmove-up)
(global-set-key (kbd "C-M-n") 'windmove-down)

(global-set-key (kbd "C-M-?") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-S-b") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-S-h") 'enlarge-window)
(global-set-key (kbd "C-M-S-n") 'shrink-window)

(defun refresh-theme ()
  (progn
    (load-theme 'base16-wal t)))

(defun theme-callback (event)
  (refresh-theme))

(require 'filenotify)
(file-notify-add-watch
  "/home/sam/.emacs.d/base16-wal-theme.el" '(change) 'theme-callback)

;; GOtoline binding

(global-set-key (kbd "C-M-g") 'goto-line)
