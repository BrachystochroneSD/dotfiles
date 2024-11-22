(use-package smtpmail :ensure t)
(use-package shr :ensure t)

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :after (smtpmail shr)
  :bind (("S-<f5>" . mu4e))
  :hook
  (mu4e-compose-mode . flyspell-mode)
  (mu4e-compose-mode . ambrevar/mu4e-compose-maybe-signed-and-crypted)
  (mu4e-view-mode . my-mu4e-view-hook)
  (mu4e-headers-mode . my-mu4e-header-hook)
  (mu4e-compose-mode . my-mu4e-compose-hook)
  (mu4e-main-mode . my-mu4e-main-hook)
  :init
  (setq
   ;; General
   message-kill-buffer-on-exit t
   mu4e-context-policy 'pick-first
   mu4e-confirm-quit nil
   mu4e-compose-dont-reply-to-self t
   mu4e-save-multiple-attachments-without-asking t
   mu4e-headers-skip-duplicates t
   mu4e-headers-include-related nil
   mu4e-date-format-long "%d/%m/%Y"
   mu4e-headers-date-format "%d/%m/%Y"
   mu4e-headers-long-date-format "%d/%m/%Y"

   ;; images
   mu4e-view-show-images t
   mu4e-view-image-max-width 1000

   ;; ACCOUNT
   user-mail-address "samueld@mailo.com"
   user-full-name "Samuel D"

   ;; SMTP
   smtpmail-default-smtp-server "mail.mailo.com"
   smtpmail-smtp-server "mail.mailo.com"
   smtpmail-smtp-service 587

   ;; sync
   mu4e-get-mail-command "offlineimap"
   mu4e-maildir (expand-file-name "~/.mail")

   ;; Sent
   mu4e-sent-messages-behavior 'sent ;; Unsure how this should be configured
   message-send-mail-function 'smtpmail-send-it

   ;; Folders
   mu4e-sent-folder "/archives/sent"
   mu4e-drafts-folder "/drafts"
   mu4e-attachment-dir "/home/sam/Downloads"

   ;; Setup Shortcuts
   mu4e-maildir-shortcuts '(("/mailo/*"             . ?M)
                            ("/umons/*"             . ?U)
                            ("/gmail/*"             . ?G)
                            ("/outlook/*"           . ?O)

                            ;; archives
                            ("/archives/"          . ?A)
                            ("/archives/important" . ?i)
                            ("/archives/umons"     . ?u)
                            ("/archives/gmail"     . ?g)
                            ("/archives/mailo"     . ?m)

                            ("/archives/sent"      . ?s)
                            ("/trash"              . ?t)
                            ("/drafts"             . ?d))

   ;; luminance for dark theme
   shr-color-visible-luminance-min 80)

  (setq mu4e-refile-folder
        ;; Taken from : https://www.djcbsoftware.nl/code/mu/mu4e/Smart-refiling.html
        (lambda (msg)
          (cond
           ((mu4e-message-contact-field-matches msg '(:to :cc) "samueld@mailo.com")
            "/archives/mailo")
           ((mu4e-message-contact-field-matches msg '(:to :cc) "mlkjazeriulkjoiu@netc.it")
            "/archives/mailo")
           ((mu4e-message-contact-field-matches msg '(:to :cc) "samrenfou@hotmail.com")
            "/archives/outlook")
           ((mu4e-message-contact-field-matches msg '(:to :cc) "samuel.dawant@alumni.umons.ac.be")
            "/archives/umons")
           ((mu4e-message-contact-field-matches msg '(:to :cc) "mrsamrenfou@gmail.com")
            "/archives/gmail")
           ;; messages sent by me go to the sent folder
           ((cl-find-if
             (lambda (addr)
               (mu4e-message-contact-field-matches msg :from addr))
             (mu4e-personal-addresses))
            mu4e-sent-folder)
           (t  "/archives/misc"))))
  :config
  (defun shr-render-current-buffer ()
    (shr-render-region (point-min) (point-max)))

  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser))
  (setq mu4e-html2text-command 'shr-render-current-buffer)


  (defun my-mu4e-important-refile ()
    (interactive)
    (let ((mu4e-refile-folder "/archives/important"))
      (mu4e-headers-mark-for-refile)))

  (defvar my-mu4e-account-alist
    '(("mailo"
       (user-mail-address "samueld@mailo.com")
       (user-full-name "\"Samuel D\"")
       (smtpmail-smtp-user "samueld@mailo.com")
       (smtpmail-local-domain "mailo.com")
       (smtpmail-default-smtp-server "mail.mailo.com")
       (smtpmail-smtp-server "mail.mailo.com")
       (smtpmail-smtp-service 587)
       )
      ;; ("outlook"
      ;;  (mu4e-sent-folder "/outlook/Sent")
      ;;  (user-mail-address "samrenfou@hotmail.com")
      ;;  (user-full-name "\"Samuel D\"")
      ;;  (smtpmail-smtp-user "samrenfou@hotmail.com")
      ;;  (smtpmail-local-domain "office365.com")
      ;;  (smtpmail-default-smtp-server "outlook.office365.com")
      ;;  (smtpmail-smtp-server "outlook.office365.com")
      ;;  (smtpmail-smtp-service 587)
      ;;  )
      ("umons"
       (user-mail-address "samuel.dawant@alumni.umons.ac.be")
       (user-full-name "\"Samuel Dawant\"")
       (smtpmail-smtp-user "120781@umons.ac.be")
       (smtpmail-local-domain "office365.com")
       (smtpmail-default-smtp-server "outlook.office365.com")
       (smtpmail-smtp-server "outlook.office365.com")
       (smtpmail-smtp-service 587)
       )
      ("gmail"
       (user-mail-address "mrsamrenfou@gmail.com")
       (user-full-name "\"Samuel D\"")
       (smtpmail-smtp-user "mrsamrenfou@gmail.com")
       (smtpmail-local-domain "gmail.com")
       (smtpmail-default-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-server "smtp.gmail.com")
       (smtpmail-smtp-service 587)
       )))

  (defun my-mu4e-set-account ()
    "Set the account for composing a message.
   This function is taken from:
     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
    (let* ((account
            (if mu4e-compose-parent-message
                (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                  (string-match "\\(/archives\\)?/\\([^/]*\\)/*" maildir)
                  (match-string 2 maildir))
              (completing-read (format "Compose with account: (%s) "
                                       (mapconcat #'(lambda (var) (car var))
                                                  my-mu4e-account-alist "/"))
                               (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                               nil t nil nil (caar my-mu4e-account-alist))))
           (account-vars (cdr (assoc account my-mu4e-account-alist))))
      (when (not account-vars)
        (cdr (assoc "mailo" my-mu4e-account-alist))) ;; add a default value (main address)
      (mapc #'(lambda (var)
                (set (car var) (cadr var)))
            account-vars)))

  (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

  ;; Include a bookmark to open all of my inboxes
  (add-to-list 'mu4e-bookmarks
               '(:name "all"
                       :query "maildir:/umons/* OR maildir:/mailo/* OR maildir:/gmail/*"
                       :key ?a))

  (add-to-list 'mu4e-bookmarks
               '(:name "sents"
                       :query "maildir:/sent OR maildir:/umons/Sent OR maildir:/mailo/Sent OR maildir:/gmail/Sent"
                       :key ?s))

  (define-key mu4e-main-mode-map "u" 'mu4e-update-index)

  (defun samueld\attach-pub-key ()
    (interactive)
    (mml-attach-file "~/.gnupg/mailo.pub.acs"))

  (defun my-mu4e-compose-hook ()
    (interactive)
    (local-set-key (kbd "<tab>") 'samueld\mu4e-tab)
    (local-set-key (kbd "C-c k") 'samueld\attach-pub-key)
    (define-key evil-insert-state-map (kbd "<tab>") 'samueld\mu4e-tab))

  (defun my-mu4e-view-hook ()
    (interactive)
    (local-set-key (kbd "<tab>") 'shr-next-link)
    (local-set-key (kbd "<backtab>") 'shr-previous-link)
    (local-set-key (kbd "C-j") 'my-mu4e-delete-junk))

  (defun my-mu4e-header-hook ()
    (interactive)
    (local-set-key (kbd "X") (lambda () (interactive) (mu4e-mark-execute-all t)))
    (setq mu4e-headers-include-related nil)
    (local-set-key (kbd "M-D") 'my-mu4e-delete-from)
    (local-set-key (kbd "M-r") 'my-mu4e-refile-subject)
    (local-set-key (kbd "M-R") 'my-mu4e-refile-from)
    (local-set-key (kbd "i") 'my-mu4e-important-refile)
    (local-set-key (kbd "C-D") 'my-mu4e-delete-subject)
    (local-set-key (kbd "C-j") 'my-mu4e-delete-junk))

  (defun my-mu4e-main-hook ()
    (interactive)
    (local-set-key (kbd "M-C") 'brachystochronesd/compose-encrypted))

  ;; Troestler mu4e iCal
  ;; (setq mu4e-view-use-gnus t)
  ;; (require 'mu4e-icalendar)
  ;; (mu4e-icalendar-setup)

  ;; maildir in headers fields
  (setq mu4e-headers-fields
        '((:date          .  12)    ;; or :human-date
          (:maildir       .  15)
          (:flags         .   6)
          (:from          .  22)
          (:subject       .  nil)))

  (defun my-mu4e-mark-as-refile-regexp (regexp)
    (interactive
     (list (read-string "my-mu4e-mark-as-refile-regexp: ")))
    (save-excursion
      (beginning-of-buffer)
      (let ((num 0))
        (while (re-search-forward regexp nil t)
          (setq num (+ num 1))
          (mu4e-headers-mark-for-refile))
        (message "%d mail marked for archive" num))))

  (defun my-mu4e-mark-as-delete-regexp (regexp)
    (interactive
     (list (read-string "my-mu4e-mark-as-delete-regexp: ")))
    (save-excursion
      (beginning-of-buffer)
      (let ((num 0))
        (while (re-search-forward regexp nil t)
          (setq num (+ num 1))
          (mu4e-headers-mark-for-delete))
        (message "%d mail marked for Delete" num))))

  (defun my-mu4e-delete-junk ()
    (interactive)
    (my-mu4e-mark-as-delete-regexp "/Junk")
    (mu4e-mark-execute-all t))

  (defun my-mu4e-delete-from ()
    (interactive)
    (my-mu4e-mark-as-delete-regexp (caar (mu4e-field-at-point :from))))

  (defun my-mu4e-refile-from ()
    (interactive)
    (my-mu4e-mark-as-refile-regexp (caar (mu4e-field-at-point :from))))

  (defun my-mu4e-refile-subject ()
    (interactive)
    (my-mu4e-mark-as-refile-regexp (mu4e-field-at-point :subject)))

  (defun my-mu4e-delete-subject ()
    (interactive)
    (my-mu4e-mark-as-delete-regexp (mu4e-field-at-point :subject)))

  (defun samueld\mu4e-tab ()
    (interactive)
    (cond
     ;; ((looking-back "^To:.*")
     ;;  (re-search-forward "Subject: *"))
     ((looking-back "^Subject:.*")
      (goto-char (point-max)))
     (t
      (message-tab)))
    )

  ;; Encryption by Ambrevar

  ;; (setq mml-secure-smime-sign-with-sender )
  (setq mm-sign-option nil)
  (setq mml-secure-openpgp-sign-with-sender t)

  (defvar ambrevar/mu4e-compose-signed-p t)

  (defvar ambrevar/mu4e-compose-signed-and-crypted-p nil)

  (defun ambrevar/mu4e-compose-maybe-signed-and-crypted ()
    "Maybe sign or encrypt+sign message.
Message is signed or encrypted+signed when replying to a signed or encrypted
message, respectively.
Alternatively, message is signed or encrypted+signed if
`ambrevar/mu4e-compose-signed-p' or `ambrevar/mu4e-compose-signed-and-crypted-p' is
non-nil, respectively.
This function is suitable for `mu4e-compose-mode-hook'."
    (let ((msg mu4e-compose-parent-message))
      (cond
       ((or ambrevar/mu4e-compose-signed-and-crypted-p
            (and msg (member 'encrypted (mu4e-message-field msg :flags))))
        (mml-secure-message-sign-encrypt))
       ((or ambrevar/mu4e-compose-signed-p
            (and msg (member 'signed (mu4e-message-field msg :flags))))
        (mml-secure-message-sign-pgpmime)))))

  (defun brachystochronesd/compose-encrypted ()
    (interactive)
    (let ((ambrevar/mu4e-compose-signed-and-crypted-p t))
      (mu4e-compose-new)
      )))
