;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(setq visible-bell nil)
(setq ring-bell-function 'ignore)
;; (setq display-line-numbers 'relative)
(global-display-line-numbers-mode)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode)
(menu-bar-mode -1)
(delete-selection-mode 1)
(show-paren-mode 1)
(setq completion-ignore-case t)
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil)
(put 'downcase-region 'disabled nil)
(global-set-key (kbd "C-x M-k") 'my-kill-current-buffer)

(defun my-kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(package-initialize)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.milkbox.net/packages/") t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("c306e0ef591df6383f666287d0d55767c2c24f3ca137e1c7785c43f08d23c9e8" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" default)))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (gh auctex company eglot dired-hide-dotfiles evil-magit evil-mc evil-mu4e pyim)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#282828")))
 '(send-mail-function (quote mailclient-send-it)))


;;dir packages ajouté
(add-to-list 'load-path "~/.emacs.d/packages/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")

;;;;;;;;;;;;;;;;;
;; AUTO-INSERT ;;
;;;;;;;;;;;;;;;;;

(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/mytemplates/") ;; dossier custom à créer
(setq auto-insert-query nil) ;; supprimer la demande de confirmation pour l'insertion


;; My system name

(defun my-system-name ()
  "Reliable way to get current hostname."
  (substring (shell-command-to-string "hostname") 0 -1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base16 Wal and Gruvbox theme on The Fly ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun refresh-theme ()
  (progn
    (load-theme 'base16-wal t)))

(defun theme-callback (event)
  (when (equal my-current-theme 'base16-wal)
    (refresh-theme)))

(require 'filenotify)

(file-notify-add-watch
 "~/.emacs.d/themes/base16-wal-theme.el" '(change) 'theme-callback)

(load-theme 'gruvbox-dark-medium)

(defvar my-theme-gruv 'gruvbox-dark-medium)
(defvar my-theme-wal 'base16-wal)
(defvar my-current-theme my-theme-gruv)

;; disable other themes before loading new one
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new one."
  (mapc #'disable-theme custom-enabled-themes))

(defun my-next-theme (theme)
  (if (eq theme 'default)
      (disable-theme my-current-theme)
    (progn
      (load-theme theme t)))
  (setq my-current-theme theme))

(defun my-toggle-theme ()
  (interactive)
  (cond ((eq my-current-theme my-theme-gruv)
         (my-next-theme my-theme-wal))
        ((eq my-current-theme my-theme-wal)
         (my-next-theme my-theme-gruv))))

(global-set-key (kbd "<f10>") 'my-toggle-theme)

;;;;;;;;;;;;;;;;;;;;;
;; Custom Bindings ;;
;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-<left>") 'move-beginning-of-line)
(global-set-key (kbd "M-<right>") 'move-end-of-line)

;;;;;;;;;;;;;;;;
;; Move-LineS ;;
;;;;;;;;;;;;;;;;

(require 'move-lines)

(global-set-key (kbd "M-<up>") 'move-lines-up)
(global-set-key (kbd "M-<down>") 'move-lines-down)
(global-set-key (kbd "C-M-:") 'move-lines-up)
(global-set-key (kbd "C-M-;") 'move-lines-down)

;;;;;;;;;;;;;;;
;; Undo Tree ;;
;;;;;;;;;;;;;;;

(require 'undo-tree)
(global-undo-tree-mode 1)
(setq undo-tree-enable-undo-in-region nil)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-M-z") 'undo-tree-redo)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Electric Pair Mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(electric-pair-mode 1)
(setq electric-pair-pairs '((?\" . ?\")
                            (?\{ . ?\})
                            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings For Windows Move ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(global-set-key (kbd "C-M-y") 'other-frame)

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


(defun my-split-window-vertically ()
  (interactive)
  (split-window-vertically)
  (set-window-buffer (next-window) (other-buffer)))

(defun my-split-window-horizontally ()
  (interactive)
  (split-window-horizontally)
  (set-window-buffer (next-window) (other-buffer)))

(global-set-key (kbd "C-M-<return>") 'my-split-window-horizontally)
(global-set-key (kbd "C-M-S-<return>") 'my-split-window-vertically)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mark-Multiple/Multiple Cursor ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'multiple-cursors)
(global-set-key (kbd "M-l") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-m") 'mc/mark-next-like-this)
(global-set-key (kbd "M-o") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FONT AND DEFAULT-FRAME ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)
                            (fullscreen . maximized)))



(set-default-font "Inconsolata-15:regular")
(add-to-list 'default-frame-alist
             '(font . "Inconsolata-15:regular"))
  ;; (progn
  ;;   (set-default-font "Consolas-14:regular")
  ;;   (add-to-list 'default-frame-alist
  ;;                '(font . "Consolas-14:regular")))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Change-Buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-next-buffer ()
  "next-buffer, only skip buffer nuls"
  (interactive)
  (next-buffer)
  (if (or (equal "dired-mode" (symbol-name major-mode))
          (equal (substring (buffer-name) 0 1) "*"))
      (my-next-buffer)))

(global-set-key (kbd "C-x C-<right>") 'my-next-buffer)
(global-set-key (kbd "C-c C-<right>") 'next-buffer)
(global-set-key (kbd "C-$") 'my-next-buffer)

(defun my-previous-buffer ()
  "previous-buffer, only skip buffer nuls"
  (interactive)
  (previous-buffer)
  (if (or (equal "dired-mode" (symbol-name major-mode))
	  (equal (substring (buffer-name) 0 1) "*"))
      (my-previous-buffer)))

(global-set-key (kbd "C-x C-<left>") 'my-previous-buffer)
(global-set-key (kbd "C-c C-<left>") 'previous-buffer)
(global-set-key (kbd "M-$") 'my-previous-buffer)

;;;;;;;;;;;;;;
;; Org-Mode ;;
;;;;;;;;;;;;;;

;;TODO bug when inside diredctory with org on its name

(define-auto-insert "\.org" "org_template.org")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(with-eval-after-load 'org
  (add-to-list 'org-structure-template-alist
               '("w" "#+BEGIN_codewl \n?\n#+END_codewl"))
  (add-to-list 'org-structure-template-alist
               '("f" "#+BEGIN_center \n#+ATTR_LATEX: :width 0.45\\linewidth :center\n?\n#+END_center"))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE-INTEGRATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO maybe add hook
(defun ealm-org-heading-return ()
  (interactive)
  (let (( currheading (org-get-heading) ))
    (org-insert-heading-respect-content)
    (when (string-match "Step \\([0-9]\\)+" currheading)
      (insert
       (format "Step %d" (1+ (string-to-number (match-string 1 currheading))))))
    ))

  (defun org-in-keyword-p ()
    "Check if in a keyword line"
    (save-excursion
      (beginning-of-line)
      (looking-at "[\t ]*#[+].*:")))

  (defun org-insert-keyword ()
    "Just insert the beginning of a keyword definition"
    (interactive)
    (end-of-line)
    (newline)
    (insert "#+"))

  (defun my-encircled-with (string)
    "put STRING around the region"
    (save-excursion
      (goto-char (region-end))
      (insert string)
      (goto-char (region-beginning))
      (insert string)))

  (defun my-org-bold ()
    (interactive)
    (if(region-active-p)
        (my-encircled-with "*")
      (insert "**")
      (backward-char)))

  (defun my-org-italic ()
    (interactive)
    (if(region-active-p)
        (my-encircled-with "/")
      (insert "//")
      (backward-char)))

  (defun my-org-underline ()
    (interactive)
    (if(region-active-p)
        (my-encircled-with "_")
      (insert "__")
      (backward-char)))

  (defun org-meta-return (&optional arg)
    (interactive "P")
    (org-check-before-invisible-edit 'insert)
    (or (run-hook-with-args-until-success 'org-metareturn-hook)
        (call-interactively (cond (arg #'org-insert-heading)
                                  ((org-at-table-p) #'org-table-wrap-region)
                                  ((org-in-item-p) #'org-insert-item)
                                  ((org-in-keyword-p) #'org-insert-keyword)
                                  (t #'org-insert-heading)))))
  )

;;Org-LaTeX
(with-eval-after-load 'ox-latex
  (setq org-latex-default-packages-alist '
        (("AUTO" "inputenc" t ("pdflatex"))
         ;; ("T1" "fontenc" t ("pdflatex"))
         (#1="" "graphicx" t)
         (#1# "grffile" t)
         (#1# "longtable" nil)
         (#1# "wrapfig" nil)
         (#1# "rotating" nil)
         ("normalem" "ulem" t)
         (#1# "amsmath" t)
         (#1# "textcomp" t)
         (#1# "amssymb" t)
         (#1# "capt-of" nil)
         (#1# "hyperref" nil)))

  (add-to-list 'org-latex-classes
               '("wlreport"
                 "\\documentclass{wlreport}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setq org-latex-toc-command "\\tableofcontents \\newpage")
  (setq org-export-with-sub-superscripts nil)
  (setq org-export-with-special-strings nil)
  (setq org-latex-default-class "wlreport")
  (setq org-latex-hyperref-template
        "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c},
 pdflang={%L},
 colorlinks=true,
 pdfborderstyle={/S/U/W 1},
 linkcolor=wlblue}") ;; color defined in wlreport class
  )

(defun my-org-insert-bold-star ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (save-excursion
          (insert "*")
          (goto-char (region-beginning))
          (insert "*")))
    (insert "**")
    (backward-char)))

(setq inhibit-compacting-font-caches t)

(defun my-org-mode-hook ()
  (interactive)
  ;; (org-bullets-mode 1)
  ;; (local-set-key (kbd "<C-return>") 'ealm-org-heading-return)
  (local-set-key (kbd "C-*") 'my-org-insert-bold-star)
  (local-set-key (kbd "C-c <C-return>") 'org-open-at-point)
  (local-set-key (kbd "C-,") 'forward-char)
  (local-set-key (kbd "C-c g") 'org-jira-browse-jira-at-point)
  (local-set-key (kbd "C-c i c") 'org-jira-get-pantimamt-from-next-cact)
  (local-set-key (kbd "C-c i h") 'org-jira-get-pantimamt-from-next-hlmu)
  (local-set-key (kbd "C-<tab>") 'mode-line-other-buffer)
  (local-unset-key (kbd "M-h"))
  (local-set-key (kbd "M-h") 'backward-paragraph))

(add-hook 'org-mode-hook 'my-org-mode-hook)

;; Org-mode Agenda
(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-include-diary t)

(setq org-todo-keywords '((sequence "TODO" "STANDBY" "|" "DONE")))

(defun org-jira-html-link (jira)
  (browse-url-default-browser (format "https://jira.worldline.com/browse/%s" jira))
  )

(defun org-jira-break-long-lines ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (not (eobp))
      (end-of-line)
      (when (< 70 (current-column))
        (beginning-of-line)
        (forward-char 70)
        (newline)
        (insert "        ")
        (previous-logical-line))
      (next-logical-line))))

(defun org-jira-browse-jira-at-point ()
  (interactive)
  (superword-mode)
  (let ((word (word-at-point)))
    (if (string-match "BELHPNSFO-[0-9]+" word)
        (org-jira-html-link word)
      (message "no jira name at point")))
  (superword-mode))

(defun org-jira-insert-pantimamt (pan date amount)
  (insert (format "- *PAN:* %s" PAN))
  (org-meta-return)
  (insert (format "*DATETIME:* %s" DATE))
  (org-meta-return)
  (insert (format "*AMOUNT:* %s" AMOUNT)))

(defun org-jira-get-pantimamt-from-next-hlmu ()
  (interactive)
  (save-excursion
    (re-search-forward
     "No type DC *+: \\([0-9]\\{3\\}\\) \\([0-9]\\{7\\}\\) [0-9][0-9] \\([0-9][0-9]\\)")
    (setq PAN (format "6703%s%s%s"
                      (match-string 1)
                      (match-string 2)
                      (match-string 3)))
    (re-search-forward "Date and Hour *: \\([0-3][0-9]-[01][0-9]-[0-9][0-9] [0-9:]+\\)")
    (setq DATE (match-string 1))
    (re-search-forward "Amount *: +\\([0-9,]+ \\(EUR\\|USD\\)\\)")
    (setq AMOUNT (match-string 1))
    )
  (org-jira-insert-pantimamt PAN DATE AMOUNT))

(defun org-jira-get-pantimamt-from-next-cact ()
  (interactive)
  (save-excursion
    (re-search-forward "Card id. +: \\([0-9]+\\)")
    (setq PAN (match-string 1))
    (re-search-forward "Date +: +\\([0-3][0-9]/[01][0-9]/20[0-9][0-9] [0-9:]+\\)")
    (setq DATE (match-string 1))
    (re-search-forward "Acq. am\\(oun\\)?t *: +\\([0-9,]+ \\(EUR\\|USD\\)\\)")
    (setq AMOUNT (match-string 2))
    )
  (org-jira-insert-pantimamt PAN DATE AMOUNT)
  )

;;TODO
(defun org-jira-create-file (jiranum)
  (interactive)
  (write-file (format "~/.emacs.d/orgfiles/Projects/%s/%s-%s/org-file.org"
                      (completing-read "Directory: " (append '("new") (directory-files "~/.emacs.d/orgfiles/Projects" nil "[^.]")))
                      jiranum
                      (read-string "short description of the project"))))

(defun org-jira-create-new-org-file ()
  (interactive)
  (switch-to-buffer "*org-jira-new*")
  (org-mode)
  (let ((jiranum (read-string "Title: " "BELHPNSFO_") ))
    (insert (format "#+TITLE: Test Document of %s\n"
                    jiranum))
    (insert "#+AUTHOR: Samuel Dawant\n")
    (insert "#+OPTIONS: H:5 num:t")
    (insert (format "#+LATEX_HEADER: \\setdocnum{CAD\\_%s}\n" (replace-regexp-in-string "_" "\\\\_" jiranum)))
    (insert "#+LATEX_HEADER: \\setreference{JIRA}\n")
    (insert "#+LATEX_HEADER: \\setrelease{BKS20.04}\n")
    (local-set-key (kbd "C-c C-c") (lambda () (interactive) (org-jira-create-file jiranum))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autorefresh Docview ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'DocView-mode-hook 'auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment Line and Box ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c ;") 'comment-line)
(global-set-key (kbd "C-c b") 'comment-box)

;;;;;;;;;;;;;;;;;;;;;;;
;; Text Scale Adjust ;;
;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;;;;;;;;;;;;;;;;;;;;
;; Set Moving Keys ;;
;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;
;; Goto-Line Binding ;;
;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-M-g") 'goto-line)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun darken-hex-color ()
  (interactive)
  (let ((hex (word-at-point)))
    (backward-word)
    (kill-word 1)
    (insert (format "%s%s%s"
                    (format "%02x" (/ (string-to-number (substring hex 0 -4) 16) 2))
                    (format "%02x" (/ (string-to-number (substring hex 2 -2) 16) 2))
                    (format "%02x" (/ (string-to-number (substring hex 4 6) 16) 2))))))

(defun hex2rgb (&optional x)
  (interactive)
  (let ((hex (or x (read-string "Hex value: "))))
    (message (format "R:%d G:%d B:%d"
		     (string-to-number (substring hex 0 -4) 16)
		     (string-to-number (substring hex 2 -2) 16)
		     (string-to-number (substring hex 4 6) 16)))))

(defun hextodec (&optional x)
  (interactive)
  (let ((hex (or x (read-string "Hex value: "))))
    (message(format "%d" (string-to-number hex 16)))))

(defun hex2dec (str)
  (format "%d" (string-to-number str 16)))

(defun hextodec-at-point ()
  (interactive)
  (message (hextodec (word-at-point))))

(defun dec2bin (i)
  (let ((res ""))
    (while (not (= i 0))
      (if (= 0 (% i 2))
          (setq res (format "0%s" res))
        (setq res (format "1%s" res)))
      (setq i (/ i 2)))
    res))

(defun bin2dec (str)
  (let ((len (length str))
        (index (- (length str) 1))
        (res 0))
    (while (>= index 0)
      (setq res (+ res (* (string-to-number (substring str (- len index 1) (1+ (- len index 1)))) (expt 2 index))))
      (setq index (1- index)))
    res))

(defun hex2bin (str)
  (dec2bin (string-to-number (hex2dec str))))

(defun hex2bin-at-point ()
  (interactive)
  (message (hex2bin (word-at-point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HPNS TRACE MODE (Work in progress) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(autoload 'hpns-trace-mode "hpns-trace-mode" "for shit" t)
(add-to-list 'auto-mode-alist '("\\.hptr\\'" . hpns-trace-mode))

;;;;;;;;;;;
;; DIRED ;;
;;;;;;;;;;;


(setq dired-listing-switches "-alh")
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
    (cond ((string-match "\\(html\\|htm\\)$" file-extension)
           (browse-url-default-browser file))
          ((string-match "\\(pdf\\|djvu\\|ps\\|dvi\\)$" file-extension)
           (org-open-file file))
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
              (re-search-forward file nil t ))))
  )

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
  (dired-jump))

(global-set-key (kbd "C-à") 'dired-jump)
(global-set-key (kbd "M-à") 'my-dired-jump)
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
  (text-scale-decrease 2)
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

;;;;;;;;;;;;;;;;;;
;; IMAGE-VIEWER ;;
;;;;;;;;;;;;;;;;;;

(defun my-image-viewer-hook ()
  (interactive)
  (eimp-mode)
  (eimp-fit-image-to-window nil))

(add-hook 'image-mode-hook 'my-image-viewer-hook)

;;;;;;;;;;;;;;;;;;
;; MY BOOKMARKS ;;
;;;;;;;;;;;;;;;;;;

(defvar my-bookmarks-alist ;; General bm
  `(("notes" . "~/.emacs.d/orgfiles/notes/")
    ("notesgeneral" . "~/.emacs.d/orgfiles/notes/general.org")
    ("packages" . "~/.emacs.d/packages/")
    ("tandemacs" . "~/.emacs.d/packages/tandemacs.el")
    ("hpns" . "~/.emacs.d/packages/hpns-trace-mode.el")
    ("emacs" . "~/.emacs.d/init.el")
    ("buffbackup" . "~/.bufferbackup/")
    ("home" . "~/")
    ("ealm" . "~/.emacs.d/packages/ealm-master/ealm.el")
    ))

;; windows bindings

(cond
 ((equal system-type 'gnu/linux) ;; Linux bm
  (setq my-bookmarks-alist
        (append
         my-bookmarks-alist
         '(
           ("zshrc" . "~/.zshrc")
           ("scripts" . "~/.script/")
           ("doc" . "~/Documents/")
           ("downloads" . "~/Downloads/")
           ("desktop" . "~/Desktop/")
           ("images" . "~/Images/")))))

 ((equal system-type 'window-nt)
  (setq my-bookmarks-alist
        (append
         my-bookmarks-alist
         '(("todo" . ,(concat "c:/Users/" (getenv "username") "/Documents/todolist.org"))
           ("doc" . ,(concat "c:/Users/" (getenv "username") "/Documents/"))
           ("downloads" . ,(concat "c:/Users/" (getenv "username") "/Downloads/"))
           ("desktop" . ,(concat "c:/Users/" (getenv "username") "/Desktop/"))
           ("images" . ,(concat "c:/Users/" (getenv "username") "/Pictures/"))))))
 )

(if (equal (my-system-name) "SamuelD-PC") ;; HomePC bm
    (setq my-bookmarks-alist
          (append
           my-bookmarks-alist
           '(("NCfiles" . "c:/Users/Samuel D/Documents/MY_ACTUAL_DOCUMENTS/NCFiles/")
             ("canon77D" . "c:/Users/Samuel D/Pictures/Canon77D/100CANON/")
             ))))

(if (equal (my-system-name) "EBEBRPCL1226") ;; eWL bm
    (setq my-bookmarks-alist
          (append
           my-bookmarks-alist
           '(("macrotacl" . "c:/Users/a757288/Documents/tacl_programs/SDAMACRO.tacl")
             ("meetings" . "~/.emacs.d/orgfiles/meetings/")
             ("manualstacl2" . "c:/Users/a757288/Documents/manuals/tacl2.pdf")
             ("manualstacl" . "c:/Users/a757288/Documents/manuals/tacl.pdf")
             ("chainpincredit" . "c:/Users/a757288/Documents/manuals/chainpincredit.pdf")
             ("cardstcc" . "//cursa/div/TCC/PUB EXT/Certification/CARDS/test cards and tools TCC.xlsm")
             ("projets" . "~/.emacs.d/orgfiles/Projects/")
             ("latexwl" . "c:/Users/a757288/Documents/Programmes/miktex/texmfs/install/tex/latex/worldline/wlreport.cls")
             ("chainhce" . "c:/Users/a757288/Documents/manuals/chain scheme HCE transactions.pdf")
             ("chainbcmc" . "c:/Users/a757288/Documents/manuals/chain scheme BCMC EMV.pdf")
             ("chainatm" . "c:/Users/a757288/Documents/manuals/chain atm.pdf")
             ("netsupport" . "c:/Program Files (x86)/NetSupport/NetSupport Manager/PCICTLUI.EXE")
             ("manuals" . "c:/Users/a757288/Documents/manuals/")
             ("cards" . "//cursa/div/TCC/PUB EXT/Certification/CARDS/")
             ("tracedb" . "c:/Users/a757288/Documents/tracesdb/")
             ("tacl" . "c:/Users/a757288/Documents/tacl_programs/")
             ("tabula" . "c:/Users/a757288/Documents/Programmes/tabula/tabula.exe")
             ("dvl" . "c:/Users/a757288/Documents/Programmes/GUI(s) Dvl/WLP-DVL.htm")
             ("bapof" . "c:/Users/a757288/Documents/Programmes/Bapof/bapof.exe")))))

(defun my-bookmarks (alias)
  (interactive
   (list (completing-read "My-bookmarks goto: " my-bookmarks-alist nil t)))
  (my-dired-find-file-internal (cdr (assoc alias my-bookmarks-alist)) t))

(defun my-bookmarks-add-bm (alias path pc)
  (interactive
   (list
    (read-string "Add bookmark alias: ")
    (read-file-name "Add bookmak pathway: " nil nil t)
    (completing-read "Which PC (General): " '("HomePC" "eWL" "Linux" "General") nil t nil nil "General")))
  (setq my-bookmarks-alist (cons (cons alias path) my-bookmarks-alist))
  (with-temp-buffer
    (find-file "~/.emacs.d/init.el")
    (save-excursion
      (if (equal pc "General")
          (progn
            (re-search-forward "General bm" (beginning-of-buffer))
            (next-logical-line))
        (progn
          (re-search-forward (format "%s bm" pc) (beginning-of-buffer))
          (next-logical-line 4)))
      (end-of-line)
      (newline)
      (indent-for-tab-command)
      (insert (format "(\"%s\" . \"%s\")" alias path))
      (save-buffer)
      (eval-defun t)
      (switch-to-prev-buffer))))

(defun my-bookmarks-remove-bm (alias)
  (interactive
   (list (completing-read "My-bookmarks goto: " my-bookmarks-alist nil t)))
  (setq my-bookmarks-alist (delq (assoc "doc" my-bookmarks-alist) my-bookmarks-alist)))

(global-set-key (kbd "C-ç") 'my-bookmarks)
(global-set-key (kbd "C-M-ç") 'my-bookmarks-remove-bm)
(global-set-key (kbd "M-ç") 'my-bookmarks-add-bm)

;;;;;;;;;;;;;;;;;;;;
;; TANDEM COMMAND ;;
;;;;;;;;;;;;;;;;;;;;

(require 'tandemacs)
;; (require 'eshell) (eshell is required in tandemacs)

(global-set-key (kbd "<f11>") 'eshell)
(global-set-key (kbd "<S-f11>") (lambda ()(interactive)(eshell 3)))
(global-set-key (kbd "<C-f11>") (lambda ()(interactive)(eshell 4)))
(global-set-key (kbd "<M-f11>") (lambda ()(interactive)(eshell 5)))

(global-set-key (kbd "<f5>") (lambda () (interactive) (find-file "c:/Users/a757288/Documents/todolist.org")))

;;;;;;;;;;;;;;;;;;;;;;;;
;; SCRATCH MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-rename-scratch ()
  (interactive)
  (rename-buffer (format "*scratch*<%s>" (read-string "Rename scratch buffer: "))))

(defun my-make-scratch ()
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*scratch*")))

(defun my-next-scratch ()
  (interactive)
  (set-frame-parameter (selected-frame) 'buffer-predicate (lambda (buf)
							    (string-match "*scratch*" (buffer-name buf))))
  (next-buffer)
  (set-frame-parameter (selected-frame) 'buffer-predicate nil))

(global-set-key (kbd "<f12>") 'my-next-scratch)
(global-set-key (kbd "M-<f12>") 'my-make-scratch)
(global-set-key (kbd "C-<f12>") 'my-rename-scratch)

(global-set-key (kbd "C-<tab>") 'mode-line-other-buffer)

;;;;;;;;;;;;
;; ELFEED ;;
;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/packages/elfeed-master/")
(require 'elfeed)
(require 'elfeed-org)
(elfeed-org)
(global-set-key (kbd "<f6>") 'elfeed)
(setq elfeed-search-filter "@3-weeks-ago +unread")

(defun my-elfeed-open ()
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (elfeed-search-show-entry (elfeed-search-selected t))
  (enlarge-window-horizontally 5)
  (other-window 1))

(defun my-elfeed-search-hook-setup ()
  (interactive)
  (local-set-key (kbd "j") 'next-line)
  (local-set-key (kbd "k") 'previous-line)
  (local-set-key (kbd "l") 'my-elfeed-open)
  (local-set-key (kbd "h") 'delete-other-windows)
  (local-set-key (kbd "/") 'isearch-forward)
  (local-set-key (kbd "?") 'isearch-backward)
  )

(add-hook 'elfeed-search-mode-hook 'my-elfeed-search-hook-setup)

(defun elfeed-play-with-vlc ()
  "Play entry link with vlc."
  (interactive)
  (let (( entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)) ))
    (message "Opening %s with vlc..." (elfeed-entry-link entry))
    (my-vlc-launc-and-quit (elfeed-entry-link entry))))

(defun elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (quality-arg "")
        (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720" "1080") nil nil "720")))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
    (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry))))

(defvar elfeed-video-patterns
  '("youtu\\.?be" "facebo\\.?ok")
  "List of regexp to match against elfeed entry link to know
whether to use vlc to visit the link.")

(defun elfeed-visit-or-play-video ()
  "Play with vlc or mpv if entry link matches `elfeed-video-patterns', visit otherwise.
See `elfeed-play-with-vlc'."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (patterns elfeed-video-patterns))
    (while (and patterns (not (string-match (car patterns) (elfeed-entry-link entry))))
      (setq patterns (cdr patterns)))
    (if patterns
        (if (equal system-type 'windows-nt)
            (elfeed-play-with-vlc)
          (elfeed-play-with-mpv))
      (if (eq major-mode 'elfeed-search-mode)
          (elfeed-search-browse-url)
        (elfeed-show-visit)))
    (elfeed-search-untag-all-unread)))

(define-key elfeed-search-mode-map "b" 'elfeed-visit-or-play-video)
(define-key elfeed-search-mode-map "B" 'elfeed-search-browse-url)

(define-key elfeed-search-mode-map "c" 'my-elfeed-copy-url)

(define-key elfeed-search-mode-map "d" 'my-elfeed-read-regex)
(define-key elfeed-search-mode-map "D" 'my-elfeed-mark-all-author-as-read)

(defun my-vlc-launc-and-quit (link)
  "Launch a link with vlc and then quit after the end of the video"
  (start-process "my-vlc-quit" nil "vlc" link "vlc://quit"))

(defun my-elfeed-read-regex (regex)
  (interactive
   (list
    (read-string "Mark as read: "
                 (elfeed-meta (car (elfeed-search-selected)) :author))))
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward regex nil t)
      (elfeed-search-untag-all-unread))
    (elfeed-search-update--force)
    (message "Done")))

(defun my-elfeed-mark-all-author-as-read ()
  (interactive)
  (my-elfeed-read-regex
   (elfeed-meta (car (elfeed-search-selected)) :author)))

(defun my-elfeed-copy-url ()
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single))))
    (message "%s copied in the clipboard." (elfeed-entry-link entry))
    (kill-new (elfeed-entry-link entry))))

;;;;;;;;;;;;;;;
;; TIME MODE ;;
;;;;;;;;;;;;;;;

(setq display-time-format "%Y/%m/%d %H:%M")

(display-time-mode 1)

;;;;;;;;;;;;;;;;;;
;; ALPHA values ;;
;;;;;;;;;;;;;;;;;;
(set-frame-parameter nil 'alpha '(95))
(add-to-list 'default-frame-alist '(alpha 95))

(defun my-make-it-transparentier (x)
  "add X to the current frame alpha value"
  (set-frame-parameter nil 'alpha (list (+ x (car(frame-parameter nil 'alpha)))))
  (message (format "Current alpha: %d"(car(frame-parameter nil 'alpha)))))

(global-set-key (kbd "<f9>") (lambda () (interactive) (my-make-it-transparentier -5)))
(global-set-key (kbd "<C-f9>") (lambda () (interactive) (my-make-it-transparentier 5)))
(global-set-key (kbd "<M-f9>") (lambda () (interactive) (my-make-it-transparentier -10)))
(global-set-key (kbd "<C-M-f9>") (lambda () (interactive) (my-make-it-transparentier 10)))

;;;;;;;;;;;;;;;
;; TACL MODE ;;
;;;;;;;;;;;;;;;

(require 'tacl-mode)

(setq tacl-indent-offset 2)

(define-auto-insert "\.tacl" "tacl_template.tacl")

(add-to-list 'auto-mode-alist '("\\.tacl\\'" . tacl-mode))

(defun my-tacl-return ()
  (interactive)
  (save-excursion (re-search-backward "\\(#[A-Za-z]+ ?\\|== ?\\|>> ?\\)"))
  (insert (format "\n%s" (match-string 0)))
  (indent-for-tab-command))

(defun my-send-file-to-my-dir-in-tandem ()
  (interactive)
  (save-buffer)
  (my-pscp-send-command (buffer-file-name) (replace-regexp-in-string ".tacl" "" (buffer-name))))

(defun my-pscp-send-command (in out)
  (shell-command (format "pscp.exe -l dawant.tstbks.run3 -pw \"mYbXSe:=:G^Rde:LB0:u\" %s dvl2.tandem.banksys.be:/G/BCT7/DZPRISDA/%s" in out) ))

(defun tacl-mode-hook-setup ()
  (local-set-key (kbd "C-c s") 'my-send-file-to-my-dir-in-tandem)
  (local-set-key (kbd "C-M-x") 'my-send-file-to-my-dir-in-tandem)
  (local-set-key (kbd "C-<return>") 'my-tacl-return)
  (add-hook 'completion-at-point-functions 'tacl-completion-at-point nil 'local))

(add-hook 'tacl-mode-hook 'tacl-mode-hook-setup)

;;;;;;;;;;
;; EALM ;;
;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/packages/ealm-master/")

(autoload 'ealm-mode "ealm-mode" "For ALM test construction" t)
(add-to-list 'auto-mode-alist '("\\.ealm\\'" . ealm-mode))

(defun my-ealm-mode-hook ()
  (interactive)
  (add-hook 'completion-at-point-functions 'ealm-completion-at-point nil 'local)
  (local-set-key (kbd "<C-return>") 'ealm-insert-new-step)
  (local-unset-key (kbd "C-c <C-return>"))
  (local-set-key (kbd "C-c <C-return>") 'ealm-ref-goto-ref)
  )

(add-hook 'ealm-mode-hook 'my-ealm-mode-hook)

;; ;;;;;;;;;;;;;;;;;;;;;;
;; ;; UNDO KILL BUFFER ;;
;; ;;;;;;;;;;;;;;;;;;;;;;
;; TODO : MAKE IT WORK
;; (defvar my-last-killed-buffer "")

;; (defun my-undo-kill-buffer-add-buffer ()
;;   (unless (string-match "*" (buffer-name))
;;     (setq my-last-killed-buffer (or (buffer-file-name) (dired-current-directory)))
;;     (message (format "Buffer \"%s\" killed and added to the kill-buffer ring. Press C-S-z to revive him" (buffer-name)))))


;; (defun my-undo-kill-buffer ()
;;   (interactive)
;;   (unless (equal my-last-killed-buffer "")
;;     (when (y-or-n-p (format "Revive %s buffer ?" my-last-killed-buffer))
;;     (find-file my-last-killed-buffer))))


;; (defun kill-buffer-hook-setup ()
;;   (interactive)
;;   (my-undo-kill-buffer-add-buffer))


;; (add-hook 'kill-buffer-hook 'kill-buffer-hook-setup)
;; (global-set-key (kbd "C-S-z") 'my-undo-kill-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;Windows;SSH/PuTTY;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;Pour windows only, utilisation du ssh avec plink
(when (equal (my-system-name) "SamuelD-PC")
  (require 'tramp)
  (modify-coding-system-alist 'process "plink" 'utf-8-unix)
  (setq tramp-default-method "plink"
        tramp-completion-without-shell-p t)
  (setq tramp-verbose 10)
  (setq tramp-debug-buffer t)

  (let ((path (getenv "PATH"))
        (plink (expand-file-name "PuTTY" (getenv "ProgramFiles(x86)"))))
    (setenv "PATH" (concat plink path-separator path)))

  (add-to-list 'exec-path "C:/Program Files (x86)/PuTTY/")
  (add-to-list 'exec-path (expand-file-name "PuTTY" (getenv "ProgramFiles(x86)")))

  (defadvice sql-mysql (around sql-mysql-around activate)
    "SSH to linux, then connect"
    (let ((default-directory "/plink:ZenoCloud:"))
      ad-do-it))

  (defun zenocyne-connect ()
    "connecte aux serveur zenocyne"
    (interactive)
    (find-file "/plink:ZenoCloud:/var/www/")
    )

  (defun my-shell-raspberry ()
    (interactive)
    (let ((default-directory "/plink:pi@192.168.0.104:"))
      (eshell)))
  )

;;;;;;;;;;;;
;; AuCTeX ;;
;;;;;;;;;;;;

;; Path environement variable only for work pc

(if (equal (my-system-name) "EBEBRPCL1226")
    (progn
      (let ((path (getenv "PATH"))
            (miktex (replace-regexp-in-string "/" "\\\\" (expand-file-name "c:/Users/a757288/Documents/Programmes/miktex/texmfs/install/miktex/bin/x64" ))))
        (setenv "PATH" (concat miktex path-separator path)))

      (add-to-list 'exec-path (replace-regexp-in-string "/" "\\\\" (expand-file-name "c:/Users/a757288/Documents/Programmes/miktex/texmfs/install/miktex/bin/x64" )))))

;;AucteX "me fait pas chier avec tes messages de confirmation quand je compile"


(setq TeX-save-query nil)
(setq TeX-clean-confirm nil)

;;nouveau tex-command Compile&Clean, qui compile deux fois et supprime les fichiers .log .aux et .toc (ici spécial windows)
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
                '("Compile&Clean" "pdflatex %s && pdflatex %s && erase *.aux *.toc *.log" TeX-run-command nil t :help "compile 2 fois et clean") t))

;;Modifie le C-c C-c de défault par Compile&Clean et ajoute Ctrl-enter et alt-enter pour respectivement ajouter un nouvel item et fermer un environnement

;;les auto-insert custom du dossier .mytemplates/ pour des extensions prédéfinies, kile est mort, vive emacs.
(define-auto-insert "\.tex" "tex_template.tex")
(add-to-list 'auto-mode-alist '("\\.tex\\'" . TeX-mode))

;; mark multiple for latex

(defun mc--get-env-context ()
  "return a list of the beginning of the env,
the end of the env and the name of the env"
  (beginning-of-line)
  (skip-chars-forward " \t")
  (while (not (looking-at "[\\]begin{"))
    (next-logical-line -1)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (when (looking-at "[\\]end{")
      (error "Not inside an environment")))
  (setq start (point))
  (forward-char 7)
  (setq name (word-at-point))
  (while (not (looking-at "[\\]end{"))
    (next-logical-line 1)
    (beginning-of-line)
    (skip-chars-forward " \t"))
  (setq end (point))
  (vector start end name))

(defun mc/mark-latex-env-pair ()
  "Mark the latex environement tag we're in and its pair
for renaming."
  (interactive)
  (let ((context (save-excursion(mc--get-env-context))))
    (goto-char (aref context 0))
    (let* ((tag-name (aref context 2))
           (num-chars (length tag-name))
           (master-start (+ 7 (point)))
           (mirror-end (save-excursion
                         (goto-char (aref context 1))
                         (+ 5 num-chars (point)))))
      (goto-char (- mirror-end num-chars))
      (set-mark mirror-end)
      (mc/create-fake-cursor-at-point)
      (goto-char master-start)
      (set-mark (+ (point) num-chars))))
  (mc/maybe-multiple-cursors-mode))

(defun my-insert-env-test ()
  (interactive)
  (insert "\\end{}")
  (forward-char -1)
  (mc/create-fake-cursor-at-point)
  (beginning-of-line)
  (insert "\\begin{}\n")
  (forward-char -2)
  (mc/maybe-multiple-cursors-mode))

(defun my-latex-insert-sc ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (insert "}")
        (goto-char (region-beginning))
        (insert "\\textsc{"))
    (insert "\\textsc{}")
    (backward-char)))

(defun my-latex-insert-bold ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (insert "}")
        (goto-char (region-beginning))
        (insert "\\textbf{"))
    (insert "\\textbf{}")
    (backward-char)))

(defun my-latex-insert-italics ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (insert "}")
        (goto-char (region-beginning))
        (insert "\\textit{"))
    (insert "\\textit{}")
    (backward-char)))

(defun my-latex-insert-underline ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (insert "}")
        (goto-char (region-beginning))
        (insert "\\underline{"))
    (insert "\\underline{}")
    (backward-char)))

(defun my-latex-insert-emph ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (insert "}")
        (goto-char (region-beginning))
        (insert "\\emph{"))
    (insert "\\emph{}")
    (backward-char)))

(defun my-TeX-LaTeX-mode-hook-setup ()
  (interactive)
  (add-to-list 'TeX-view-program-selection '(output-pdf "mupdf") )
  (add-to-list 'TeX-view-program-list '("mupdf" "mupdf %o"))
  (local-set-key (kbd "<C-return>") 'LaTeX-insert-item)
  (local-set-key (kbd "<M-return>") 'LaTeX-close-environment)
  (outline-minor-mode)
  (local-set-key (kbd "M-v") 'outline-previous-visible-heading)
  (local-set-key (kbd "C-v") 'outline-next-visible-heading)
  (local-set-key (kbd "C-c M-l") 'TeX-next-error)
  (local-set-key (kbd "C-c i b") 'my-latex-insert-bold)
  (local-set-key (kbd "C-c i s") 'my-latex-insert-sc)
  (local-set-key (kbd "C-c i i") 'my-latex-insert-italics)
  (local-set-key (kbd "C-c i u") 'my-latex-insert-underline)
  (local-set-key (kbd "C-c i e") 'my-latex-insert-emph)
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  )


(add-hook 'TeX-mode-hook 'my-TeX-LaTeX-mode-hook-setup)
(add-hook 'LaTeX-mode-hook 'my-TeX-LaTeX-mode-hook-setup)

;;visual-line-mode pour le latex

(add-hook 'LaTeX-mode-hook #'visual-line-mode)

;;;;;;;;;;;;;;;
;; ENCODING  ;;
;;;;;;;;;;;;;;;

;; (prefer-coding-system 'dos-)
;; (set-default-coding-systems 'dos)
;; (set-language-environment ')
;; (set-selection-coding-system 'dos)

;; /plink:dawant.tstbks.run3@dvl2.tandem.banksys.be:/G/syst02/macros

;; psftp.exe dawant.tstbks.run3@dvl2.tandem.banksys.be

(defvar my-mail-file (concat "c:/Users/" (getenv "username") "Documents/mailhtml/mail.html"))

(defun my-html-mail-creator (string)
  (with-temp-file my-mail-file
    (insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">\n<head><meta charset=\"utf-8\"/></head>\n<body>\n")
    (insert string)
    (insert "</body>\n</html>")))

(defun my-mail-this-buffer ()
  (interactive)
  (my-html-mail-creator (buffer-substring (point-min) (point-max))))

;; random defun

(defun my-bool-test-defun (condition)
  (if condition
      (message "ok")
    (message "nok")))

;;;;;;;;;;;;;;;
;; EVIL MODE ;;
;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/packages/evil-master")

(setq evil-want-C-u-scroll t)
(setq evil-want-fine-undo t)
(require 'evil)
(evil-mode 1)

;; Don't use evil-mode for dired mode. I've already set the key to be vim-like
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'eshell-mode 'emacs)
(evil-set-initial-state 'elfeed-search-mode 'emacs)
(evil-set-initial-state 'elfeed-show-mode 'emacs)
(evil-set-initial-state 'messages-buffer-mode 'emacs)

;; New command:

(evil-ex-define-cmd "kb" 'my-kill-current-buffer)
(evil-ex-define-cmd "kw" 'kill-buffer-and-window)
(evil-ex-define-cmd "wkb" (lambda ()
                            (interactive)(save-buffer)(my-kill-current-buffer)))

;; bind meta space to exit to normal mode (escape is too far !!!)

(define-key evil-insert-state-map (kbd "M-SPC") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(define-key evil-visual-state-map (kbd "M-SPC") 'evil-exit-visual-state)
(define-key evil-replace-state-map (kbd "M-SPC") 'evil-normal-state)
(define-key evil-emacs-state-map (kbd "M-SPC") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "M-SPC") 'evil-emacs-state)

(define-key evil-visual-state-map (kbd "<tab>") 'indent-for-tab-command)
;; (define-key evil-normal-state-map (kbd "<tab>") 'indent-for-tab-command)
(evil-define-key 'insert org-mode-map (kbd "<tab>") #'org-cycle)

(evil-define-key '(insert normal replace visual emacs)
  'global (kbd "C-e") #'move-end-of-line)

;; Get rid of C-z to switch to emacs mode (M-spc used instead)
(evil-define-key '(insert normal replace visual emacs)
  'global (kbd "C-z") #'undo-tree-undo)

(define-key evil-insert-state-map (kbd "<tab>") 'indent-for-tab-command)

(add-hook 'org-metareturn-hook 'evil-insert-state)
(add-hook 'org-insert-heading-hook 'evil-insert-state)

(define-key evil-normal-state-map (kbd "ù") 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "µ") 'evil-end-of-line)

(define-key evil-normal-state-map (kbd "é") 'evil-goto-mark)

(define-key evil-normal-state-map (kbd "J") 'next-line)
(define-key evil-normal-state-map (kbd "K") 'previous-line)

(define-key evil-visual-state-map (kbd "J") 'next-line)
(define-key evil-visual-state-map (kbd "K") 'previous-line)

(define-key evil-normal-state-map (kbd "C-M-j") 'evil-join)


;; evil mode-line gruvbox colors

(defun my-evil-normal-state-hook ()
  (interactive)
  (face-remap-add-relative
   'mode-line '((:background "#cc241d" :foreground "#351717") mode-line)))

(defun my-evil-emacs-state-hook ()
  (interactive)
  (face-remap-add-relative
   'mode-line '((:background "#b16286"       :foreground "#35212b") mode-line)))

(defun my-evil-insert-state-hook ()
  (interactive)
  (face-remap-add-relative
   'mode-line '((:background "#d65d0e"    :foreground "#3f2100") mode-line)))

(defun my-evil-replace-state-hook ()
  (interactive)
  (face-remap-add-relative
   'mode-line '((:background "#d79921"      :foreground "#3f2b00") mode-line)))

(defun my-evil-motion-state-hook ()
  (interactive)
  (face-remap-add-relative
   'mode-line '((:background "#98971a"          :foreground "#2b2b00") mode-line)))

(defun my-evil-visual-state-hook ()
  (interactive)
  (face-remap-add-relative
   'mode-line '((:background "#458588"           :foreground "#212b2b") mode-line)))

(defun my-evil-operator-state-hook ()
  (interactive)
  (face-remap-add-relative
   'mode-line '((:background "#689d6a"    :foreground "#212b21") mode-line)))

(add-hook 'evil-visual-state-entry-hook 'my-evil-visual-state-hook)
(add-hook 'evil-motion-state-entry-hook 'my-evil-motion-state-hook)
(add-hook 'evil-replace-state-entry-hook 'my-evil-replace-state-hook)
(add-hook 'evil-insert-state-entry-hook 'my-evil-insert-state-hook)
(add-hook 'evil-emacs-state-entry-hook 'my-evil-emacs-state-hook)
(add-hook 'evil-normal-state-entry-hook 'my-evil-normal-state-hook)
(add-hook 'evil-operator-state-entry-hook 'my-evil-operator-state-hook)

;;;;;;;;;;
;; PYIM ;;
;;;;;;;;;;

;; (require 'pyim)
;; (set-input-method 'pyim)
;; (global-set-key (kbd "C-=") 'toggle-input-method)

;; (setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
;; (set-language-environment 'utf-8)
;; (setq locale-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-selection-coding-system
;;  (if (eq system-type 'windows-nt)
;;      'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
;;    'utf-8))
;; (prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; MU4E for LINUX ONLY ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(when (equal system-type 'gnu/linux)
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (require 'mu4e)
  (global-set-key (kbd "S-<f5>") 'mu4e)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-confirm-quit nil)

  (setq mu4e-attachment-dir "/home/sam/Downloads")

  (setq
   mu4e-view-show-images t
   mu4e-view-image-max-width 1000)

  (setq mu4e-maildir (expand-file-name "~/.mail"))

  ;; Setup Shortcuts
  (setq mu4e-maildir-shortcuts
        '(("/outlook/Inbox"     . ?O)
          ("/umons/Inbox"       . ?U)
          ("/gmail/INBOX"       . ?G)
          ("/sent"              . ?s)
          ("/trash"	        . ?t)
          ("/drafts"	        . ?d)))

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "offlineimap")

  ;; html mail
  (require 'shr)

  (defun shr-render-current-buffer ()
    (shr-render-region (point-min) (point-max)))

  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser))
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
       (user-mail-address "samuel.dawant@alumni.umons.ac.be")
       (smtpmail-smtp-user "120781@umons.ac.be")
       (smtpmail-local-domain "office365.com")
       (smtpmail-default-smtp-server "outlook.office365.com")
       (smtpmail-smtp-server "outlook.office365.com")
       (smtpmail-smtp-service 587)
       )))

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

  (defun my-mu4e-view-hook ()
    (interactive)
    (local-set-key (kbd "<tab>") 'shr-next-link)
    (local-set-key (kbd "<backtab>") 'shr-previous-link)
    (local-set-key (kbd "C-j") 'my-mu4e-delete-junk))

  (defun my-mu4e-header-hook ()
    (interactive)
    (local-set-key (kbd "X") (lambda () (interactive) (mu4e-mark-execute-all t)))
    (local-set-key (kbd "C-j") 'my-mu4e-delete-junk))

  (add-hook 'mu4e-view-mode-hook 'my-mu4e-view-hook)
  (add-hook 'mu4e-headers-mode-hook 'my-mu4e-header-hook)

  ;; Troestler mu4e iCal
  ;; (setq mu4e-view-use-gnus t)
  ;; (require 'mu4e-icalendar)
  ;; (mu4e-icalendar-setup)

  ;; maildir in headers fields
  (setq mu4e-headers-fields
        '( (:date          .  12)    ;; or :human-date
           (:maildir       .  15)
           (:flags         .   6)
           (:from          .  22)
           (:subject       .  nil)))

  (defun my-mu4e-delete-junk ()
    (interactive)
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward "/Junk" nil t)
        (mu4e-headers-mark-for-delete))
      (mu4e-mark-execute-all t)))

  )

;;;;;;;;;;;;
;; PYTHON ;;
;;;;;;;;;;;;

(defun my-python-get-args-from-line ()
  (re-search-backward "def __init__")
  (let (( args (buffer-substring
                (progn
                  (re-search-forward "(self,")
                  (point))
                (progn
                  (re-search-forward ")")
                  (forward-char -1)
                  (point)))))
    (split-string args ",")))

(defun my-python-newline ()
  (interactive)
  (cond
   ((looking-back "def __init__(.*):")
    (my-python-init-basic-class))
   ((looking-back "class .*:")
    (newline nil t)
    (insert "def __init__(self,):")
    (forward-char -2))
   (t
    (newline nil t))))

(defun my-python-init-basic-class ()
  (interactive)
  (let ((args (my-python-get-args-from-line)))
    (end-of-line)
    (dolist (arg args)
      (newline nil t)
      (insert (format "self.%s=%s" arg arg)))))

(defun my-python-create-package ()
  (interactive)
  (let (( pack_name (read-string "Package name: ") ))
    (make-directory pack_name)
    (write-region "" "" (expand-file-name "__init__.py" pack_name))))

(defun my-python-hook ()
  (interactive)
  (local-set-key (kbd "C-<return>") 'my-python-newline)
  (hs-minor-mode)
  (eglot)
  )

(add-hook 'python-mode-hook 'my-python-hook)

;;;;;;;;;;;;;;;;;;;;;
;; INSERT BRACKETS ;;
;;;;;;;;;;;;;;;;;;;;;

(defun insert-bracket--right ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (save-excursion
          (insert "]")
          (goto-char (region-beginning))
          (insert "["))
        (forward-char 2))
    (insert "[]")))

(defun insert-bracket--left ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (insert "]")
        (goto-char (region-beginning))
        (insert "[")
        (forward-char))
    (insert "[]")))

(global-set-key (kbd "M-]") 'insert-bracket--right)
(global-set-key (kbd "M-[") 'insert-bracket--left)

(defun insert-curly-bracket--right ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (save-excursion
          (insert "}")
          (goto-char (region-beginning))
          (insert "{"))
        (forward-char 2))
    (insert "{}")))

(defun insert-curly-bracket--left ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (insert "}")
        (goto-char (region-beginning))
        (insert "{")
        (forward-char))
    (insert "{}")))

(global-set-key (kbd "M-}") 'insert-curly-bracket--right)
(global-set-key (kbd "M-{") 'insert-curly-bracket--left)

(defun insert-quotes ()
  (interactive)
  (if (region-active-p)
      (progn
        (goto-char (region-end))
        (save-excursion
          (insert "\"")
          (goto-char (region-beginning))
          (insert "\""))
        (forward-char 2))
    (insert "\"\"")))

(global-set-key (kbd "M-\"") 'insert-quotes)

;;;;;;;;;;;;;;;;;;;;;
;; CARD MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;

(when (equal (my-system-name) "EBEBRPCL1226")
  (defun tandemacs-mas-create-card (pan expdate bank env)
    (interactive
     (list
      (read-string "Pan: ")
      (read-string "Expiration Date: ")
      (read-string "Bank: ")
      (read-string "Environement: ")))
    (kill-new (format "(path),(description),002,014,023,035,052,055\nCardProfiles_User.SDA.%1$s %3$s %4$s,%1$s,%1$s,%2$s,002,%1$sD%2$s221000001162309,8080,820239008407A00000000430609F10120315A08003240000000000000000000000FF" pan expdate bank env)))



  (defun copy-pan-for-hlmu ()
    (interactive)
    (let ((word (word-at-point) ))
      (if (string-match "6703\\([0-9]\\)\\{13\\}" word)
          (progn
            (message "PAN saved in kill-ring")
            (kill-new (format "%s%s%s" (substring word 4 14) "  " (substring word 14 16))))
        (message "No PAN at point"))
      ))

  (defun my-card-windows ()
    "Show the cards in a little windows on the left"
    (interactive)
    (split-window-horizontally -40)
    (windmove-right)
    (find-file "~/.cards"))

  (global-set-key (kbd "<f7>") 'copy-pan-for-hlmu)
  (global-set-key (kbd "<f8>") 'my-card-windows)

  )


;; TODO MSICELANOUS
(defun delete-doublon (&optional pos)
  (interactive)
  (unless pos (setq pos (point-min)))
  (goto-line pos)
  (setq centpourcent (line-number-at-pos (point-max)))
  (setq killvar 0)
  (while (not (eobp))
    (let (( line (word-at-point) ))
      (save-excursion
        (when (re-search-forward line nil t)
          (while (re-search-forward line nil t)
            (setq killvar (+ 1 killvar))
            (setq centpourcent (- centpourcent 1))
            (kill-whole-line)))))
    (next-logical-line)
    (let (( linum (line-number-at-pos)))
      (message "Cleanning progress: %3d %% (%d killed) (line %s)"
               (* 100 (/ linum centpourcent 1.0)) killvar linum)) ))



;;;;;;;;;;;
;; MAGIT ;;
;;;;;;;;;;;

;; optional: this is the evil state that evil-magit will use
;; (setq evil-magit-state 'normal)
;; optional: disable additional bindings for yanking text
;; (setq evil-magit-use-y-for-yank nil)
(require 'evil-magit)

(global-set-key (kbd "M-²") 'magit)

;;;;;;;;;;;;;
;; COMPANY ;;
;;;;;;;;;;;;;

(require 'company)

(global-company-mode)

(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 3)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous))


(require 'eglot)

(define-key eglot-mode-map (kbd "M-*") 'xref-find-definitions)
(define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)

;; (add-to-list 'eglot-server-programs '(foo-mode . ("foo-language-server" "--args")))
(add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
(add-to-list 'eglot-server-programs '(nxml-mode . ("java" "-jar" "c:/Users/Samuel D/Documents/Programmes/bin/org.eclipse.lsp4xml-uber.jar")))
(add-to-list 'eglot-server-programs '(css-mode . ("css-languageserver" "--stdio")))
(add-to-list 'eglot-server-programs '(mhtml-mode . ("html-languageserver" "--stdio")))

(setq eglot-events-buffer-size 2000)
(setq eglot-put-doc-in-help-buffer t)
(setq eglot-auto-display-help-buffer nil)

;; disable feature
(add-to-list 'eglot-ignored-server-capabilites :documentHighlightProvider)
(add-to-list 'eglot-ignored-server-capabilites :hoverProvider)

;; (defun eglot--update-doc (string hint)
;;   "Put updated documentation STRING where it belongs.
;; Honours `eglot-put-doc-in-help-buffer'.  HINT is used to
;; potentially rename EGLOT's help buffer."
;;   (if (or (eq t eglot-put-doc-in-help-buffer)
;;           (and eglot-put-doc-in-help-buffer
;;                (funcall eglot-put-doc-in-help-buffer string)))
;;       (with-current-buffer (eglot--help-buffer)
;;         (rename-buffer (format "*eglot-help for %s*" hint))
;;         (let ((inhibit-read-only t))
;;           (erase-buffer)
;;           (insert string)
;;           (goto-char (point-min))
;;           (if eglot-auto-display-help-buffer
;;               (display-buffer (current-buffer))
;;             (unless (get-buffer-window (current-buffer))
;;               (eglot--message
;;                "%s"
;;                (truncate-string-to-width
;;                 (replace-regexp-in-string "\\(.*\\)\n.*" "\\1" string)
;;                 (frame-width) nil nil "...")
;;                (buffer-name eglot--help-buffer))))
;;           (help-mode)))
;;     (eldoc-message string)))

;;;;;;;;;;;;;;;;;;;;;
;; FOLDING/OUTLINE ;;
;;;;;;;;;;;;;;;;;;;;;

(define-key evil-normal-state-map (kbd "z e") 'outline-toggle-subtree)

(defun outline-toggle-subtree ()
  "Show or hide the current subtree depending on its current state."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (line-end-position)))
        (outline-hide-subtree)
      (outline-show-subtree)
      ;; (outline-show-entry)
      )))


;;;;;;;;;;;;;;;;;
;; C-mode hook ;;
;;;;;;;;;;;;;;;;;

(defun my-c-mode-hook ()
  (interactive)
  (outline-minor-mode))

(add-hook 'c-mode-hook 'my-c-mode-hook)

;;;;;;;;;;;;;
;; FLYMAKE ;;
;;;;;;;;;;;;;


(global-set-key (kbd "M-§") 'flymake-goto-next-error)
;; (setq image-file-name-extensions (remove "svg" image-file-name-extensions))

;;;;;;;;;;
;; NXML ;;
;;;;;;;;;;

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

;;;;;;;;
;; GH ;;
;;;;;;;;

(defun my-git-create-new-repo (user reponame)
  (interactive
   (list
    (read-string "Username: " "BrachystochroneSD")
    (read-string "Repo name: ")))
  (shell-command
   (format "curl -u %s:%s https://api.github.com/user/repos -d '{\"name\":\"%s\"}'"
           user (read-passwd "Git Hub Password: ") reponame))
  (format "git@github.com:%s/%s.git" user reponame))

(defun emergency-percent ()
  (interactive)
  (insert "%"))

(define-key evil-normal-state-map (kbd "£") 'evil-jump-item)
(define-key evil-visual-state-map (kbd "£") 'evil-jump-item)
(define-key evil-insert-state-map (kbd "C-M-µ") 'emergency-percent)
