;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode)
(menu-bar-mode -1)
(setq create-lockfiles nil)
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

(require 'package)
(package-initialize)
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)


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
 '(ansi-term-color-vector
   [unspecified "#10180d" "#94a19a" "#A3C348" "#9DC248" "#B6CDB9" "#B0D2C1" "#B6CDB9" "#B6CDB9"] t)
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("c306e0ef591df6383f666287d0d55767c2c24f3ca137e1c7785c43f08d23c9e8" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" default))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(mml-secure-key-preferences
   '((OpenPGP
      (sign
       ("samueld@mailo.com" "97A79AE8BEAE08D06ABBFE0145EBA6683B3604EA"))
      (encrypt))
     (CMS
      (sign)
      (encrypt))))
 '(package-selected-packages
   '(vue-mode typescript-mode kivy-mode kotlin-mode lua-mode fzf eglot php-mode gh auctex company dired-hide-dotfiles evil-magit evil-mc evil-mu4e pyim))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#282828"))
 '(send-mail-function 'mailclient-send-it))


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

(global-set-key (kbd "<f8>") 'toggle-truncate-lines)
(global-set-key (kbd "<C-f8>") 'visual-line-mode)

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

(set-frame-font "firacode-14:regular")
(add-to-list 'default-frame-alist
             '(font . "firacode-14:regular"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE-INTEGRATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'org
  (add-to-list 'org-structure-template-alist
               '("f" "#+BEGIN_center \n#+ATTR_LATEX: :width 0.45\\linewidth :center\n?\n#+END_center"))

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

  (setq org-latex-toc-command "\\tableofcontents \\newpage")
  (setq org-export-with-sub-superscripts nil)
  (setq org-export-with-special-strings nil)
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
  (local-set-key (kbd "C-<tab>") 'mode-line-other-buffer)
  (local-unset-key (kbd "M-h"))
  (local-set-key (kbd "M-h") 'backward-paragraph))

(add-hook 'org-mode-hook 'my-org-mode-hook)

;; Org-mode Agenda
(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-include-diary t)

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

(defun my-text-scale-adjust (height)
  (set-face-attribute 'default nil :height height))

(defun my-text-scale-decrease (incr)
  (interactive (list 10))
  (let ((currheight (face-attribute 'default :height)))
    (my-text-scale-adjust (- currheight incr))))

(defun my-text-scale-increase (incr)
  (interactive (list 10))
  (let ((currheight (face-attribute 'default :height)))
    (my-text-scale-adjust (+ currheight incr))))

(global-set-key (kbd "C-+") 'my-text-scale-increase)
(global-set-key (kbd "C--") 'my-text-scale-decrease)

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
    (cond ((string-match "\\(pdf\\|djvu\\|ps\\|dvi\\)$" file-extension)
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
;; MY BOOKMARKS ;;
;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f5>") (lambda () (interactive) (find-file "~/Documents/Administrative/a_faire.org")))

(defvar my-bookmarks-alist ;; General bm
  `(("packages" . "~/.emacs.d/packages/")
    ("emacs" . "~/.emacs.d/init.el")
    ("home" . "~/")
    ("zshrc" . "~/.zshrc")
    ("scripts" . "~/.script/")
    ("doc" . "~/Documents/")
    ("downloads" . "~/Downloads/")
    ("images" . "~/Images/")
    ))

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

(defun my-elfeed-search-hook-setup ()
  (interactive)
  (init-fonts)
  (local-set-key (kbd "j") 'next-line)
  (local-set-key (kbd "k") 'previous-line)
  (local-set-key (kbd "l") 'my-elfeed-open)
  (local-set-key (kbd "+") 'my-elfeed-add-week)
  (local-set-key (kbd "-") 'my-elfeed-sub-week)
  (local-set-key (kbd "t") 'my-elfeed-toggle-unread)
  (local-set-key (kbd "h") 'delete-other-windows)
  (local-set-key (kbd "/") 'isearch-forward)
  (local-set-key (kbd "?") 'isearch-backward)
  (local-set-key (kbd "q") 'delete-frame)
  (local-set-key (kbd "C-b") (lambda () (interactive) (elfeed-play-with-mpv-at-quality 720 (elfeed-search-selected :single))))
  (local-set-key (kbd "C-n") 'my-elfeed-scroll-up)
  (local-set-key (kbd "C-p") 'my-elfeed-scroll-down)
  )

(add-hook 'elfeed-search-mode-hook 'my-elfeed-search-hook-setup)

(defun elfeed-play-with-mpv-at-quality (quality-val entry)
  (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
  (if (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val))
    (setq quality-arg ""))
  (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry)))

(defun elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "360" "480" "720" "1080") nil nil "720")))
    (elfeed-play-with-mpv-at-quality (string-to-number quality-val) entry)))

(defvar elfeed-video-patterns
  '("youtu\\.?be" "facebo\\.?ok")
  "List of regexp to match against elfeed entry link to know
whether to use mpv to visit the link.")

(defun elfeed-visit-or-play-video ()
  "Play with mpv if entry link matches `elfeed-video-patterns', visit otherwise.
See `elfeed-play-with-mpv'."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (patterns elfeed-video-patterns))
    (while (and patterns (not (string-match (car patterns) (elfeed-entry-link entry))))
      (setq patterns (cdr patterns)))
    (if patterns
        (elfeed-play-with-mpv)
      (if (eq major-mode 'elfeed-search-mode)
          (elfeed-search-browse-url)
        (elfeed-show-visit)))
    (elfeed-search-untag-all-unread)))

(define-key elfeed-search-mode-map "b" 'elfeed-visit-or-play-video)
(define-key elfeed-search-mode-map "B" 'elfeed-search-browse-url)

(define-key elfeed-search-mode-map "d" 'my-elfeed-read-regex)
(define-key elfeed-search-mode-map "D" 'my-elfeed-mark-all-author-as-read)

(defun my-vlc-launc-and-quit (link)
  "Launch a link with vlc and then quit after the end of the video"
  (start-process "my-vlc-quit" nil "vlc" link "vlc://quit"))

(defun my-elfeed-read-regex (regex)
  (interactive
   (list
    (read-string "Mark as read: "
                 (let ((feed (elfeed-entry-feed (car (elfeed-search-selected)))))
                   (when feed
                     (or (elfeed-meta feed :title) (elfeed-feed-title feed)))
                   )
)))
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

;;;;;;;;;;;;
;; AuCTeX ;;
;;;;;;;;;;;;

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

(defun my-latex-replace-shit (shit replacement)
  (delete-backward-char (length shit))
  (insert replacement)
  )

(defun my-latex-smart-tab ()
  (interactive)
  (cond

   ((looking-back "<s")
    (delete-backward-char 2)
    (indent-for-tab-command)
    (LaTeX-section 2)
    )

   ((looking-back "<f")
    (delete-backward-char 2)
    (indent-for-tab-command)
    (LaTeX-insert-environment "frame")
    (insert "\\frametitle{}")
    (backward-char 1))

   ((looking-back "<e")
    (delete-backward-char 2)
    (indent-for-tab-command)
    (LaTeX-insert-environment "enumerate")
    (insert "\\item ")
    )

   ((looking-back "<i")
    (delete-backward-char 2)
    (indent-for-tab-command)
    (LaTeX-insert-environment "itemize")
    (insert "\\item ")
    )

   ((looking-back "<c")
    (delete-backward-char 2)
    (indent-for-tab-command)
    (LaTeX-insert-environment "center"))

   ((looking-back "->")
    (my-latex-replace-shit "->" "$\\rightarrow$"))
   ((looking-back "<-")
    (my-latex-replace-shit "<-" "$\\lefttarrow$"))
   ((looking-back "=>")
    (my-latex-replace-shit "=>" "$\\Rightarrow$"))
   ((looking-back "<=")
    (my-latex-replace-shit "<=" "$\\Lefttarrow$"))
   ((looking-back "[.][.][.]")
    (my-latex-replace-shit "..." "$\\ldots$"))
   (t
    (indent-for-tab-command)))
  )


(defun my-latex-smart-return ()
  (interactive)
  (cond
   ((looking-back "\\item ")
    (kill-whole-line)
    (end-of-line)
    (newline))
   ((and (looking-back "\\frametitle{.*")
         (looking-at "}"))
    (end-of-line)
    (newline))
   (t
    (LaTeX-insert-item)))
  )

(defun my-TeX-LaTeX-mode-hook-setup ()
  (interactive)
  (add-to-list 'TeX-view-program-selection '(output-pdf "mupdf") )
  (add-to-list 'TeX-view-program-list '("mupdf" "mupdf %o"))
  (local-set-key (kbd "<C-return>") 'my-latex-smart-return)
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

  (local-set-key (kbd "<tab>") 'my-latex-smart-tab)
  (evil-define-key 'insert 'local (kbd "<tab>") 'my-latex-smart-tab)

  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  )


(add-hook 'TeX-mode-hook 'my-TeX-LaTeX-mode-hook-setup)
(add-hook 'LaTeX-mode-hook 'my-TeX-LaTeX-mode-hook-setup)

;;visual-line-mode pour le latex

(add-hook 'LaTeX-mode-hook #'visual-line-mode)

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

;;;;;;;;;;
;; MU4E ;;
;;;;;;;;;;

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'shr)
(require 'smtpmail)

;; Global invoke key
(global-set-key (kbd "S-<f5>") 'mu4e)

;; html mail

(defun shr-render-current-buffer ()
  (shr-render-region (point-min) (point-max)))

(add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser))
(setq mu4e-html2text-command 'shr-render-current-buffer)


(setq
 ;; General
 message-kill-buffer-on-exit t
 mu4e-context-policy 'pick-first
 mu4e-confirm-quit nil
 mu4e-compose-dont-reply-to-self t
 mu4e-save-multiple-attachments-without-asking t
 mu4e-headers-skip-duplicates t

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
 shr-color-visible-luminance-min 80
 )

(setq mu4e-refile-folder
      ;; Taken from : https://www.djcbsoftware.nl/code/mu/mu4e/Smart-refiling.html
      (lambda (msg)
        (cond
         ((mu4e-message-contact-field-matches msg '(:to :cc) "samueld@mailo.com")
	  "/archives/mailo")
         ((mu4e-message-contact-field-matches msg '(:to :cc) "samrenfou@hotmail.com")
	  "/archives/outlook")
         ((mu4e-message-contact-field-matches msg '(:to :cc) "samuel.dawant@alumni.umons.ac.be")
	  "/archives/umons")
         ((mu4e-message-contact-field-matches msg '(:to :cc) "mrsamrenfou@gmail.com")
	  "/archives/gmail")
         ;; messages sent by me go to the sent folder
         ((find-if
	   (lambda (addr)
	     (mu4e-message-contact-field-matches msg :from addr))
	   (mu4e-personal-addresses))
	  mu4e-sent-folder)
         (t  "/archives/misc"))))

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
             (make-mu4e-bookmark
              :name "all"
              :query "maildir:/umons/* OR maildir:/mailo/* OR maildir:/gmail/*"
              :key ?a))

(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
              :name "sents"
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
  (local-set-key (kbd "M-D") 'my-mu4e-delete-from)
  (local-set-key (kbd "M-r") 'my-mu4e-refile-subject)
  (local-set-key (kbd "M-R") 'my-mu4e-refile-from)
  (local-set-key (kbd "i") 'my-mu4e-important-refile)
  (local-set-key (kbd "C-D") 'my-mu4e-delete-subject)
  (local-set-key (kbd "C-j") 'my-mu4e-delete-junk))

(defun my-mu4e-main-hook ()
  (interactive)
  (local-set-key (kbd "M-C") 'brachystochronesd/compose-encrypted))

(add-hook 'mu4e-view-mode-hook 'my-mu4e-view-hook)
(add-hook 'mu4e-headers-mode-hook 'my-mu4e-header-hook)
(add-hook 'mu4e-compose-mode-hook 'my-mu4e-compose-hook)
(add-hook 'mu4e-main-mode-hook 'my-mu4e-main-hook)

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
   ((looking-back "^To:.*")
    (re-search-forward "Subject: *"))
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
    ))

(add-hook 'mu4e-compose-mode-hook 'ambrevar/mu4e-compose-maybe-signed-and-crypted)


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
  (hs-minor-mode))

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

;;;;;;;;;;;
;; EGLOT ;;
;;;;;;;;;;;

(require 'eglot)

(define-key eglot-mode-map (kbd "M-*") 'xref-find-definitions)
(define-key eglot-mode-map (kbd "C-c h") 'eglot-help-at-point)

;; ;; (add-to-list 'eglot-server-programs '(foo-mode . ("foo-language-server" "--args")))
;; (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
(add-to-list 'eglot-server-programs '(lua-mode . ("lua-language-server")))
(add-to-list 'eglot-server-programs '(kotlin-mode . ("kotlin-language-server")))
;; (add-to-list 'eglot-server-programs '(mhtml-mode . ("html-languageserver" "--stdio")))

(setq eglot-events-buffer-size 2000)
(setq eglot-put-doc-in-help-buffer t)
(setq eglot-auto-display-help-buffer nil)

;; disable feature
(add-to-list 'eglot-ignored-server-capabilites :documentHighlightProvider)
(add-to-list 'eglot-ignored-server-capabilites :hoverProvider)

(defun eglot--update-doc (string hint)
  "Put updated documentation STRING where it belongs.
Honours `eglot-put-doc-in-help-buffer'.  HINT is used to
potentially rename EGLOT's help buffer."
  (if (or (eq t eglot-put-doc-in-help-buffer)
          (and eglot-put-doc-in-help-buffer
               (funcall eglot-put-doc-in-help-buffer string)))
      (with-current-buffer (eglot--help-buffer)
        (rename-buffer (format "*eglot-help for %s*" hint))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert string)
          (goto-char (point-min))
          (if eglot-auto-display-help-buffer
              (display-buffer (current-buffer))
            (unless (get-buffer-window (current-buffer))
              (eglot--message
               "%s"
               (truncate-string-to-width
                (replace-regexp-in-string "\\(.*\\)\n.*" "\\1" string)
                (frame-width) nil nil "...")
               (buffer-name eglot--help-buffer))))
          (help-mode)))
    (eldoc-message string)))

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

(defun my-git-init-and-remote (user reponame)
  (interactive
   (list
    (read-string "Username: " "BrachystochroneSD")
    (read-string "Repo name: ")))
  (magit-init default-directory)
  (magit-remote-add "origin" (my-git-create-new-repo user reponame)))

(defun emergency-percent ()
  (interactive)
  (insert "%"))

(define-key evil-normal-state-map (kbd "£") 'evil-jump-item)
(define-key evil-visual-state-map (kbd "£") 'evil-jump-item)
(define-key evil-insert-state-map (kbd "C-M-µ") 'emergency-percent)

;;;;;;;;;;;;;;
;; LUA-MODE ;;
;;;;;;;;;;;;;;

(setq lua-indent-level 4)

;;;;;;;;;;;;;;;
;; HTML-MODE ;;
;;;;;;;;;;;;;;;

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
  (local-set-key (kbd "C-c C-p") 'sgml-skip-toggle-tag)
  (local-set-key (kbd "C-c C-t") 'my-sgml-tag)
  (local-set-key (kbd "C-c C-d") 'my-sgml-delete-tag))

(add-hook 'html-mode-hook 'my-html-mode-hook)

;;;;;;;;;;;
;; EMOJI ;;
;;;;;;;;;;;

(defun init-fonts ()
  (set-fontset-font t '(#x1f000 . #x1faff)
                    (font-spec :family "JoyPixels")))

;;;;;;;;;;;
;; SUBED ;;
;;;;;;;;;;;

;; (add-to-list 'load-path "~/.emacs.d/packages/subed/")
;; (require 'subed)

;; (defun my-subed-hook ()
;;   (interactive)
;;   (setq-local fill-column 40)
;;   (save-place-local-mode)
;;   (turn-on-auto-fill)
;;   (subed-disable-sync-point-to-player)
;;   )

;; (add-hook 'subed-mode-hook 'my-subed-hook)

;;;;;;;;;;;;;;
;; FLYSPELL ;;
;;;;;;;;;;;;;;

;; (setq ispell-program-name "hunspell")
;; (setq ispell-dictionary "francais")

(setq ispell-program-name "hunspell")
(setq ispell-hunspell-dict-paths-alist
      '(("fr_BE" "/usr/share/hunspell/fr_BE.aff")))
(setq ispell-dictionary "fr_BE")
(setq ispell-hunspell-dictionary-alist
      ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
      ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
      '(("fr_BE" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "fr_BE,en_GB") nil utf-8)))

(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
