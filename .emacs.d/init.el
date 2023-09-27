(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path "~/.emacs.d/my-packages")
(package-initialize)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

(setq inhibit-startup-screen t)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq display-line-numbers-type t)
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

(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'before-save-hook 'untabify)

(defun my-kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

;; time
(setq display-time-format "%Y/%m/%d %H:%M")
(display-time-mode 1)

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-dark-medium)
  (setq default-frame-alist '((vertical-scroll-bars . nil)
                              (horizontal-scroll-bars . nil)
                              (fullscreen . maximized)))




  (set-face-attribute 'default nil :family "firacode" :height 110)
  (defun set-other-face ()
    (message "TODO")
    ;;(let ((h (face-attribute 'default :height)))
    ;;  (set-face-attribute 'font-lock-string-face nil
    ;;                      :family "cascadiacodepl" :weight 'semibold :slant 'italic :height (* 1.1 h))
    ;;  (set-face-attribute 'font-lock-type-face nil
    ;;                      :family "VictorMono NFM" :weight 'demibold :slant 'oblique :height (* 1.1 h))
    ;;  (set-face-attribute 'font-lock-function-name-face nil
    ;;                      :family "VictorMono NFM" :weight 'medium :height h)
    ;;  (set-face-attribute 'font-lock-builtin-face nil
    ;;                      :family "VictorMono NFM" :weight 'demibold :slant 'italic :height h)
    ;;  (set-face-attribute 'font-lock-keyword-face nil
    ;;                      :family "VictorMono NFM" :weight 'demibold :slant 'italic :height h))
    )
  ;; (set-other-face)

  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))

  (set-frame-parameter nil 'alpha '(90))
  (add-to-list 'default-frame-alist '(alpha 90))

  (defun my-make-it-transparentier (x)
    "add X to the current frame alpha value"
    (set-frame-parameter nil 'alpha (list (max 20 (min 100 (+ x (car (frame-parameter nil 'alpha)))))))
    (message (format "Current alpha: %d" (car (frame-parameter nil 'alpha)))))

  :bind (("<f9>" . (lambda () (interactive) (my-make-it-transparentier -5)))
         ("<C-f9>" . (lambda () (interactive) (my-make-it-transparentier 5)))
         ("<M-f9>" . (lambda () (interactive) (my-make-it-transparentier -10)))
         ("<C-M-f9>" . (lambda () (interactive) (my-make-it-transparentier 10)))))

;;;;;;;;;;;;;;;;;;;;;
;; Custom Bindings ;;
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
(global-set-key (kbd "C-M-?") (lambda () (interactive) (enlarge-window-horizontally 5)))
(global-set-key (kbd "C-M-S-b") (lambda () (interactive) (shrink-window-horizontally 5)))
(global-set-key (kbd "C-M-S-h") (lambda () (interactive) (enlarge-window 5)))
(global-set-key (kbd "C-M-S-n") (lambda () (interactive) (shrink-window 5)))
(global-set-key (kbd "<f8>") 'toggle-truncate-lines)
(global-set-key (kbd "<C-f8>") 'visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;
;; INSERT BRACKETS ;;
;;;;;;;;;;;;;;;;;;;;;

(defun insert-or-encircle (char &optional insert)
  (let ((start-char (if (eq (length char) 2) (substring char 0 1) char))
        (end-char (if (eq (length char) 2) (substring char 1) char)))
    (if (region-active-p)
        (let ((start (region-beginning))
              (end (region-end)))
          (save-excursion
            (goto-char start)
            (insert start-char)
            (goto-char end)
            (forward-char)
            (insert end-char)))
      (insert (or insert char)))))

(defun insert-bracket--right () (interactive) (insert-or-encircle "[]" "]"))
(defun insert-bracket--left () (interactive) (insert-or-encircle "[]" "["))
(defun insert-curly-bracket--right () (interactive) (insert-or-encircle "{}" "}"))
(defun insert-curly-bracket--left () (interactive) (insert-or-encircle "{}" "{"))
(defun insert-quotes () (interactive) (insert-or-encircle "\""))
(defun insert-quote () (interactive) (insert-or-encircle "'"))

(global-set-key (kbd "M-}") 'insert-curly-bracket--right)
(global-set-key (kbd "M-{") 'insert-curly-bracket--left)
(global-set-key (kbd "M-]") 'insert-bracket--right)
(global-set-key (kbd "M-[") 'insert-bracket--left)
(global-set-key (kbd "M-\"") 'insert-quotes)
(global-set-key (kbd "M-'") 'insert-quotes)

;;;;;;;;;;;;;;;;;
;; Window Move ;;
;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-M-y") 'other-frame)
(global-set-key (kbd "C-M-<backspace>") 'delete-window)
(global-set-key (kbd "C-M-S-<backspace>") 'kill-buffer-and-window)

(defun my-split-window-vertically ()
  (interactive)
  (setq my-zoom-state nil)
  (split-window-vertically)
  (set-window-buffer (next-window) (other-buffer)))

(defun my-split-window-horizontally ()
  (interactive)
  (setq my-zoom-state nil)
  (split-window-horizontally)
  (set-window-buffer (next-window) (other-buffer)))

(global-set-key (kbd "C-M-<return>") 'my-split-window-horizontally)
(global-set-key (kbd "C-M-S-<return>") 'my-split-window-vertically)

(setq my-zoom-state nil)

(defun my-zoom-window ()
  (window-configuration-to-register 1)
  (delete-other-windows))

(defun my-dezoom-window ()
  (let ((curr-pos (point)))
    (jump-to-register 1)
    (goto-char curr-pos)))

(defun my-toggle-zoom-window ()
  (interactive)
  (message "%s" my-zoom-state)
  (if (not my-zoom-state)
      (my-zoom-window)
    (my-dezoom-window))
  (setq my-zoom-state (not my-zoom-state)))

;;;;;;;;;;;;;;
;; Comments ;;
;;;;;;;;;;;;;;

(global-set-key (kbd "C-c ;") 'comment-line)
(global-set-key (kbd "C-c b") 'comment-box)

;;;;;;;;;;;;;;;
;; Pair mode ;;
;;;;;;;;;;;;;;;

(electric-pair-mode 1)
(setq electric-pair-pairs '((?\" . ?\") (?\{ . ?\})))

;;;;;;;;;;;;;;;;;;;
;; Change-Buffer ;;
;;;;;;;;;;;;;;;;;;;

(defun my-next-buffer (&optional previous modified)
  "next-buffer, only skip buffer nuls"
  (interactive)
  (if previous (previous-buffer) (next-buffer))
  (if (or (when modified (not (buffer-modified-p)))
          (equal "dired-mode" (symbol-name major-mode))
          (equal (substring (buffer-name) 0 1) "*"))
      (my-next-buffer previous modified)))

(defun my-previous-buffer () (interactive) (my-next-buffer t))
(defun my-next-modified-buffer () (interactive) (my-next-buffer nil t))
(defun my-previous-modified-buffer () (interactive) (my-next-buffer t t))


(global-set-key (kbd "C-$") 'my-next-buffer)
(global-set-key (kbd "M-$") 'my-previous-buffer)
(global-set-key (kbd "C-*") 'my-next-modified-buffer)
(global-set-key (kbd "M-*") 'my-previous-modified-buffer)

;;;;;;;;;;;;;;;;;;;;;;;
;; Text Scale Adjust ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun my-text-scale-adjust (height)
  (set-face-attribute 'default nil :height height)
  (set-other-faces))

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

;;;;;;;;;;;;;;;;;;;;;;;
;; Number Minor Mode ;;
;;;;;;;;;;;;;;;;;;;;;;;

(define-minor-mode number-mode
  "To write number with ,;:jkluion"
  :init-value nil
  :lighter " Number"
  :keymap `((,(kbd ",") . "1")
            (,(kbd ";") . "2")
            (,(kbd ":") . "3")
            (,(kbd "j") . "4")
            (,(kbd "k") . "5")
            (,(kbd "l") . "6")
            (,(kbd "u") . "7")
            (,(kbd "i") . "8")
            (,(kbd "o") . "9")
            (,(kbd "n") . "0")))

(global-set-key (kbd "C-S-n") 'number-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; Goto-Line Binding ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun my-goto-line ()
  (interactive)
  (let ((number-mode t)) (call-interactively 'goto-line)))

(global-set-key (kbd "C-M-g") 'my-goto-line)

;;;;;;;;;;;;;;;;;;
;; MY BOOKMARKS ;;
;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f5>") (lambda () (interactive) (find-file "~/Documents/Administrative/a_faire.org")))

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
  (my-dired-find-file-internal (cdr (assoc alias my-bookmarks-alist)) t))

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

;;;;;;;;;;;;;;;;;;;;;;;;
;; SCRATCH MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(setq initial-scratch-message "")

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

;;;;;;;;;;
;; MISC ;;
;;;;;;;;;;

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

;;;;;;;;;;;;
;; DRAGON ;;
;;;;;;;;;;;;

(defun my-dragon-launch ()
  (interactive)
  (shell-command (format "dragon-drag-and-drop %s" (buffer-file-name))))

(global-set-key (kbd "M-g M-g") 'my-dragon-launch)

;;;;;;;;;;;
;; EMOJI ;;
;;;;;;;;;;;

(defun init-fonts ()
  (set-fontset-font t '(#x1f000 . #x1faff)
                    (font-spec :family "JoyPixels")))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------------ PACKAGES ------------ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pkgbuild-mode
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("M-l" . mc/mark-previous-like-this)
         ("M-m" . mc/mark-next-like-this)
         ("M-o" . mc/mark-all-like-this)))

(use-package flymake
  :bind (("C-µ" . flymake-goto-next-error)
         ("C-ù" . flymake-goto-prev-error)))

(use-package undo-tree
  :bind (("C-z" . undo-tree-undo)
         ("C-M-z" . undo-tree-redo))
  :config
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo_tree_backups/")))
  (global-undo-tree-mode 1))

(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :init
  (setq ivy-count-format "%-4d ")
  :bind (("C-M-<tab>" . completion-at-point))
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :after (ivy magit)
  :hook (after-init . counsel-mode)
  :bind (("C-c C-n" . my-git-fzf)
         ("C-c C-g" . my-grep-fzf))
  :config
  (defun my-git-fzf () (interactive) (counsel-fzf nil (magit-toplevel)))
  (defun my-grep-fzf (regexp)
    (interactive
     (list (read-string
            (format "rgrep on %s: "
                    (or (magit-toplevel)
                        (file-name-directory (buffer-file-name)))))))
    (grep-compute-defaults)
    (rgrep regexp "*" (magit-toplevel))))

(use-package eglot
  :ensure t
  :bind (:map eglot-mode-map
              ("M-*" . xref-find-definitions)
              ("C-c h" . eglot-help-at-point))
  :config
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
      (eldoc-message string))))

(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :bind (:map evil-window-map ("z" . my-toggle-zoom-window)
              :map evil-insert-state-map ("M-SPC" . evil-normal-state)
              :map evil-insert-state-map
              ("C-d" . delete-char)
              ("<tab>" . indent-for-tab-command)
              :map evil-visual-state-map
              ("M-SPC" . evil-exit-visual-state)
              ("<tab>" . indent-for-tab-command)
              ("J" . next-line)
              ("K" . previous-line)
              :map evil-replace-state-map ("M-SPC" . evil-normal-state)
              :map evil-emacs-state-map ("M-SPC" . evil-normal-state)
              :map evil-normal-state-map
              ("z e" . outline-toggle-subtree)
              ("M-SPC" . evil-emacs-state)
              ("q" . evil-goto-mark)
              ("J" . next-line)
              ("K" . previous-line)
              ("C-M-j" . evil-join))
  :hook ((org-metareturn . evil-insert-state)
         (org-insert-heading . evil-insert-state))
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  (evil-set-initial-state 'messages-buffer-mode 'emacs)

  (evil-define-key 'insert org-mode-map (kbd "<tab>") #'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (evil-define-key '(insert normal replace visual emacs) 'global (kbd "C-e") #'move-end-of-line)
  (evil-define-key '(insert normal replace visual emacs) 'global (kbd "C-z") #'undo-tree-undo)

  (evil-ex-define-cmd "g" 'my-goto-line)

  (defun outline-toggle-subtree ()
    "Show or hide the current subtree depending on its current state."
    (interactive)
    (save-excursion
      (outline-back-to-heading)
      (if (not (outline-invisible-p (line-end-position)))
          (outline-hide-subtree)
        (outline-show-subtree)
        ;; (outline-show-entry)
        ))))

(use-package evil-collection
  :ensure t
  :after (evil magit)
  :config
  (evil-collection-init '((magit magit-repos magit-submodule) magit-section magit-todos)))

(use-package magit
  :ensure t
  :bind (("M-²" . magit))
  :config (require 'magit))

(use-package forge
  :ensure t
  :after magit)

(use-package my-mood-line
  :after evil
  :init
  (setq evil-mood-line-colors
        '((emacs . (:background "#b16286" :foreground "#35212b"))
          (insert . (:background "#d65d0e" :foreground "#3f2100"))
          (replace . (:background "#d79921" :foreground "#3f2b00"))
          (motion . (:background "#98971a" :foreground "#2b2b00"))
          (visual . (:background "#458588" :foreground "#212b2b"))
          (operator . (:background "#689d6a" :foreground "#212b21"))
          (normal . (:background "#cc241d" :foreground "#351717"))))
  (setq my-mood-line-show-eol-style t)
  (setq my-mood-line-show-encoding-information t)
  :load-path "~/.emacs.d/my-packages/my-mood-line"
  :hook ((evil-visual-state-entry . my-evil-color-modeline)
         (evil-motion-state-entry . my-evil-color-modeline)
         (evil-replace-state-entry . my-evil-color-modeline)
         (evil-insert-state-entry . my-evil-color-modeline)
         (evil-emacs-state-entry . my-evil-color-modeline)
         (evil-normal-state-entry . my-evil-color-modeline)
         (evil-operator-state-entry . my-evil-color-modeline)
         (after-init . my-mood-line-mode))
  :config
  (my-mood-line-mode 1)
  (defun my-evil-color-modeline ()
    (interactive)
    (face-remap-set-base 'my-mood-line-modified `(,(alist-get evil-state evil-mood-line-colors) my-mood-line-modified))
    (face-remap-set-base 'my-mood-line-unimportant `(,(alist-get evil-state evil-mood-line-colors) my-mood-line-unimportant))
    (face-remap-set-base 'my-mood-line-anzu `(,(alist-get evil-state evil-mood-line-colors) my-mood-line-anzu))
    (face-remap-set-base 'my-mood-line-buffer-name `(,(alist-get evil-state evil-mood-line-colors) my-mood-line-buffer-name))))

(use-package org
  :ensure t
  :config
  (define-auto-insert "\.org" "org_template.org")
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-to-list 'org-structure-template-alist
               '("f" "#+BEGIN_center \n#+ATTR_LATEX: :width 0.45\\linewidth :center\n?\n#+END_center"))

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
    ;; (local-set-key (kbd "<C-return>") 'ealm-org-heading-return)
    (local-set-key (kbd "C-*") 'my-org-insert-bold-star)
    (local-set-key (kbd "C-c <C-return>") 'org-open-at-point)
    (local-set-key (kbd "C-,") 'forward-char)
    (local-set-key (kbd "C-<tab>") 'mode-line-other-buffer)
    (setq org-startup-folded t)
    (local-unset-key (kbd "M-h"))
    (local-set-key (kbd "M-h") 'backward-paragraph))

  (add-hook 'org-mode-hook 'my-org-mode-hook)

  ;; Org-mode Agenda
  (global-set-key "\C-ca" 'org-agenda)
  (setq org-agenda-include-diary t))

(use-package ox-latex
  :after org
  :config
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
 linkcolor=blue}"))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autorefresh Docview ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'DocView-mode-hook 'auto-revert-mode)

;;;;;;;;;;;
;; DIRED ;;
;;;;;;;;;;;

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
      (start-process "elfeed-mpd" nil (expand-file-name "~/.script/podcast") (elfeed-entry-link entry))))

  (defun elfeed-play-with-mpv ()
    "Play entry link with mpv."
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
          (quality-val (completing-read "Max height resolution (0 for unlimited): " '("1080" "720" "360" "480"))))
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

(use-package tex
  :after evil
  :init
  (setq TeX-save-query nil)
  (setq TeX-clean-confirm nil)
  :config
  (define-auto-insert "\.tex" "tex_template.tex")
  (add-to-list 'auto-mode-alist '("\\.tex\\'" . TeX-mode))
  (add-to-list 'TeX-command-list
               '("Compile&Clean" "pdflatex %s && pdflatex %s && erase *.aux *.toc *.log" TeX-run-command nil t :help "compile 2 fois et clean") t)

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
    (insert replacement))

  (defun my-latex-smart-tab ()
    (interactive)
    (cond
     ((looking-back "<s")
      (delete-backward-char 2)
      (indent-for-tab-command)
      (LaTeX-section 2))

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
      (insert "\\item "))

     ((looking-back "<i")
      (delete-backward-char 2)
      (indent-for-tab-command)
      (LaTeX-insert-environment "itemize")
      (insert "\\item "))

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
      (indent-for-tab-command))))


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
      (LaTeX-insert-item))))

  (add-to-list 'TeX-view-program-selection '(output-pdf "mupdf") )
  (add-to-list 'TeX-view-program-list '("mupdf" "mupdf %o"))
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (outline-minor-mode)
  (evil-define-key 'insert 'local (kbd "<tab>") 'my-latex-smart-tab)
  (defun my-TeX-LaTeX-mode-hook-setup ()
    (interactive)
    (local-set-key (kbd "<C-return>") 'my-latex-smart-return)
    (local-set-key (kbd "<M-return>") 'LaTeX-close-environment)
    (local-set-key (kbd "M-v") 'outline-previous-visible-heading)
    (local-set-key (kbd "C-v") 'outline-next-visible-heading)
    (local-set-key (kbd "C-c M-l") 'TeX-next-error)
    (local-set-key (kbd "C-c i b") 'my-latex-insert-bold)
    (local-set-key (kbd "C-c i s") 'my-latex-insert-sc)
    (local-set-key (kbd "C-c i i") 'my-latex-insert-italics)
    (local-set-key (kbd "C-c i u") 'my-latex-insert-underline)
    (local-set-key (kbd "C-c i e") 'my-latex-insert-emph)
    (local-set-key (kbd "<tab>") 'my-latex-smart-tab))

  (add-hook 'TeX-mode-hook 'my-TeX-LaTeX-mode-hook-setup)
  (add-hook 'LaTeX-mode-hook 'my-TeX-LaTeX-mode-hook-setup)

  ;;visual-line-mode pour le latex

  (add-hook 'LaTeX-mode-hook #'visual-line-mode)
  )


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

(use-package c-mode :hook (c-mode . outline-minor-mode))

(use-package lua-mode :config (setq lua-indent-level 4))

(use-package python-mode :hook (python-mode . eglot))

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

(use-package uuidgen
  :ensure t)

(use-package codegeex
  :after uuidgen
  :load-path "~/.emacs.d/my-packages/codegeex.el"
  :custom (codegeex-idle-delay 1)
  :bind (:map codegeex-mode-map
              ("C-:" . codegeex-accept-completion)
              ("C-=" . codegeex-next-completion)
              ("C-;" . codegeex-previous-completion))
  :config
  (require 'codegeex))

(use-package gdscript-mode
  :hook
  (gdscript-mode . (codegeex-mode 1))
  (gdscript-mode . eglot))
