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

(fset 'epg-wait-for-status 'ignore)
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
  :bind (("<f9>" . (lambda () (interactive) (my-make-it-transparentier -5)))
         ("<C-f9>" . (lambda () (interactive) (my-make-it-transparentier 5)))
         ("<M-f9>" . (lambda () (interactive) (my-make-it-transparentier -10)))
         ("<C-M-f9>" . (lambda () (interactive) (my-make-it-transparentier 10))))
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

  (set-frame-parameter nil 'alpha-background 70)
  (add-to-list 'default-frame-alist '(alpha-background . 70))

  (defun my-make-it-transparentier (x)
    "add X to the current frame alpha value"
    (set-frame-parameter nil 'alpha-background
                         (max 20 (min 100 (+ x (frame-parameter nil 'alpha-background)))))
    (message (format "Current alpha: %d" (frame-parameter nil 'alpha-background)))))

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
(global-set-key (kbd "M-'") 'insert-quote)

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

(defvar my-next-buffer--first-buffer nil "Shite")

(defun my-next-buffer (&optional previous modified)
  "next-buffer, only skip buffer nuls"
  (interactive)
  (unless my-next-buffer--first-buffer
    (setq my-next-buffer--first-buffer (current-buffer)))
  (if previous (previous-buffer) (next-buffer))
  (if (eq my-next-buffer--first-buffer (current-buffer))
      (progn (message "Looped around, stopping")
             (setq my-next-buffer--first-buffer nil))
    (if (or (when modified (not (buffer-modified-p)))
            (equal "dired-mode" (symbol-name major-mode))
            (equal (substring (buffer-name) 0 1) "*"))
        (my-next-buffer previous modified)
      (setq my-next-buffer--first-buffer nil))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ------------ PACKAGES ------------ ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flyspell
  :init
  (setq ispell-program-name "hunspell")
  ;; :config
  ;; (add-to-list 'ispell-hunspell-dict-paths-alist '("fr_BE" "/usr/share/hunspell/fr_BE.aff"))
  )

;; (setq ispell-hunspell-dict-paths-alist
      ;; '())
;; (setq ispell-dictionary "fr_BE")
;; (setq ispell-hunspell-dictionary-alist
      ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
      ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
      ;; '(("fr_BE" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "fr_BE,en_GB") nil utf-8)))

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
  :bind (("C-c n" . my-git-fzf)
         ("C-c C-n" . my-git-fzf)
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
  :hook (gdscript-mode . eglot-ensure)
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
              ("C-M-j" . evil-join)
              ("C-w RET" . evil-window-vsplit)
              ("C-w c" . evil-window-split)
              ("C-w {" . evil-window-move-far-left)
              ("C-w }" . evil-window-move-far-right)
              ("C-w DEL" . evil-window-delete))
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
  :after (evil magit forge)
  :config
  (evil-collection-init '((magit magit-repos magit-submodule) magit-section magit-todos)))

(use-package magit
  :ensure t
  :bind (("M-²" . magit)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autorefresh Docview ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'DocView-mode-hook 'auto-revert-mode)

(defvar init-dir "~/.emacs.d/init.d/")
(defun load-config-files ()
  (dolist (file (directory-files init-dir t "\\.el"))
    (condition-case err
        (load file)
      (error (message "Error loading %s: %s" file err)))))

(load-config-files)
