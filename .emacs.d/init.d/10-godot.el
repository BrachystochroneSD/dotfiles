(use-package gdshader-mode
  :straight (gdshader-mode :type git :host github :repo "bbbscarter/gdshader-mode")

  ;; Optional customisations for company-mode completion.
  :init
  (defun gdshader-config()
    (interactive)
    (setq-local company-dabbrev-downcase nil)
    (setq-local tab-width 4)
    (setq-local company-backends
                '((company-keywords company-dabbrev))))

  :hook ((gdshader-mode . gdshader-config)
         (gdshader-mode . auto-revert-mode))
  :config
  (add-to-list 'company-keywords-alist (append '(gdshader-mode) gdshader-all-keywords)))


;; (use-package gdscript-mode
;;   :after (codegeex magit)
;;   :hook
;;   (gdscript-mode . eglot))


;; (use-package gdshader-mode
;;   ;; Optional customisations for company-mode completion.
;;   :load-path "~/.emacs.d/my-packages/gdshader-mode"
;;   :config
;;   (defun gdshader-func-hook ()
;;     ;; Use tab instead of space for this mode:
;;     (setq-local indent-tabs-mode 'only)
;;     (setq-local tab-width 4)
;;     (setq-local c-basic-offset 4))

;;   :hook (gdshader-mode . gdshader-func-hook)

;;   )
  ;; :init
  ;; (defun gdshader-completion-at-point ()
  ;;   "This is the function to be used for the hook `completion-at-point-functions'."
  ;;   (interactive)
  ;;   (let* ((bounds (bounds-of-thing-at-point 'symbol))
  ;;          (start (car bounds))
  ;;          (end (cdr bounds)))
  ;;     (list start end gdshader-all-keywords
  ;;           . nil)))
  ;; :hook (completion-at-point-functions . gdshader-completion-at-point))
