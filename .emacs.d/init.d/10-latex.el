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

  (add-hook 'LaTeX-mode-hook #'visual-line-mode))
