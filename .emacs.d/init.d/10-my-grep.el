(defvar my-grep-history-file-regexp nil)
(defvar my-grep-history-file-regexp-last nil)

(defvar my-grep-history-regexp nil)

(defun my-grep (regexp file-regexp)
    (interactive
     (list
      (read-string "regexp: " "" 'my-grep-history-regexp)
      (read-string
       "file regexp: "
       my-grep-history-file-regexp-last
       (if my-grep-history-file-regexp-last
           '(my-grep-history-file-regexp . 1)
         'my-grep-history-file-regexp))))
    (setq my-grep-history-file-regexp-last file-regexp)
    (grep-find
     (format
      "find \"%s\" -type f -name \"%s\" -exec grep --color=auto -nH --null -e \"%s\" \\{\\} +"
      (or (magit-toplevel)
          default-directory)
      file-regexp regexp)))

(global-set-key (kbd "C-c C-g") 'my-grep)
