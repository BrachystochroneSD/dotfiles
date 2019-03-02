(require 'base16-theme)

;; colours generated dynamically by wal
(defun set-wal-colors () (setq base16-wal-colors
			       '(:base00 "#362121"
					 :base01 "#362121"
                                         :base02 "#6a4a2f"
					 :base03 "#DAD063"
					 :base04 "#A88879"
					 :base05 "#B49597"
					 :base06 "#E4AF99"
					 :base07 "#eadeca"
					 :base08 "#a39b8d"
					 :base09 "#A88879"
					 :base0A "#D4955E"
					 :base0B "#DAD063"
					 :base0C "#E77C9B"
					 :base0D "#B49597"
					 :base0E "#E4AF99"
					 :base0F "#eadeca")))

(defvar base16-wal-colors nil "All colors for base16-wal are defined here.")
(set-wal-colors)

;; Define the theme
(deftheme base16-wal)

;; Add all the faces to the theme
(base16-theme-define 'base16-wal base16-wal-colors)

;; Mark the theme as provided
(provide-theme 'base16-wal)

(provide 'base16-wal)
