(require 'base16-theme)

;; colours generated dynamically by wal
(defun set-wal-colors () (setq base16-wal-colors
			       '(:base00 "#092A10"
					 :base01 "#092A10"
					 :base02 "#AFA95D"
					 :base03 "#FE8D72"
					 :base04 "#A29F58"
					 :base05 "#F7BF78"
					 :base06 "#EB9F8A"
					 :base07 "#f5ccbb"
					 :base08 "#ab8e82"
					 :base09 "#A29F58"
					 :base0A "#AFA95D"
					 :base0B "#FE8D72"
					 :base0C "#ECB076"
					 :base0D "#F7BF78"
					 :base0E "#EB9F8A"
					 :base0F "#f5ccbb")))

(defvar base16-wal-colors nil "All colors for base16-wal are defined here.")
(set-wal-colors)

;; Define the theme
(deftheme base16-wal)

;; Add all the faces to the theme
(base16-theme-define 'base16-wal base16-wal-colors)

;; Mark the theme as provided
(provide-theme 'base16-wal)

(provide 'base16-wal)
