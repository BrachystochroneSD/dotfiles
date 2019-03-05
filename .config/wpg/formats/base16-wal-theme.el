(require 'base16-theme)

;; colours generated dynamically by wal
(defun set-wal-colors () (setq base16-wal-colors
			       '(:base00 "#2c2219"
					 :base01 "#2c2219"
					 :base02 "#69B791"
					 :base03 "#99A28A"
					 :base04 "#929968"
					 :base05 "#B6C186"
					 :base06 "#C4DFAA"
					 :base07 "#dee9cc"
					 :base08 "#9ba38e"
					 :base09 "#929968"
					 :base0A "#69B791"
					 :base0B "#99A28A"
					 :base0C "#B9BB85"
					 :base0D "#B6C186"
					 :base0E "#C4DFAA"
					 :base0F "#dee9cc")))

(defvar base16-wal-colors nil "All colors for base16-wal are defined here.")
(set-wal-colors)

;; Define the theme
(deftheme base16-wal)

;; Add all the faces to the theme
(base16-theme-define 'base16-wal base16-wal-colors)

;; Mark the theme as provided
(provide-theme 'base16-wal)

(provide 'base16-wal)
