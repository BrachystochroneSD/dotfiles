(require 'base16-theme)

;; colours generated dynamically by wal
(defun set-wal-colors () (setq base16-wal-colors
			       '(:base00 "{color0}"
					 :base01 "{color1}"
					 :base02 "{color2}"
					 :base03 "{color3}"
					 :base04 "{color4}"
					 :base05 "{color5}"
					 :base06 "{color6}"
					 :base07 "{color7}"
					 :base08 "{color8}"
					 :base09 "{color9}"
					 :base0A "{color10}"
					 :base0B "{color11}"
					 :base0C "{color12}"
					 :base0D "{color14}"
					 :base0E "{color15}"
					 :base0F "{foreground}")))

(defvar base16-wal-colors nil "All colors for base16-wal are defined here.")
(set-wal-colors)

;; Define the theme
(deftheme base16-wal)

;; Add all the faces to the theme
(base16-theme-define 'base16-wal base16-wal-colors)

;; Mark the theme as provided
(provide-theme 'base16-wal)

(provide 'base16-wal)
