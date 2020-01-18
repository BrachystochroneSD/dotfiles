(require 'base16-theme)

;; colours generated dynamically by wal
(defun set-wal-colors () (setq base16-wal-colors
			       '(:base00 "#1d2021"
					 :base01 "#1d2021"
                                         :base02 "#5c5d13"
					 :base03 "#fabd2f"
					 :base04 "#fb4934"
					 :base05 "#d3869b"
					 :base06 "#8ec07c"
					 :base07 "#d5c4a1"
					 :base08 "#665c54"
					 :base09 "#fb4934"
					 :base0A "#b8bb26"
					 :base0B "#fabd2f"
					 :base0C "#83a598"
					 :base0D "#d3869b"
					 :base0E "#8ec07c"
					 :base0F "#fbf1c7")))

(defvar base16-wal-colors nil "All colors for base16-wal are defined here.")
(set-wal-colors)

;; Define the theme
(deftheme base16-wal)

;; Add all the faces to the theme
(base16-theme-define 'base16-wal base16-wal-colors)

;; Mark the theme as provided
(provide-theme 'base16-wal)

(provide 'base16-wal)
