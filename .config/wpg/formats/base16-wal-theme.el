(require 'base16-theme)

;; colours generated dynamically by wal
(defun set-wal-colors () (setq base16-wal-colors
			       '(:base00 "#2d2d1e"
					 :base01 "#2d2d1e"
					 :base02 "#71BD82"
					 :base03 "#73C288"
					 :base04 "#E1CD7C"
					 :base05 "#BBD899"
					 :base06 "#89D3A5"
					 :base07 "#dfe7c3"
					 :base08 "#9ca188"
					 :base09 "#E1CD7C"
					 :base0A "#71BD82"
					 :base0B "#73C288"
					 :base0C "#90CE9A"
					 :base0D "#BBD899"
					 :base0E "#89D3A5"
					 :base0F "#dfe7c3")))

(defvar base16-wal-colors nil "All colors for base16-wal are defined here.")
(set-wal-colors)

;; Define the theme
(deftheme base16-wal)

;; Add all the faces to the theme
(base16-theme-define 'base16-wal base16-wal-colors)

;; Mark the theme as provided
(provide-theme 'base16-wal)

(provide 'base16-wal)
