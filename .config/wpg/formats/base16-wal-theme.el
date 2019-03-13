(require 'base16-theme)

;; colours generated dynamically by wal
(defun set-wal-colors () (setq base16-wal-colors
			       '(:base00 "#223322"
					 :base01 "#223322"
					 :base02 "#57A7B8"
					 :base03 "#52A8D4"
					 :base04 "#817D9E"
					 :base05 "#E5CF99"
					 :base06 "#ECD7A0"
					 :base07 "#f6edd1"
					 :base08 "#aca592"
					 :base09 "#817D9E"
					 :base0A "#57A7B8"
					 :base0B "#52A8D4"
					 :base0C "#63ABD9"
					 :base0D "#E5CF99"
					 :base0E "#ECD7A0"
					 :base0F "#f6edd1")))

(defvar base16-wal-colors nil "All colors for base16-wal are defined here.")
(set-wal-colors)

;; Define the theme
(deftheme base16-wal)

;; Add all the faces to the theme
(base16-theme-define 'base16-wal base16-wal-colors)

;; Mark the theme as provided
(provide-theme 'base16-wal)

(provide 'base16-wal)
