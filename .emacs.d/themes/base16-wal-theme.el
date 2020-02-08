(require 'base16-theme)

;; colours generated dynamically by wal
(defun set-wal-colors () (setq base16-wal-colors
			       '(:base00 "#2c2c2c"
					 :base01 "#2c2c2c"
                                         :base02 "#494c45"
					 :base03 "#d0c57a"
					 :base04 "#c77369"
					 :base05 "#825969"
					 :base06 "#938e8f"
					 :base07 "#ddddb5"
					 :base08 "#4e4e4e"
					 :base09 "#c77369"
					 :base0A "#93988a"
					 :base0B "#d0c57a"
					 :base0C "#747f89"
					 :base0D "#825969"
					 :base0E "#938e8f"
					 :base0F "#ededb7")))

(defvar base16-wal-colors nil "All colors for base16-wal are defined here.")
(set-wal-colors)

;; Define the theme
(deftheme base16-wal)

;; Add all the faces to the theme
(base16-theme-define 'base16-wal base16-wal-colors)

;; Mark the theme as provided
(provide-theme 'base16-wal)

(provide 'base16-wal)
