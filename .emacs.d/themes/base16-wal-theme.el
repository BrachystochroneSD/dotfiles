(require 'base16-theme)

;; colours generated dynamically by wal
(defun set-wal-colors () (setq base16-wal-colors
			       '(:base00 "#362121"
					 :base01 "#362121"
                                         :base02 "#645d32"
					 :base03 "#E77B9B"
					 :base04 "#DA9D68"
					 :base05 "#E5AF9B"
					 :base06 "#E7CEA7"
					 :base07 "#e4d1e0"
					 :base08 "#9f929c"
					 :base09 "#DA9D68"
					 :base0A "#C9BA65"
					 :base0B "#E77B9B"
					 :base0C "#A88683"
					 :base0D "#E5AF9B"
					 :base0E "#E7CEA7"
					 :base0F "#e4d1e0")))

(defvar base16-wal-colors nil "All colors for base16-wal are defined here.")
(set-wal-colors)

;; Define the theme
(deftheme base16-wal)

;; Add all the faces to the theme
(base16-theme-define 'base16-wal base16-wal-colors)

;; Mark the theme as provided
(provide-theme 'base16-wal)

(provide 'base16-wal)
