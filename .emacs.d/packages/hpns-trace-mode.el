;;; hpns-trace-mode.el --- sample major mode for editing LSL. -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Samuel Dawant ( samuel.dawant@equensworldline.com )
;; Version: 0.0.5
;; Created: 18 Jun 2019
;; Keywords: trace hpns

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Code:

(require 'hpns-alists)
(require 'tandemacs)

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq hpns-trace-font-lock-keywords
      (let* (
             ;; define several category of keywords
             (x-keywords '("RECORD" "KEY" "LEN" "no symbol"))
             (x-types '("INFO" "UKWN" "ISO-8583" "BKSTLV"))
             (x-banques '("->"))
             (x-events '("at_target" "attach" "Bitmap" "PAN" "Amount" "Datetime" "STAN" "Datetime local" "Point of Service data code" "Card Sequence Number" "Function code" "Auth resp code/Action Code" "Acquirer institution identification code" "ISO track 2 data" "Retrieval reference number" "Card acceptor terminal identification Code" "Card acceptor identification Code" "Card acceptor name/location" "Additional data, private" "Currency code, transaction" "Pin data" "Security related control information" "ICC system related data" "Message authentication code" "Process Code" "Amount (cardholder billing)" "Conversion rate, cardholder, billing" "Date, expiration" "Message reason code" "Card acceptor business code" "Date, reconciliation" "Forwarding institution identification code" "Currency code, cardholder, billing" "Transaction destination institution ID code" "Transaction originator institution ID code" "Receiving institution ID code" "Additional data – private" "De124 – Information text" "Mac" "Auth resp code" "Approval code" "Original data elements" "Currency code" "Credit, reversal number" "Debits, number" "Credit, reversal amount" "Debits, amount" "Amount, net reconciliation"))
             (x-format '("HEX" "ANS"))
             ;; generate regex string for each category of keywords
             (x-keywords-regexp (regexp-opt x-keywords t))
             (x-types-regexp (regexp-opt x-types t))
             (x-banques-regexp (regexp-opt x-banques t))
             (x-events-regexp (regexp-opt x-events t))
             (x-format-regexp (regexp-opt x-format 'words)))

        `((,x-types-regexp . font-lock-type-face)
          (,x-banques-regexp . font-lock-constant-face)
          (,x-events-regexp . font-lock-builtin-face)
          (,x-format-regexp . font-lock-function-name-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))



;;;;;;;;;;;;;;;;;;
;; TRACE PARSER ;;
;;;;;;;;;;;;;;;;;;
;; Trace format styles

(defvar hpns-trace-FISN-style-format
  '((name . "FISN style")
    (tracefile "FI")
    (spesymbregexp . "FIS")
    (DVL "$OB01" "$OB02")
    (parser . hpns-trace-FISN-parse-block))
  "Identifier for \"FISN\" style trace")

(defvar hpns-trace-equal-sign-style-format
  '((name . "equalsign style")
    (tracefile "BA")
    (spesymbregexp . ";[0-9]\\{3\\}=")
    (DVL "$OB01" "$OB02")
    (parser . hpns-trace-vb01-parse-block))
  "Identifier for \"equalsign\" style trace")

(defvar hpns-trace-TVL-style-format
  '((name . "tppn style")
    (tracefile "AY" "BY" "AA" "IA")
    (spesymbregexp . "\\(APN\\|[.][.]FIS\\|[.][.]AAP\\)")
    (DVL "$ABSQ" "$ABSR" "$GIP0")
    (parser . hpns-trace-TLV-parse-block))
  "Identifier for \"tppn\" style trace")

(defvar hpns-trace-symbol-style-format
  '((name . "no symbol style")
    (tracefile "IA" "MA")
    (spesymbregexp . "\\([[]ISO []] OUT\\|no symbol\\||BKSTLV|\\||ISO-8583|\\|\\[[0-9:]+\\]\\)")
    (DVL "$GIP0")
    (parser . hpns-trace-ISO-8583-or-BKSTLV-parse-block-at-point))
  "Identifier for \"symbol\" style trace")

(defvar hpns-trace-style-format-list
  `(,hpns-trace-TVL-style-format
    ,hpns-trace-equal-sign-style-format
    ,hpns-trace-symbol-style-format
    ,hpns-trace-FISN-style-format)
  "List of all trace format style")

(defvar hpns-trace-trace-format-param-list '(tracefile spesymbregexp)
  "List of all parameter to be checked.
For optimisation purpose, the order need to be from the most restrictive to the least one.")

;; Major Parser

(defun hpns-trace-smart-parse-block-at-point ()
  (interactive)
  (save-excursion
    (re-search-backward "RECORD[0-9 ]+KEY[0-9 ]+")
    (beginning-of-line)
    (setq beg (point))
    (or (re-search-forward "RECORD[0-9 ]+KEY[0-9 ]+" nil t 2) (end-of-buffer))
    (beginning-of-line)
    (backward-char 1)
    (setq end (point))
    (let (( style (hpns-trace-identify-trace-block-format (buffer-substring beg end)) ))
      (if (listp style)
          (progn
            (hpns-trace-pre-parse-region beg end)
            (delete-trailing-whitespace beg end)
            (hpns-trace-equalize-line-length-block-at-point)
            (message "Parsing using %s" (cdr (assoc 'name style)))
            (funcall (cdr (assoc 'parser style))))
        (message style)))))

(defun hpns-trace-identify-trace-block-format (block_string)
  "Identify the trace block format."
  (let ((formats hpns-trace-style-format-list))
    (dolist (param hpns-trace-trace-format-param-list)
      (setq formats (hpns-trace-check-param-at-block param block_string formats)))
    (if (not formats)
        "No format found for this block"
      (car formats))))

(defun hpns-trace-check-param-at-block (param string formats)
  (interactive)
  ;; (message "\n\nCURRENT FORMATS : \n%s\n\n" formats)
  (cond
   ((equal param 'tracefile)
    (seq-filter
     (lambda (elt) (member (hpns-trace-get-tracefile-name string) (cdr (assoc param elt)))) formats))
   ((equal param 'spesymbregexp)
    (seq-filter
     (lambda (elt) (string-match (cdr (assoc param elt)) string)) formats))
   ))

;; OBSELETE OBSELETE OBSELETE OBSELETE OBSELETE OBSELETE OBSELETE
;; (defun hpns-trace-smart-parse-block-at-point-old ()
;;   "Check the caracteristic of the trace block and use
;; `hpns-trace-TLV-parse-block' or `hpns-trace-ISO-8583-or-BKSTLV-parse-block-at-point'"
;;   (interactive)
;;   (hpns-trace-pre-parse-block)
;;   (save-excursion
;;     (setq beg (re-search-backward "RECORD[0-9 ]+KEY[0-9 ]+"))
;;     (setq end (or (re-search-forward "RECORD[0-9 ]+KEY[0-9 ]+" nil t 2) (point-max))))
;;   (cond
;;    ((>= (how-many "\\[[0-9:]+\\]" beg end) 1)
;;     (message "Parsing using \"Shit\"")
;;     (hpns-trace-ISO-8583-or-BKSTLV-parse-block-at-point))
;;    (t
;;     ;; (string-match "|UKWN|INFO|\\.\\." (buffer-substring beg end)) (when ..AAP alwyasthis parsing)
;;     (message "Parsing using default")
;;     (hpns-trace-TLV-parse-block))
;;    ))

;; Toogle trace infos

(defvar hpns-trace-keep-trace-infos t)

(defun hpns-trace-toggle-trace-infos ()
  "Toogle to keep infos while parsing.
Like the type of the field, of the number of character."
  (interactive)
  (setq hpns-trace-keep-trace-infos (not hpns-trace-keep-trace-infos))
  (if hpns-trace-keep-trace-infos
      (message "Trace infos is enabled globally")
    (message "Trace infos is disabled globally")))

;; Pre Parsing and little parser

(defun hpns-trace-get-max-lin-length-block ()
  (save-excursion
    (re-search-backward "RECORD[0-9 ]+KEY[0-9 ]+")
    (cl-flet (( set-next-beg-end ()
				 (next-logical-line)
				 (beginning-of-line)
				 (setq begline (point))
				 (end-of-line)
				 (setq endline (point))))
      (set-next-beg-end)
      (setq linelength 0)
      (while (and
	      (not(equal (point)(point-max)))
	      (not(string-match "RECORD[0-9 ]+KEY[0-9 ]+"
                                (buffer-substring begline endline))))
	(if (> (- endline begline) linelength )
	    (setq linelength (- endline begline) ))
	(set-next-beg-end))
      linelength)))

(defun hpns-trace-equalize-line-length-block-at-point ()
  (interactive)
  (save-excursion
    (setq maxlinelength (hpns-trace-get-max-lin-length-block))
    (cl-flet (( set-beg-end () (save-excursion
				 (setq begline (point))
				 (end-of-line)
				 (setq endline (point)))))
      (re-search-backward "RECORD[0-9 ]+KEY[0-9 ]+")
      (next-logical-line)
      (beginning-of-line)
      (set-beg-end)
      (while (and (not (equal (point) (point-max)))
                  (not (looking-at ".*RECORD[0-9 ]+KEY[0-9 ]+")))
	(end-of-line)
	(if (< (- endline begline) maxlinelength)
	    (dotimes (shit (- maxlinelength (- endline begline) )) (insert " ")))
	(delete-forward-char 1)
	(set-beg-end)))
    (newline 3)))

(defun hpns-trace-pre-parse-buffer ()
  (interactive)
  (hpns-trace-pre-parse-region (point-min) (point-max)))

(defun hpns-trace-pre-parse-region (start end)
  "Just get rid of line number"
  ;; (delete-trailing-whitespace start end) TODO
  (save-excursion
    (goto-char end)
    (while (> (point) start)
      (previous-logical-line)
      (beginning-of-line)
      (when (looking-at " *[0-9]+: *")
        (while (not(looking-at ":")) (delete-char 1))
        (delete-char 1)
        (while (looking-at " ") (delete-char 1))))))

(defun hpns-trace-parse-datetime-at-point ()
  (interactive)
  (save-excursion
    (backward-word)
    (if (looking-at "20[0-2][0-9][0-1][0-9][0-3][0-9]")
        (progn
          (forward-char 4)
          (insert "/")
          (forward-char 2)
          (insert "/")
          (forward-char 2)
          (insert " ")
          (forward-char 2)
          (insert ":")
          (forward-char 2)
          (insert ":"))
      (user-error "No datetime at point"))))

(defun hpns-trace-remove-empty-records ()
  (interactive)
  (save-excursion
    (let ((numline (read-number "Which lines ? (LEN)")))
      (while (re-search-forward (format "LEN %d" numline) (beginning-of-buffer) t)
        (backward-paragraph)
        (setq beg (point))
        (forward-paragraph)
        (setq end (point))
        (kill-region beg end)))))

(defun hpns-trace-parse-elv-shits ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward (rx "6") nil t)
      (delete-char 1)
      (replace-match ""))))

;; Tracefile name

(defun hpns-trace-get-tracefile-name-block-at-point ()
  (save-excursion
    (re-search-backward "RECORD[0-9 ]+KEY[0-9 ]+")
    (beginning-of-line)
    (setq beg (point))
    (or (re-search-forward "RECORD[0-9 ]+KEY[0-9 ]+" nil t 2) (end-of-buffer))
    (beginning-of-line)
    (backward-char 1)
    (setq end (point))
    (hpns-trace-get-tracefile-name (buffer-substring beg end))))

(defun hpns-trace-get-tracefile-name (string)
  (string-match "$BCT[0-9]+[.]TRACE.*[.]\\([A-Z]+?\\)[0-9]+" string)
  (match-string 1 string))

(defun hpns-trace-add-tracefile-name-in-format-style ()
  (interactive)
  (let* ((filename (hpns-trace-get-tracefile-name-block-at-point))
         (style (completing-read
                 (format "Adding trace file name \"%s\" into which style? " filename)
                 '("equal-sign" "tppn" "symbol"))) )
    (with-temp-buffer
      (find-file "~/.emacs.d/packages/hpns-trace-mode.el")
      (re-search-forward (format "defvar hpns-trace-%s-style-format" style) (beginning-of-buffer))
      (re-search-forward "tracefile")
      (end-of-line)
      (backward-char 1)
      (insert (format " \"%s\"" filename))
      (save-buffer)
      (switch-to-prev-buffer)))
  (message "Trace file name added."))


;; TPPN HARD Parser

(defvar hpns-trace-olti-1100 '((1 2 3 4 6 7 11 12 14 22 24 26 28 32 33 37 41 42 43 49 51 93 94 100) 10 23 25 35 48 52 55 121 124 128)
  "The list of field of message 1100 for tcpc-sinsys in the form of '((mandatorylist) conditionnallist)")

(defun hpns-trace-OLTI-parse-return-1100 ()
  (interactive)
  ;; SET DEFAULT VARIABLES : TODO : get rid of these
  (setq currentdate (format-time-string "%y%m%d" (current-time)))
  (setq pan "0000000000000000")

  (dolist (pnum (sort (concatenate 'list (car hpns-trace-olti-1100) (cdr hpns-trace-olti-1100)) '<))
    ;; Set variables when visited
    (cond
     ((equal pnum 2)
      (setq pan (buffer-substring (+ 2 (point)) (+ (string-to-number (buffer-substring (point) (+ 2 (point)))) (point)))))
     ((equal pnum 12)
      (setq currentdate (buffer-substring (point) (+ 6 (point))))))

    (let (( fieldValue (cdr (assoc pnum hpns-alists-olti-field)) ))
      (if (member pnum (car hpns-trace-olti-1100))
          (if (equal (substring (cadr fieldValue) -1) ".")
              (progn
                (insert (car fieldValue))
                (forward-char (- (length (cadr fieldValue)) 1))
                (insert " : ")
                (forward-char (string-to-number (buffer-substring (- (point) (+ 3 (- (length (cadr fieldValue)) 1))) (- (point) 3))))
                (newline))
            (progn
              (insert (car fieldValue))
              (forward-char (string-to-number (cadr fieldValue)))
              (newline)))
        (if (hpns-trace-olti-ommit-check pnum currentdate pan)
            (insert (format "%s Ommitted\n" (car fieldValue)))
          (if (equal (substring (cadr fieldValue) -1) ".")
              (progn
                (insert (car fieldValue))
                (forward-char (- (length (cadr fieldValue)) 1))
                (insert " : ")
                (forward-char (string-to-number (buffer-substring (- (point) (+ 3 (- (length (cadr fieldValue)) 1))) (- (point) 3))))
                (newline))
            (progn
              (insert (car fieldValue))
              (forward-char (string-to-number (cadr fieldValue)))
              (newline))))))))


(defun hpns-trace-check-date-format (string)
  "check if string as a dateime format"
  (and (>= (string-to-number (substring string 0 2)) 19)
       (<= (string-to-number (substring string 2 4)) 12)
       (<= (string-to-number (substring string 4 6)) 31)
       (< (string-to-number (substring string 6 8)) 24)
       (< (string-to-number (substring string 8 10)) 60)
       (< (string-to-number (substring string 10 12)) 60)
       ))

(defun hpns-trace-olti-ommit-check (fieldnum datetime pan)
  (cond
   ((equal fieldnum 10)
    (equal  (buffer-substring (- (point) 5 ) (- (point) 1)) (buffer-substring (+ 14 (point)) (+ 18 (point)))))
   ((equal fieldnum 23)
    (let (( datepoint (save-excursion (re-search-forward datetime))))
      (not (or (equal (abs (- (point) datepoint) ) 20) (equal (abs (- (point) datepoint) ) 16)))))
   ((equal fieldnum 25)
    (let (( datepoint (save-excursion (re-search-forward datetime))))
      (not (equal (abs (- (point) datepoint) ) 14))))
   ((equal fieldnum 35)
    (and
     (> (string-to-number (buffer-substring (point) (+ 2 (point)))) 37)
     (equal pan (buffer-substring (+ 2 (point)) (+ (length pan) (point))))))
   ((equal fieldnum 48)
    (not (string-match
	  (format "00[123]\\([0-9]\\)\\{3\\}\\([0-9UY]\\)\\{%d\\}" (- (string-to-number (buffer-substring (point) (+ 3 (point)))) 6))
	  (buffer-substring (+ 3 (point)) (+ 3 (point) (string-to-number (buffer-substring (point) (+ 3 (point)))))))))
   ((equal fieldnum 55)
    (or
     (not (string-match "\\([0-9]\\)\\{3\\}" (buffer-substring (point)(+ 3 (point)))))
     (not (string-match
	   "."
	   (buffer-substring (+ 3 (point)) (+ 3 (point) (string-to-number (buffer-substring (point) (+ 3 (point))))))))))
   ((equal fieldnum 121)
    (not (string-match
	  (format "[01][0-9]\\([0-9]\\)\\{2\\}\\([0-9A-Za-z]\\)\\{%d\\}" (- (string-to-number (buffer-substring (point) (+ 3 (point)))) 4))
	  (buffer-substring (+ 3 (point)) (+ 3 (point) (string-to-number (buffer-substring (point) (+ 3 (point)))))))))
   ((equal fieldnum 124)
    (or
     (> (string-to-number (buffer-substring (point) (+ 3 (point)))) 255)
     (not (string-match
	   (format "0[01]\\([0-9A-Z ]\\)\\{%d\\}" (- (string-to-number (buffer-substring (point) (+ 3 (point)))) 2))
	   (buffer-substring (+ 3 (point)) (+ 3 (point) (string-to-number (buffer-substring (point) (+ 3 (point))))))))))
   ((equal fieldnum 128)
    (or
     (not (string-match
	   "\\([^ ]\\)\\{8\\}"
	   (buffer-substring (point) (+ 8 (point)))))
     (not (string-match
	   "."
	   (buffer-substring (point) (+ 8 (point)))))))
   (t
    (y-or-n-p (format "Ommit field %d \"%s\" (length : %s)?" fieldnum (cadr(assoc fieldnum hpns-alists-olti-field)) (cddr(assoc fieldnum hpns-alists-olti-field)))))))

;; Parser FISN style

(defun hpns-trace-FISN-parse-block ()
  (interactive)
  (backward-paragraph)
  (re-search-forward "[.][.]FIS")
  (forward-char 7)
  (while (not (looking-at "[\t ]*\n"))
    (hpns-trace-FISN-parse-return)))

(defun hpns-trace-FISN-parse-return ()
  (interactive)
  (newline)
  (forward-char)
  (let (( name  (cdr (assoc (string-to-number (buffer-substring (point) (+ (point) 3))) hpns-alists-olti-field)) )
        ( length  (string-to-number (buffer-substring (+ (point) 3) (+ 7 (point)) )) ))
    (forward-char 3)
    (insert " : " )
    (when name (insert (format "%s " (car name))))
    (forward-char 4)
    (insert " : ")
    (forward-char length)))

;; Parser Equal Sign Style

(defun hpns-trace-vb01-parse-block ()
  (interactive)
  (backward-paragraph)
  (setq beg (point))
  (forward-paragraph)
  (setq end (point))
  (backward-paragraph)
  (dotimes (shit (how-many "=" beg end))
    (hpns-trace-vb01-parse-return)))

(defun hpns-trace-vb01-parse-return ()
  (interactive)
  (re-search-forward "=")
  (backward-char 4)
  (newline)
  (let (( name  (cdr (assoc (string-to-number (buffer-substring (point) (+ (point) 3))) hpns-alists-visa-fields)) ))
    (forward-char 3)
    (insert (format " : %s " (car name)))
    (forward-char 1)
    (insert " "))
  )

;; Parser TLV tag value Length style

(defun hpns-trace-TLV-parse-return ()
  (interactive)
  (forward-char 3)
  (let (( name  (cdr (assoc (string-to-number (buffer-substring (point) (- (point) 3))) hpns-alists-olti-field)) ))
    (if name
	(insert (format " : %s"(car name)))
      (insert " : ")))
  (let (( fieldnumchar (string-to-number (buffer-substring (point) (+ (point) 4))) ))
    (if hpns-trace-keep-trace-infos
        (progn
          (forward-char 4)
          (insert " : "))
      (delete-char 4))
    (forward-char fieldnumchar))
  (newline))


(defun hpns-trace-TLV-parse-block ()
  (interactive)
  (save-excursion
    (re-search-backward "RECORD[0-9 ]+KEY[0-9 ]+")
    (re-search-forward (rx (or "|FORCE|" "..")))
    (if (equal ".." (match-string 0))
        (if (looking-at "FIS")
            (progn
              (re-search-forward "[.]")
              (while (not (looking-at "[0-9]"))
                (delete-char 1)))
          (forward-char 10)))
    (newline)
    (while (string-match "[0-9]\\{3\\}" (buffer-substring (point) (+ 3 (point))))
      (hpns-trace-TLV-parse-return))
    (kill-line)))


;; Parser symbol style

(defun hpns-trace-ISO-8583-or-BKSTLV-parse-return (&optional fieldname)
  (interactive)
  (when (not hpns-trace-keep-trace-infos)
    (re-search-forward " +no symbol. +\\(ANS\\|HEX\\|N\\)\\[[0-9:]+\\][ :]")
    (replace-match ""))
  (re-search-forward "\\.\\([0-9]\\)\\{3\\}[: ]")
  (backward-char 4)
  (newline)
  (let (( name  (cdr (assoc (string-to-number (buffer-substring (point) (+ (point) 3))) hpns-alists-olti-field)) ))
    (if (and name fieldname)
	(insert (car name)))))


(defun hpns-trace-ISO-8583-or-BKSTLV-parse-block-at-point ()
  "Parse the no symbol style trace"
  (interactive)
  (save-excursion
    (hpns-trace-equalize-line-length-block-at-point)
    (backward-paragraph)
    (setq beg (point))
    (forward-paragraph)
    (setq end (point))
    (re-search-backward "RECORD[0-9 ]+KEY[0-9 ]+")
    (re-search-forward (rx (or "|FORCE|" "|INFO|" "OUT :")))
    (if (equal (match-string 0) "OUT :") (setq withfield nil) (setq withfield 1))
    (if (equal "mti" (buffer-substring (point) (+ 3 (point))))
	(setq isoadd -1)
      (setq isoadd 0))
    (newline)
    (dotimes (shit (- (how-many "\\[[0-9:]+\\]" beg end) 1 isoadd))
      (hpns-trace-ISO-8583-or-BKSTLV-parse-return withfield))))



(defvar hpns-trace-mode-map nil "Keymap for `hpns-trace-mode'")

(progn
  (setq hpns-trace-mode-map (make-sparse-keymap))

  (define-key hpns-trace-mode-map (kbd "C-c C-p") 'hpns-trace-pre-parse-buffer)

  (define-key hpns-trace-mode-map (kbd "C-c C-t") 'hpns-trace-parse-datetime-at-point)

  (define-key hpns-trace-mode-map (kbd "C-c C-e") 'hpns-trace-parse-elv-shits)

  (define-key hpns-trace-mode-map (kbd "C-c C-o") 'tandemacs-smart-check-code-at-point)

  (define-key hpns-trace-mode-map (kbd "C-c C-r") 'hpns-trace-remove-empty-records)

  (define-key hpns-trace-mode-map (kbd "<C-return>") 'hpns-trace-smart-parse-block-at-point)

  (define-key hpns-trace-mode-map (kbd "<M-return>") 'hpns-trace-TLV-parse-block)
  )

;;;###autoload
(define-derived-mode hpns-trace-mode text-mode "HPNS trace mode"
  "Major mode for highligting trace from server HPNS"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((hpns-trace-font-lock-keywords))))


;; add the mode to the `features' list
(provide 'hpns-trace-mode)

;;; hpns-trace-mode.el ends here
