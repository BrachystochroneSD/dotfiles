;;; tandemacs.el --- A tandem/TACL command assistant

;; This is free and unencumbered software released into the public domain.

;; Author: Samuel Dawant <samuel.dawant@equensworldline.com>
;; Keywords: Tandem HPNS TACL


;;; Notes:

;; Tandemacs is an assistant to be more efficient with the not-so-ergonomics
;; and no-so-fast brontosaurus that is Tandem and HPNS server

;; The main purpose of all of these was to avoid using Win6530 (and its shitty interfaces).

;;; Code:

(require 'eshell)
(require 'hpns-alists)

(defcustom tandemacs-traceDB-dir "~/.tandemacs/traceDB/"
  "Custom variable for the directory of traceDB TODO: explain a little ")

(defcustom tandemacs-prompt-regexp "\\($BCT[0-9]+..[A-Z0-9]+ [0-9]+> ?\\|EQL SCHEMA [0-9]+-> ?\\)"
  "Regexp for prompt in tandemacs")

(defvar tandemacs-keywords
  '("fndsrvrs" "fup" "logoff" "first" "copy" "count" "share" "fold" "bks" "vie" "sfnfo" "sdfnfoalt"
    "aoso" "atpp" "abeg"
    "boso" "btpp" "bbeg"
    "koso" "ktpp" "kbeg"
    "otpp"
    "sinfo" "altparam" "restart" "purgedata" "select" "distinct" "from" "emvparse"
    ;; Shortcuts :
    "load $bct7.dzprisda.sdamacro"
    "$bct7.dzprisda" "ste00jcl"
    ;; Custom program
    "atcchg" "perchk" "pinchk"
    ;; SERVER NAME:
    "oapp" "gapp" "napp"
    ;; miscelanous
    "aaut-tppn-vint" "fisn-tppn-vint" "vb01" "nfe" "ofe " "gfe" "vfe" "advv" "advv-base1" "advv-cacq"
    ;; ING
    "tcps-pnau-300"	"tcps-pnau-300"	"tcps-pnad-300"	"pppe-pnau-tp300" "pppe-pnad-tp300" "tcpc-300-olau" "tcpc-300-olau"
    ;; BPNPF
    "tcps-pnau-008" "tcps-pnau-008" "tcps-pnad-008" "pppe-pnau-tp008" "pppe-pnad-tp008" "tcpc-008-olau" "tcpc-008-olau"
    "bal-tppn" "apn-tppn" "bsp-aut-cnf" "ips-bsau" "bppp-ips-bsau" "baut" "olmg"
    ;; TRACE FILES
    "trace"
    ;; Advice chain
    "tcpc-726-opad koso" "fwrd-opad-726 koso"
    "tcpc-008-opad koso" "fwrd-opad-008 koso"
    "tcpc-300-opad koso" "fwrd-opad-300 koso"
    "tcpc-000-opad koso" "fwrd-opad-000 koso"
    ;; MasVISA chain
    "tcpc-sinsys"
    ;;EQL
    "dvbcttom.tomeqle" "eql"
    "d--hl-card-atc-2" "d--hl-r4-info.card-r4-atc"
    ;;BSP
    "tcps-bsau-eps" "tcps-bsad-eps" "bppp-bsau-eps" "bppp-bsad-eps" "bsp-bcmc-fwd" "tcpc-bsp"
    )
  "List of all of the keywords used by `tandemacs-completion-at-point'.
The initial value has the main tacl commands as
well as the list of the main server name")


(defun tandemacs-completion-at-point ()
  "This is a function to be used for the hook `completion-at-point-functions'.
Completion will check the custom words defined in `tandemacs-keywords'."
  (interactive)
  (let* (
         (bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (list start end (append tandemacs-keywords my-eshell-history-list) . nil ))) ;; TODO


(defun tandemacs-incident-table-check (num)
  (interactive
   (list (read-string "Check incident table: ")))
  (with-current-buffer (find-file "c:/Users/a757288/Documents/Tables/incident_table.org")
    (re-search-forward num (beginning-of-buffer))
    ))

(defun tandemacs-smart-check-code-at-point ()
  (interactive)
  (let ((line (buffer-substring (line-beginning-position) (line-end-position)) ))
    (cond
     ((string-match "Function Code" line)
      (message (tandemacs-check-code-alist (number-at-point) hpns-alists-function-code)))
     ((string-match "Incident Code" line)
      (message (tandemacs-check-code-alist (number-at-point) hpns-alists-incident-code)))
     ((string-match "Currency Code" line)
      (message (tandemacs-check-code-alist (number-at-point) hpns-alists-currency-code)))
     ((string-match "Action Code" line)
      (message (tandemacs-check-code-alist (number-at-point) hpns-alists-action-code)))
     (t
      (message "Code specs nont found, use specific function")))))

(defun tandemacs-check-code-alist (x alist)
  (or (cdr (assoc x alist)) (car (rassoc x alist)) "Not Found"))


(defun tandemacs-check-alist-at-point (alist)
  (interactive
   (list (completing-read "Which alist : " hpns-alists-list nil t)))
  (cond
   ((equal alist "Action")
    (message (tandemacs-check-code-alist (number-at-point) hpns-alists-action-code)))
   ((equal alist "Function")
    (message (tandemacs-check-code-alist (number-at-point) hpns-alists-function-code)))
   ((equal alist "Abbrev")
    (message (tandemacs-check-code-alist (upcase (word-at-point)) hpns-alists-abbrev)))
   ((equal alist "Currency")
    (message (tandemacs-check-code-alist (word-at-point) hpns-alists-currency-code)))
   ((equal alist "Incident")
    (message (tandemacs-check-code-alist (number-at-point) hpns-alists-incident-code)))))


(defun tandemacs-check-incident-code-at-point ()(interactive)(tandemacs-check-alist-at-point "Incident"))
(defun tandemacs-check-function-code-at-point ()(interactive)(tandemacs-check-alist-at-point "Function"))
(defun tandemacs-check-abbrev-code-at-point ()(interactive)(tandemacs-check-alist-at-point "Abbrev"))
(defun tandemacs-check-action-code-at-point ()(interactive)(tandemacs-check-alist-at-point "Action"))
(defun tandemacs-check-currency-code-at-point ()(interactive)(tandemacs-check-alist-at-point "Currency"))


(defun tandemacs-check-incident-code ()
  (interactive)
  (message (tandemacs-check-code-alist (read-number "Incident Code Check : ") hpns-alists-incident-code)))
(defun tandemacs-check-function-code ()
  (interactive)
  (message (tandemacs-check-code-alist (read-number "Function Code Check : ") hpns-alists-function-code)))
(defun tandemacs-check-abbrev-code ()
  (interactive)
  (message (tandemacs-check-code-alist (read-string "Abbrev Code Check : ") hpns-alists-abbrev)))
(defun tandemacs-check-action-code ()
  (interactive)
  (message (tandemacs-check-code-alist (read-number "Action Code Check : ") hpns-alists-action-code)))
(defun tandemacs-check-currency-code ()
  (interactive)
  (message (tandemacs-check-code-alist (read-number "Currency Code Check : ") hpns-alists-currency-code)))


(defun tandemacs-fupi ()
  "Insert typical fup info command"
  (interactive)
  (insert "fup info ,stat")
  (backward-char 5))


(defun tandemacs-insert-datetimestamp ()
  "Insert the datetimestamp convention of the current day
for the trace files"
  (interactive)
  (insert (format-time-string "%y%m%d" (current-time))))


(defun tandemacs-fupc ()
  "Insert a typical fup copy command"
  (interactive)
  (insert "fup copy ,,share,a")
  (backward-char 9))


(defun tandemacs-add-seconds (time s)
  "Add a S number of seconds to a string TIME formated like hh:mm
This is mainly because the time of the tandem server are 1:21 seconds earlier"
  (format-time-string "%H:%M" (time-add (date-to-time (format "FRI JUN 28 %s:00 2019" time)) s)))


(defun tandemacs-get-env-from-prompt ()
  "Get the environment number from the prompt.
   - BCT1 is for Sprog5
   - BCT2 is for Sprog3
   - BCT17 is for Sprog4"
  (save-excursion
    (when (re-search-backward "BCT[0-9]+" nil t)
	(cdr (assoc (match-string 0) '(("BCT1" . 5) ("BCT17" . 4) ("BCT2" . 3)))))))


(defun tandemacs-elv-command-insert ()
  "Insert a elv set of command to get the EMS error code
of a certain environement for a specific time (time
encoded and 2 minutes after it)"
  (interactive)
  (let* ((sprog-alist '((1 . "1A")(3 . "3C")(4 . "4D")(5 . "5E")(2 . "2B")))
	 (envnum (read-number "Environement " (tandemacs-get-env-from-prompt)))
	 (date (read-string "Date (TODAY or yyyy-mm-dd): " nil nil (format-time-string "%Y-%m-%d" (current-time))))
	 (time (read-string "Time of tx (hh:mm): "))
	 (interval (read-number "How many minutes: " 1))
	 (delay (read-number "Check delay ? (81 seconds delay between life and hpns server): " 81))
	 (ignoredList (split-string (read-string "ignored list: "))))
    (insert (format "elv\n%s\n%s\n%s\n%s\n%s\n\n" date (tandemacs-add-seconds time delay) date (tandemacs-add-seconds time (+ delay (* interval 60))) (cdr (assoc envnum sprog-alist))))
    (if ignoredList
	(dolist (ignvar ignoredList)
	  (insert (format "%s\n" ignvar)))
      ))
  (eshell-send-input))

(defun tandemacs-insert-daan-elv-shit ()
  (interactive)
  (let ((sprog-alist '((1 . "1A")(3 . "3C")(4 . "4D")(5 . "5E")(2 . "2B")))
        (envnum (read-number "Environement :")))
        (insert (format "elv\n\n\n\n\n%s\n\n$kbsx\n$kepi\n$kga\n$klga\n$krdi\n$ksif\n$kqtr\n$kzk\n$olga\nefb306\nbssi\nbbox\ncommunication prob\nconnection prob\nforward proc\ngat-\nnotif\nwfcall\nxs\n\n" (cdr (assoc envnum sprog-alist)))) ))


(defun tandemacs-copy-elv-output ()
  "Copy the output of a elv command"
  (interactive)
  (save-excursion
    (kill-ring-save
     (progn
       (beginning-of-line)
       (point))
     (progn
       (search-backward "READING" nil t)
       (next-line)
       (beginning-of-line)
       (point)))))


(defun tandemacs-copy-last-output ()
  "Copy the last output of a tandem prompt"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (setq begin (point))
    (re-search-backward tandemacs-prompt-regexp nil t)
    (next-line)
    (beginning-of-line)
    (setq end (point))
    (kill-ring-save begin end))
  (message "Last tandem output stored in the kill-ring."))


(defun tandemacs-workspace-tracefile ()
  (interactive)
  (let (( tracefile (tandemacs-get-trace-file-name-from-sinfo)))
    (tandemacs-copy-last-output)
    (find-file (read-file-name "Create new file: " (format "%s%s" tandemacs-traceDB-dir (format-time-string "%y%m%d" (current-time))) nil nil (format "/%s.htpr" "tracefile")))
    (yank)
    (hpns-trace-remove-numline)
    (beginning-of-buffer)))


(defun tandemacs-convert-time (time)
  "Convert a hh:mm:ss format TIME string into a number of seconds."
  (let ((hours (string-to-number (substring time 0 2)))
	(minutes (string-to-number (substring time 3 5)))
	(seconds (string-to-number (substring time 6 8))))
    (+ (* 60 60 hours) (* 60 minutes) seconds)))


(defun tandemacs-purge-last-week ()
  "Used to purge files to free LAC of a certain issuer.
This need to be used after a \"f\" command on the
Stop/Forward \"SP59XXX\" log subvolume inside the right environement volume."
  (interactive)
  (let (( string (buffer-substring
		  (progn
		    (search-backward (upcase (format-time-string "%d%b%Y"(time-subtract (current-time) 604800))) nil t)
		    (beginning-of-line)
		    (point))
		  (progn
		    (re-search-backward "OP\\([0-9]\\)\\{6\\}  O" nil t)
		    (next-line)
		    (point))) ))
    (end-of-buffer)
    (save-excursion (insert string)))
  (newline)
  (flush-lines "OP\\([0-9]\\)\\{6\\}\\( \\)\\{9\\}0A\\( \\)\\{15\\}0.*$" nil t)
  (while (re-search-forward "         0A.*$" nil t)
    (progn
      (replace-match " ")
      (delete-char 1)))
  (beginning-of-line)
  (delete-backward-char 1)
  (let ((filelist (split-string (buffer-substring (point) (point-max)))))
    (dolist (file filelist)
      (insert (format "fup purgedata %s" file))
      (newline)))
  (delete-region (point) (point-max)))

;;;;;;;;;;;;;;;;;;;;;
;; CHECK ON TABLES ;;
;;;;;;;;;;;;;;;;;;;;;


(defun tandemacs-crop-bcmc-pan (pan)
  (substring pan 4))


(defun tandemacs-check-pin ()
  "Check pin for pans (complete pans, no need to crop them)"
  (interactive)
  (let* (( panstring (mapconcat 'identity (cl-sort (split-string (read-string "Tandemacs Check Pin : Which Pans ? ")) 'string-lessp) " ") )
	 (index (string-match "6703" panstring)))
    (cond
     ((equal index 0)
      (tandemacs-check-pin-bcmc (split-string panstring)))
     ((not index)
      (tandemacs-check-pin-credit (split-string panstring)))
     (t
      (tandemacs-check-pin-credit (split-string (substring panstring 0 index)))
      (newline)
      (tandemacs-check-pin-bcmc (split-string (substring panstring index))))))
  (eshell-send-input))

(defvar tandemacs-check-sqlci-table-pan-name-alist '(("pin" "PIN_CARD_PAN")
						     ("ccpd" "CCPD_CARD_PAN"))
  "alist of pan card column name for each table")


;; NOTE : card, caky, acco, pin

(defvar tandemacs-sqlci-table-env-path
  '((3 . "$bct2.dvbctmdt")
    (5 . "$bct1.dvbctmdt")
    (4 . "$bct17.dvbctmdt")))


(defun tandemacs-check-sqlci-table (schema columns pans env)
  "Insert a sqlci command to check data in table SCHEMA.
COLUMNS and PANS are list of strings"
  (let (( pancol (car (cdr (assoc schema tandemacs-check-sqlci-table-pan-name-alist))) ))
    (insert (format "sqlci\nselect %s from %s.%s where %s like %s%%;exit;\n" (mapconcat 'identity columns ",") (cdr (assoc env tandemacs-sqlci-table-env-path)) schema pancol (mapconcat 'identity pans (format "%% or where %s like " pancol))))))


(defun tandemacs-check-pin-credit (panlist)
  (insert (format "sqlci\nselect PIN_CARD_PAN,PIN_BAPOF from =pin where PIN_CARD_PAN like \"%s%%\"" (mapconcat 'identity panlist "%\" or where PIN_CARD_PAN like \"")))
  (insert ";exit;"))


(defun tandemacs-check-pin-bcmc (panlist)
  (insert "dvbcttom.tomeqle\nset select hl d--hl.hl-key.holder-id\nset select hl d--hl.hl-holder.bapof\n")
  (if (> (length panlist) 1)
      (progn
	(insert "set where hl or d--hl.hl-key.holder-id like ")
	(insert (mapconcat 'tandemacs-crop-bcmc-pan panlist " \nset where hl or d--hl.hl-key.holder-id like "))
	(insert "\nselect * from hl\nexit"))
    (insert "select * from hl where pkey like " (substring (car panlist) 3) "\nexit")))


;; THE FOLLOWING ARE OBSOLETE, USE TANDEMACS-CHECK-PIN
;; (defun tandemacs-sqlci-pin-bapof ()
;;   (interactive)
;;   (let ((pan (read-string "Tandemacs check pin bapof for pan: ")))
;;   (insert (format "sqlci\nselect PIN_CARD_PAN,PIN_BAPOF from =pin where PIN_CARD_PAN like \"%s%%\";exit;\n" pan)))
;;   (eshell-send-input))
;; THE FOLLOWING ARE OBSOLETE, USE TANDEMACS-CHECK-PIN
;; THE FOLLOWING ARE OBSOLETE, USE TANDEMACS-CHECK-PIN
;; THE FOLLOWING ARE OBSOLETE, USE TANDEMACS-CHECK-PIN
;; (defun tandemacs-eql-pin-check ()
;;   (interactive)
;;   (let (( pan (read-string "Tandemacs check pin bapof for pan: (670)") ))
;;     (setq panlist (split-string pan " "))
;;     (insert "dvbcttom.tomeqle\n")
;;     (insert "set select hl d--hl.hl-key.holder-id\nset select hl d--hl.hl-holder.bapof\n")
;;     (if (> (length panlist ) 1)
;; 	(progn
;; 	  (while panlist
;; 	    (insert (format "set where hl or d--hl.hl-key.holder-id like %s\n" (substring (car panlist) 1)))
;; 	    (setq panlist (cdr panlist)))
;; 	  (insert (format "select * from hl\nexit\n")))
;;       (insert (format "select * from hl where pkey like %s\nexit\n" (car panlist)))))
;;   (eshell-send-input))

(defun tandemacs-check-atc ()
  "Check atc for pans (complete pans, no need to crop them)"
  (interactive)
  (let* (( panstring (mapconcat 'identity (cl-sort (split-string (read-string "Tandemacs Check ATC : Which Pans ? ")) 'string-lessp) " ") )
	 (index (string-match "6703" panstring)))
    (cond
     ((equal index 0)
      (tandemacs-check-atc-bcmc (split-string panstring)))
     ((not index)
      (tandemacs-check-atc-credit (split-string panstring)))
     (t
      (tandemacs-check-atc-credit (split-string (substring panstring 0 index)))
      (newline)
      (tandemacs-check-atc-bcmc (split-string (substring panstring index))))))
  (eshell-send-input))


(defun tandemacs-check-atc-credit (panlist)
  (insert (format "sqlci\nselect CCPD_CARD_PAN,CCPD_ATC from =ccpd where CCPD_CARD_PAN like  %s%%;exit;\n" (mapconcat 'identity panlist "% or where CCPD_CARD_PAN like "))))


(defun tandemacs-check-atc-bcmc (panlist)
  (insert "dvbcttom.tomeqle\nset select hl d--hl.hl-key.holder-id\nset select hl d--hl-r4-info.card-r4-atc\n")
  (if (> (length panlist) 1)
      (progn
	(insert "set where hl or d--hl.hl-key.holder-id like ")
	(insert (mapconcat 'tandemacs-crop-bcmc-pan panlist " \nset where hl or d--hl.hl-key.holder-id like "))
	(insert "\nselect * from hl\nexit"))
    (insert "select * from hl where pkey like " (substring (car panlist) 3))))


;; THE FOLLOWING ARE OBSOLETE, USE TANDEMACS-CHECK-ATC
;; (defun tandemacs-sqlci-atc-check ()
;;   (interactive)
;;   (let (( pan (read-string "Tandemacs check atc for pans: ") ))
;;     (insert (format "sqlci\nselect CCPD_CARD_PAN,CCPD_ATC from =ccpd where CCPD_CARD_PAN like \"%s%%\";exit;\n" pan))
;;   (eshell-send-input)))
;; THE FOLLOWING ARE OBSOLETE, USE TANDEMACS-CHECK-ATC
;; THE FOLLOWING ARE OBSOLETE, USE TANDEMACS-CHECK-ATC
;; THE FOLLOWING ARE OBSOLETE, USE TANDEMACS-CHECK-ATC
;; (defun tandemacs-eql-atc-check ()
;;   (interactive)
;;   (let (( pan (read-string "Tandemacs check atc for pan: (670)") ))
;;     (setq panlist (split-string pan " "))
;;     (insert "dvbcttom.tomeqle\n")
;;     (insert "set select hl d--hl.hl-key.holder-id\nhl d--hl-card-atc-2\nset select hl d--hl-r4-info.card-r4-atc\n")
;;     (if (> (length panlist ) 1)
;; 	(progn
;; 	  (while panlist
;; 	    (insert (format "set where hl or d--hl.hl-key.holder-id like %s\n" (substring (car panlist) 1)))
;; 	    (setq panlist (cdr panlist)))
;; 	  (insert "select * from hl\nexit\n"))
;;       (insert (format "select * from hl where pkey like %s\nexit\n" (car panlist)))))
;;   (eshell-send-input))
;; THE FOLLOWING ARE OBSOLETE, USE TANDEMACS-CHECK-ATC


(defun tandemacs-check-tcps-port ()
  (interactive)
  (let ((processname (read-string "Tandemacs check port for process: ")))
  (insert (format "sqlci\nselect TCPS_WK_PORT from =tcps where TCPS_FRONTEND = \"%s\";exit;\n" processname)))
  (eshell-send-input))


(defun tandemacs-check-pnpe-period (issuer envnum)
  "Insert a sqlci command to see the openned period of a desired issuer"
  (interactive
   (list (read-string "Check Period : ISSUER (59300) " nil nil "59300")
	 (read-number "Check Period : Env " (or (tandemacs-get-env-from-prompt) 5))))
  (insert (format "sqlci\nselect pnpe_per_nr from %s.pnpe where pnpe_acq_id = \"%s\" and pnpe_per_state = 1; exit;" (cdr(assoc envnum tandemacs-sqlci-table-env-path )) issuer)))

(defun tandemacs-get-trace-file-name-from-sinfo ()
  (save-excursion
    (if (re-search-backward "\\($BCT[0-9]+[.]TRACE.*[.][A-Z]+\\)" nil t)
	(match-string 1)
      (user-error "Tracefile not found"))))

(defun tandemacs-insert-trace-file-name ()
  (interactive)
  (insert (tandemacs-get-trace-file-name-from-sinfo))
  (insert (format-time-string "%y%m%d" (current-time))))

(defun tandemacs-smart-fup-info ()
  (interactive)
  (message (tandemacs-get-trace-file-name-from-sinfo))
  (insert (format "fup info %s%s,stat" (tandemacs-get-trace-file-name-from-sinfo) (format-time-string "%y%m%d" (current-time))))
  (backward-char 5))


(defun tandemacs-smart-fup-copy ()
  (interactive)
  (insert (format "fup copy %s%s,,share,a" (tandemacs-get-trace-file-name-from-sinfo) (format-time-string "%y%m%d" (current-time))))
  (backward-char 9))

(defun tandemacs-smart-sfc ()
  (interactive)
  (insert (format "sfc %s%s " (tandemacs-get-trace-file-name-from-sinfo) (format-time-string "%y%m%d" (current-time)))))

;;;;;;;;;;;;;;;;;;;;
;; MY ESHELL PART ;;
;;;;;;;;;;;;;;;;;;;;


(defun my-eshell-send-input-and-reset-index ()
  (interactive)
  (my-eshell-add-to-history (buffer-substring eshell-last-output-end (point-max)))
  (setq my-eshell-history-index 0)
  (eshell-send-input))


(defvar my-eshell-history-forbidden-list
  '("" "y" "n"))

(defun my-eshell-add-to-history (input)
  (if (not (member input my-eshell-history-forbidden-list))
      (setq my-eshell-history-list (cons input my-eshell-history-list))))


(defvar my-eshell-history-list '())

(defun my-eshell-backup-screen ()
  (interactive)
  (let ((buffcont (buffer-substring (point-min) (point-max)))
        (buffname (buffer-name)))
    (with-current-buffer (find-file (replace-regexp-in-string "[<>*]" "_" (format "~/.bufferbackup/screen%s%s.txt" buffname (format-time-string "%y%m%d" (current-time)))))
      (insert buffcont)
      (save-buffer)
      (kill-current-buffer))))

(defun my-eshell-clear-history () (interactive) (setq my-eshell-history-list '()))


(defun my-eshell-insert (input)
  (delete-region eshell-last-output-end (point-max))
  (insert input))


(defun my-eshell-last-input ()
  "Less Dirty history is less bad history."
  (interactive)
  (if (> (length my-eshell-history-list) 0)
      (progn (my-eshell-insert(nth (% my-eshell-history-index (length my-eshell-history-list)) my-eshell-history-list))
	     (setq my-eshell-history-index (+ 1 my-eshell-history-index)))
    (message "History empty")))

(defun my-eshell-clear ()
  (interactive)
  "Clear the eshell  buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun my-eshell-mode-hook ()
  (interactive)
  (setq eshell-prompt-regexp "\\($BCT[0-9]+..[A-Z0-9]+ [0-9]+> \\|EQL SCHEMA [0-9]+-> \\|^[^#$\n]* [#$] \\|>>\\|=\\)")
  (local-unset-key (kbd "C-c C-s"))
  (local-unset-key (kbd "C-c C-f"))
  (local-unset-key (kbd "C-c C-w"))
  (local-unset-key (kbd "M-n"))

  (local-set-key (kbd "M-n") 'forward-paragraph)

  (local-set-key (kbd "C-M-l") 'my-eshell-clear)

  (local-set-key (kbd "M-p") 'my-eshell-last-input)
  (local-set-key (kbd "<return>") 'my-eshell-send-input-and-reset-index)

  (local-set-key (kbd "C-c C-f c") 'tandemacs-fupc)

  (local-set-key (kbd "C-x C-s") 'my-eshell-backup-screen)

  (local-set-key (kbd "C-c C-f i") 'tandemacs-fupi)
  (local-set-key (kbd "C-c C-j") 'tandemacs-insert-trace-file-name)
  (local-set-key (kbd "C-c C-f C-c") 'tandemacs-smart-fup-copy)
  (local-set-key (kbd "C-c C-f C-s") 'tandemacs-smart-sfc)
  (local-set-key (kbd "C-c C-f C-i") 'tandemacs-smart-fup-info)

  (local-set-key (kbd "C-c C-s a") 'tandemacs-check-abbrev-at-point)
  (local-set-key (kbd "C-c C-s i") 'tandemacs-check-incident-code-at-point)
  (local-set-key (kbd "C-c C-s h") 'hextodec-at-point)

  (local-set-key (kbd "C-c C-t") 'tandemacs-insert-datetimestamp)

  (local-set-key (kbd "C-c C-w o") 'tandemacs-copy-last-output)
  (local-set-key (kbd "C-c C-w e") 'tandemacs-copy-elv-output)
  (add-hook 'completion-at-point-functions 'tandemacs-completion-at-point nil 'local)
  (local-set-key (kbd "<tab>") 'completion-at-point)
  (local-set-key (kbd "<C-return>") 'hpns-trace-smart-parse-block-at-point))

(add-hook 'eshell-mode-hook 'my-eshell-mode-hook)


(provide 'tandemacs)

;;; tandemacs.el ends here
