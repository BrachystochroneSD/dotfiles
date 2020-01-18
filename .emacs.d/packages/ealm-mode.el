;;; ealm-mode.el --- sample major mode for make ALM tables. -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Samuel Dawant ( samuel.dawant@equensworldline.com )
;; Version: 0.0.1
;; Created: 18 Jun 2019
;; Keywords: ALM Table

;; This file is not part of GNU Emacs.

;; Notes

;; This is based of the org-mode table package. I've just
;; put some function to make it suitable for ALM tables


;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Code:

(require 'org-table)

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq ealm-font-lock-keywords
      (let* (
             ;; define several category of keywords
             (x-keywords '("Subject" "Test Name" "Tester" "Testset" "Type" "Description" "Step Name" "Description" "Expected Result" "Designer"))
             (x-types '("Design Steps"))
             (x-banques '("jsfldkfj"))
             (x-events '("fjsldkjflsd"))
             (x-format '("lfskdjlfjs"))

             ;; generate regex string for each category of keywords
             (x-keywords-regexp (regexp-opt x-keywords t))
             (x-types-regexp (regexp-opt x-types t))
             (x-banques-regexp (regexp-opt x-banques t))
             (x-events-regexp (regexp-opt x-events t))
             (x-format-regexp (regexp-opt x-format 'words)))

        `(
          (,x-types-regexp . font-lock-type-face)
          (,x-banques-regexp . font-lock-constant-face)
          (,x-events-regexp . font-lock-builtin-face)
          (,x-format-regexp . font-lock-function-name-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

(defcustom ealm-temporary-folder "TEMPORARY TESTS\\SDA"
  "the name of the folder where I need to put my temporary test")

(defcustom ealm-current-folder nil
  "the name of the folder where I want to put my test, can be moified with `ealm-set-current-folder'")

(defcustom ealm-temptable-file "c:/Users/a757288/Documents/temptable.csv"
  "The location of the file where ealm will export the table")

(defcustom ealm-database-directory "~/.ealm/"
  "The directory where ealm will store all the exported csv and org-table is a dir-subdir tree.
It will help a lot for non-regression.")

(defcustom ealm-default-table "|Subject|Test Name|Designer|Tester|Testset|Type|Description|Step Name (Design Steps)|Description (Design Steps)|Expected Result (Design Steps)|"
  "List of header of the default table.
Called by `ealm-insert-table'")

(defvar ealm-testset-field-keywords
  '("FO-Payment Mobile" "VAS Server-VBK Service" "VAS Server-Multi Service" "VAS Server-ADV Service" "Undefined" "TS-Terminal Software" "OWS-WS" "OWS-PAS" "Odoo" "Mobile Application-Xengo" "IT Glob-Merchant Management" "IT Glob-Merchant Extranet" "ISP-IFSF-Payment Petrol" "ING C2T-ODS" "ING C2T-nCMS" "ING C2T-Generic card stop" "ING C2T-FO (TA/FA/TA scripts)" "ING C2T-Card Management WS" "ING C2T-Card Management GUI" "GCS-BKS" "GCS - Update" "GCS - Reporting" "GCS - INQ" "GCS - Block" "GCS - Admin" "FO-Withdrawal" "FO-TPPN" "FO-Terminal Status" "FO-Terminal Parametrisation" "FO-Terminal Balance" "FO-Terminal Association" "FO-Supervision Screens" "FO-SIM Management" "FO-Proton" "FO-PLUM" "FO-Pin Management" "FO-Payment Petrol-SATELLIC" "FO-Payment Petrol" "FO-Payment E-commerce" "FO-Payment Debit" "FO-Payment Credit" "FO-OLTB" "FO-NDH" "FO-Merchant Management" "FO-IVR" "FO-FD&R" "FO-CSM Life Cycle" "FO-Card Stop Center" "FO-Card Management" "FO-BCMC EMV" "FO&BO-Contract Management" "Extranet for merchants" "E-Voucher-EVC-Contract Management" "E-Voucher-EVC-Card Management" "E-Voucher-CTAP-POS" "E-Voucher-BXAP-ECOM" "ECS-Billing" "DBO-Sorter (SRT)" "DBO-Petrol Logging Extraction (CE)" "DBO-ODS" "DBO-Financial Treatment (CEC)" "DBO-Financial Logging Extraction (EXL)" "DBO-FD&R" "DBO-Clearing (NCC)" "Da Vinci-Web Service" "Da Vinci-Statement" "Da Vinci-Premium GUI" "Da Vinci-Payment & reimbursement" "Da Vinci-ODS" "Da Vinci-FO Payment credit" "Da Vinci-Daily txn & balance" "Da Vinci-Clearing" "Da Vinci-Card stop" "Da Vinci-Card Management" "Da Vinci-Accounting" "Cloud Wallet" "Central Issuing - IBO Sanity" "Central Issuing - E2E Integration" "Central Issuing - CISS Function" "Central acquiring-WAP-POS" "Central Acquiring-Third Party POS" "Central acquiring-SMAC-Contract Management" "Central Acquiring-SATELLIC" "Central acquiring-OLW" "Central acquiring-NOV-ATM" "Central acquiring-NGAP" "Central acquiring-NEXO-POS" "Central acquiring-NEXO-ECOM" "Central acquiring-Merlin-Contract Management" "Central acquiring-KAAI-POS" "Central acquiring-KAAI-Ecom (Standard)" "Central acquiring-KAAI-Ecom (Six Card)" "Central acquiring-IFSF-PET-TotalNeftis" "Central acquiring-IFSF-PET-Tokheim" "Central acquiring-IFSF-PET-Q8" "Central acquiring-GPE-POS" "Central acquiring-Gift Cards" "Central acquiring-GICC-POS" "Central acquiring-GICC-Manual Cash" "Central acquiring-GICC-ECOM" "Central acquiring-EPAS-SMARTPOS" "Central acquiring-EPAS-POS" "Central acquiring-CTAP-TOKEN" "Central acquiring-CTAP-TINA" "Central acquiring-CTAP-POS-TP" "Central acquiring-CTAP-POS-Spaar&Pluk" "Central acquiring-CTAP-POS-SK" "Central acquiring-CTAP-POS-MINITIX" "Central acquiring-CTAP-POS-JCB" "Central acquiring-CTAP-POS-IKANO" "Central acquiring-CTAP-POS-DINERS" "Central acquiring-CTAP-POS-DCC" "Central acquiring-CTAP-POS-CZ" "Central acquiring-CTAP-POS-CUP-NFC" "Central acquiring-CTAP-POS-CUP" "Central Acquiring-CTAP-POS-CTAP3" "Central Acquiring-CTAP-POS-CTAP10" "Central acquiring-CTAP-POS-AMEX" "Central acquiring-CTAP-POS-Alipay" "Central acquiring-CTAP-POS" "Central acquiring-CTAP-PET-JCB" "Central acquiring-CTAP-PET-DINERS" "Central acquiring-CTAP-PET-CUP" "Central acquiring-CTAP-PET-CTAP3" "Central acquiring-CTAP-PET-CTAP10" "Central acquiring-CTAP-PET-AMEX" "Central acquiring-CTAP-PET" "Central acquiring-CTAP-INSTALLMENT" "Central acquiring-BXAP-ECOM" "Central acquiring-Amex-Contract Management" "CBO-SMAC-Central Acquiring" "CBO-SMAC" "CBO-SK" "CBO-Repository" "CBO-RBU3" "CBO-RBU2" "CBO-ODS" "CBO-New Fee Rules" "CBO-MACS" "CBO-FPC" "CBO-Clearing" "CBO-CAMS" "CBO-ATM ACQ" "CBO-AMEX" "CBO-Accounting" "BO-PLUM" "BO-Mini Abo" "BO-INT2PEACH" "BO-Card Management" "BO-BCMC EMV" "BMDIR-Transactional Processing" "BMDIR-Data Management" "BMA-WOPA-REFUND" "BMA-WAP-POS-Tokheim" "BMA-WAP-POS-Keyware" "BMA-Third Party POS" "BMA-Protocol Checkers" "BMA-IR" "BMA-IFSF-PET-Total Neftis" "BMA-FDR" "BMA-CTAP-TINA-CTAP10" "BMA-CTAP-TINA" "BMA-CTAP-POS-NO PIN" "BMA-CTAP-POS-NGAP" "BMA-CTAP-POS-CTAP3" "BMA-CTAP-POS-CTAP10" "BMA-CTAP-POS-CONTACTLESS" "BMA-CTAP-POS" "BMA-CTAP-PET-CTAP3" "BMA-CTAP-PET-CTAP10" "BMA-CTAP-PET" "BMA-CTAP-OCT&Refund" "BMA-CTAP-Contract Management" "BMA-BXAP-ECOM" "BMA-ABO" "BI-Reporting" "Automatic TSC" "Automatic Transactions Abroad" "Automatic TPPN" "Automatic Proton" "Automatic Petrol" "Automatic E-commerce" "Automatic Distribution" "Automatic Bank Advice Private Operations" "Automatic ATM")
  "List of keywords for the field \"Testset\" used by `ealm-completion-at-point'.")

(defvar ealm-subject-field-keywords
  '("TEMPORARY TESTS" "SDA" "Banks (BNK)" "Business View" "Cardholder (CH)" "Security (SCY)" "Debit Card Management (DC MGT)")
  "List of main keywords for the field \"Subject\" used by `ealm-completion-at-point'.")

(defvar ealm-keywords
  '("a757288" "Subject" "Test Name" "Tester" "Testset" "Type" "Description" "Step Name" "Description" "Expected Result" "Designer" )
  "List of main keywords used by `ealm-completion-at-point'.")

;;;;;;;;;;;;;;;;
;; COMPLETION ;;
;;;;;;;;;;;;;;;;

(defun ealm-completion-at-point ()
  "This is a function to be used for the hook `completion-at-point-functions'.
Completion will check the custom words defined in `ealm-testset-field-keywords' or `ealm-keywords'."
  (interactive)
  (let* (
         (bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (cond
     ((equal (ealm-current-column-header) "Testset")
      (list start end ealm-testset-field-keywords . nil ))
     ((equal (ealm-current-column-header) "Subject")
      (list start end ealm-subject-field-keywords . nil ))
     (t
      (list start end ealm-keywords . nil )))))

;;;;;;;;;;;;;;;;;
;; BASIC SHITS ;;
;;;;;;;;;;;;;;;;;

(defun ealm-set-current-folder (string)
  (interactive
   (list (read-string "Set current folder")))
  (setq ealm-current-folder string))

(defun ealm-insert-default-folder ()
  "Insert the default folder"
  (interactive)
  (insert (format "%s\\" (mapconcat 'identity (list ealm-temporary-folder ealm-current-folder) "\\"))))

(defun ealm-get-current-cell-content ()
  (interactive)
  (org-table-get (org-table-current-line) (org-table-current-column)))

(defun ealm-field-is-empty (&optional n colname noerror)
  "Check if the table field at point is empty.

If n is set, check n lines from the point (positive or negative number).

If colname is set, check the column with COLNAME for header name.

If no NOERROR is set, return NOERROR if no cell is found at the desired position. Return t otherwise"
  (unless n (setq n 0))
  (unless noerror (setq noerror t))
  (if colname
      (setq col (ealm-get-column-number-from-name colname))
    (setq col (org-table-current-column)))
  (condition-case nil
      (equal "" (replace-regexp-in-string " " "" (org-table-get (+ n (org-table-current-line)) col)))
    (error noerror)))

(defun ealm-tab ()
  (interactive)
  (if (org-at-table-p)
      (ealm-next-field)
    (indent-for-tab-command)))

(defun ealm-next-field ()
  "Insert default value according to the header and go to the next field."
  (interactive)
  (cond
   ((equal (ealm-current-column-header) "Subject")
    (progn
      (org-table-next-field)
      (if (ealm-field-is-empty)
	  (insert (ealm-get-nomenclature)))))
   ((string-match (ealm-current-column-header) "\\(Designer\\|Tester\\)")
    (progn
      (if (ealm-field-is-empty)
	  (insert "\"A757288\""))
      (org-table-next-field)))
   ((equal (ealm-current-column-header) "Testset")
    (progn
      (if (ealm-field-is-empty)
	  (insert "\"Undefined\""))
      (org-table-next-field)))
   ((equal (ealm-current-column-header) "Type")
    (progn
      (if (ealm-field-is-empty)
	  (insert "\"MANUAL\""))
      (org-table-next-field)))
   ((equal (ealm-current-column-header) "Step Name (Design Steps)")
    (progn
      (if (ealm-field-is-empty)
	  (insert "\"Step 0\""))
      (org-table-next-field)))
   (t (org-table-next-field))))

(defun ealm-insert-table (&optional header)
  "Insert the table with HEADER as the... well... header.
 If HEADER omitted, take the basic of a table defined in `ealm-default-table'"
  (interactive)
  (unless header (setq header (read-string "Which header (format : |a|b|c|d|e|):" nil nil ealm-default-table)))
  (insert (concat "|-\n" header "\n|-"))
  (org-table-next-field))

(defun ealm-table-copy-column (list)
  "Copy the row with values defined by the names of the columns in list."
  (save-excursion
    (dolist (colname list)
      (org-table-goto-column (ealm-get-column-number-from-name colname))
      (org-table-copy-down 1))))

(defun ealm-current-column-header ()
  "Get the header name of the column"
  (org-table-get 1 (org-table-current-column)))

(defun ealm-goto-column (name)
  "Get the header name of the column"
  (org-table-goto-column (ealm-get-column-number-from-name name)))

(defun ealm-get-column-number-from-name (name)
  "Get the column number with the header NAME."
  (save-excursion
    (search-forward name (beginning-of-buffer) t)
    (org-table-current-column)))


(defun ealm-insert-new-step ()
  "Insert a new step by incrementing the previous step number."
  (interactive)
  (org-table-align)
  (if (ealm-field-is-empty 1 "Step Name (Design Steps)")
      (progn
	(ealm-table-copy-column '("Subject"  "Test Name"  "Tester"  "Testset"  "Type"  "Designer"))
	(save-excursion
	  (ealm-goto-column "Step Name (Design Steps)")
	  (let ((nextstep (+ 1 (string-to-number (replace-regexp-in-string "Step " "" (org-table-get-field) )))))
	    (org-table-next-row)
	    (insert (format "Step %d" nextstep))
	    (org-table-align)))))
  (org-table-next-row))

;;;;;;;;;;;;;;;;
;; REFERENCES ;;
;;;;;;;;;;;;;;;;

(defun ealm-change-ref-name ()
  (interactive)
  (message "TODO"))

(defun ealm-delete-reference (refname)
  (if (re-search-forward (format "!%s! ?{" refname) (beginning-of-buffer) t)
      (progn
        (beginning-of-line)
        (setq beg (point))
        (re-search-forward "}")
        (setq end (point))
        (delete-region beg end)
        (delete-char 2))
    (user-error "No references Found")))

(defun ealm-format-string (string)
  (when (string-match "\"\\(.*\\)\"" string)
    (setq string (replace-regexp-in-string "\\\\n" "\n" (substring string 1 -1))))
  string)

(defun ealm-ref-insert-all-ref ()
  (interactive)
  (if (org-at-table-p)
      (let ((cell-content (ealm-get-current-cell-content)))
        (if (string-match "!REF!:.*" cell-content)
            (progn
              (save-excursion
                (while (re-search-forward cell-content (beginning-of-buffer) t)
                  (ealm-ref-insert-ref-in-table)))
              (ealm-delete-reference (replace-regexp-in-string "!REF!:" "" cell-content)))
          (user-error "No ref at point")))
    (user-error "No Table at Point")))

(defun ealm-ref-insert-ref-in-table ()
  (interactive)
  (if (org-at-table-p)
      (save-excursion
        (org-table-align)
        (let ((content (ealm-get-reference-content (ealm-get-current-cell-content)) ))
          (org-table-blank-field)
          (insert (format "\"%s\"" (replace-regexp-in-string "\n" "\\\\n" content)))
          (org-table-align)
          ))
    (user-error "No table at point")))

(defun ealm-insert-existing-ref ()
  (interactive)
  (if (org-at-table-p)
      (let ((refname (completing-read "Which ref to be inserted: " (ealm-ref-get-all-ref))))
        (org-table-blank-field)
        (insert (format "!REF!:%s" refname))
        (org-table-align))
    (user-error "No table at point")))

(defun ealm-ref-create (&optional USEDEFAULT)
  "Create a reference on the table and move the content of the
cell ine the end of the buffer.
If USEDEFAULT is nil or omitted, prompt for a reference name"
  (interactive)
  (let* ((defname (ealm-ref-get-next-refname (upcase (ealm-current-column-header))))
         (refname (if USEDEFAULT defname (read-string (format "New ref name (default: %s)" defname) nil nil defname)))
         (cell-content (ealm-get-current-cell-content)))
    (while (search-forward cell-content (beginning-of-buffer) t)
      (org-table-blank-field)
      (insert (format "!REF!:%s" refname))
      (org-table-align))
    (end-of-buffer)
    (beginning-of-line)
    (unless (eobp)
      (end-of-buffer)
      (newline))
    (newline)
    (ealm-ref-insert-ref refname cell-content)))

(defun ealm-ref-create-or-insert ()
  (interactive)
  (if (org-at-table-p)
      (let ((cell-content (ealm-get-current-cell-content)))
        (if (string-match "!REF!:.*" cell-content)
            (ealm-ref-insert-all-ref)
          (ealm-ref-create-use-default)))
    (user-error "No Table at Point")))

(defun ealm-ref-create-use-default ()
  (interactive)
  (ealm-ref-create t))

(defun ealm-ref-insert-ref (refname &optional content)
  (insert (format "!%s! {\n\n}" refname))
  (backward-char 2)
  (when content
    (insert (ealm-format-string content))))

(defun ealm-ref-get-next-refname (string)
  "Get the next reference name from STRING by adidng the right num"
  (let ((reflist (ealm-ref-get-all-ref))
        (refnum 1))
    (while (member (format "%s%d" string refnum) reflist)
      (setq refnum (1+ refnum)))
    (format "%s%d" string refnum)))

(defun ealm-ref-get-all-ref ()
  (save-excursion
    (let ((reslist))
      (beginning-of-buffer)
      (while (re-search-forward "!\\(.*\\)![\t \n]+?{" nil t)
        (setq reslist (cons (match-string 1) reslist)))
      reslist)))

(defun ealm-ref-goto-ref ()
  (interactive)
  (let ((pointsave (point))
        (content (org-table-get (org-table-current-line) (org-table-current-column))))
    (if (org-table-p)
        (if (re-search-forward (format "!%s!" (ealm-ref-parse content)) (beginning-of-buffer) t)
            (progn
              (re-search-forward "}")
              (backward-char 2))
          (progn
            (user-error "No reference found")
            (goto-char pointsave)))
      (when (re-search-backward "!\\(.*\\)! ?{" nil t)
        (re-search-forward (format "!REF!:%s" (match-string 1)) (beginning-of-buffer))))))

(defun ealm-ref-parse (string)
  "Parse the !REF!: cell"
  (when (equal (substring string 0 1) " ")
    (setq string (substring string 1) ))
  (when (equal (substring string -1) " ")
    (setq string (substring string 0 -1) ))
  (replace-regexp-in-string "!REF!:" "" string))

(defun ealm-get-reference-content (string)
  (save-excursion
    (re-search-backward (ealm-ref-parse string) (end-of-buffer))
    (re-search-forward "{")
    (while (looking-at "[\n ]") (forward-char))
    (setq beg (point))
    (re-search-forward "}\n?")
    (backward-char 3)
    (while (looking-at "[\n ]") (backward-char))
    (setq end (+ (point) 1))
    (buffer-substring beg end)))


(defun ealm-get-all-folder-names (string)
  "Get the folder name from a string by extracting all the capitals letters inside the parenthesis"
  (when (string-match "!REF!:" string)
    (setq string (ealm-get-reference-content string)))
  (let ((res "")
	(index 0))
    (while (string-match "(\\([A-Z0-9]+\\))" string index)
      (setq res (concat res (match-string 1 string) "-"))
      (setq index (match-end 0)))
    res))

(defun ealm-get-nomenclature ()
  "Get the nomenclature of the line from the Subject folder tree"
  (ealm-get-all-folder-names (org-table-get (org-table-current-line) (ealm-get-column-number-from-name "Subject"))))

;;;;;;;;;;;;;;;;;
;; IMPORTATION ;;
;;;;;;;;;;;;;;;;;

(defvar ealm-complete-table "|Subject|Test Name|Designer|Tester|Testset|Type|Modified|Execution status|Test-Type|Creation Date|Estimated time|Ref Proj.|Status|Test ID|Description|Step Name (Design Steps)|Description (Design Steps)|Expected Result (Design Steps)|"
  "List of header of the complete table.")

(defvar ealm-parse-alm-mail-keywords-list
  '("Description" "Test ID" "Subject" "Status" "Designer" "Creation Date" "Type" "Estimated time" "Test priority" "Ref Proj." "Testset" "Test-Type" "Tester" "Execution status" "Modified" "Design Steps"
    )
  "list of all keywords used by `ealm-parse-ALM-mail'
for the importation of an ALM test")

(defvar ealm-field-list'("Subject" "Test Name" "Designer" "Tester" "Testset" "Type" "Modified" "Execution status" "Test-Type" "Creation Date" "Estimated time" "Ref Proj." "Status" "Test ID" "Description")
  "list of the field name")

(defun ealm-get-bufsubstr-to-eol ()
  "Function to get the buffer from the current point
to the end of line"
  (interactive)
  (setq beg (point))
  (end-of-line)
  (setq end (point))
  (buffer-substring beg end))

(defun ealm-parse-ALM-mail-as-list ()
  "Parse an ALM mail into an associative list of each
fields."
  (let ((reslist))
    (dolist (fieldname ealm-parse-alm-mail-keywords-list)
      (when (re-search-forward fieldname (beginning-of-buffer) t)
        (cond
         ((equal "Test ID" fieldname)
          (forward-char 3)
          (setq reslist (cons (cons fieldname (ealm-get-bufsubstr-to-eol)) reslist))
          (forward-char)
          (setq reslist (cons (cons "Test Name" (ealm-get-bufsubstr-to-eol)) reslist)))
         ((equal "Description" fieldname)
          (forward-char)
          (setq beg (point))
          (re-search-forward "\n\nTest Details\n\n")
          (previous-logical-line 3)
          (backward-char)
          (setq end (point))
          (setq reslist (cons (cons fieldname (buffer-substring beg end)) reslist)))
         ((equal "Design Steps" fieldname)
          (let ((steplist))
            (when (re-search-forward "Name	Description	Expected Result" (beginning-of-buffer) t)
              (while (re-search-forward "Step [0-9]+" nil t)
                (beginning-of-line)
                (setq beg (point))
                (or (re-search-forward "Step [0-9]+" nil t 2)
                    (end-of-buffer))
                (beginning-of-line)
                (backward-char)
                (setq end (point))
                (setq steplist (cons (split-string (buffer-substring beg end) "\t") steplist)))
              (setq reslist (cons (cons fieldname steplist) reslist)))))
         (t
          (if (or (equal "Tester" fieldname)
                  (equal "Designer" fieldname))
              (progn
                (next-logical-line)
                (beginning-of-line))
            (forward-char))
          (setq fieldvalue (ealm-get-bufsubstr-to-eol))
          (setq reslist (cons (cons fieldname fieldvalue) reslist))))))
    reslist))

(defun ealm-pre-parse-shits ()
  "Pre parsing the mail to get rid of shits like list item
with tabs."
  (interactive)
  (save-excursion
    (while (re-search-forward "\"\t" (beginning-of-buffer) t)
      (replace-match "  - "))
    (while (re-search-forward "\"" (beginning-of-buffer) t)
      (replace-match "''"))))

(defun ealm-import-mail-alm ()
  "Import the current buffer that is a copy of an ALM mail
into a ealm format table. (The given table can be exported into
an excel file)"
  (interactive)
  (ealm-pre-parse-shits)
  (let ((valuelist (ealm-parse-ALM-mail-as-list)))
    ;; (message "%s" valuelist)
    (with-current-buffer (get-buffer-create "*ealm-table-expoted*")
      (ealm-mode)
      (insert "|-\n|Subject|Test Name|Designer|Tester|Testset|Type|Modified|Execution status|Test-Type|Creation Date|Estimated time|Ref Proj.|Status|Test ID|Description|Step Name (Design Steps)|Description (Design Steps)|Expected Result (Design Steps)|\n|-\n")
      (dolist (stepobj (reverse (cdr (assoc "Design Steps" valuelist))))
        (insert "|")
        (dolist (fieldname ealm-field-list)
          (when (cdr (assoc fieldname valuelist))
            (insert (format "\"%s\"" (replace-regexp-in-string "\n" "\\\\n" (cdr (assoc fieldname valuelist))))))
          (insert "|"))
        (insert (format "\"%s\"|\"%s\"|\"%s\"|\n" (replace-regexp-in-string "\n" "\\\\n" (car stepobj)) (replace-regexp-in-string "\n" "\\\\n" (cadr stepobj)) (replace-regexp-in-string "\n" "\\\\n" (caddr stepobj)))))
      (org-table-align)
      (switch-to-buffer "*ealm-table-expoted*"))))

;;;;;;;;;;;;;;;;;
;; EXPORTATION ;;
;;;;;;;;;;;;;;;;;

(defun ealm-table-export (&optional file format)
  "This is based from `org-table-export', only change the \\n into \n."
  (interactive)
  (unless (org-at-table-p) (user-error "No table at point"))
  (org-table-align)	       ; Make sure we have everything we need.
  (let ((file (or file (org-entry-get (point) "TABLE_EXPORT_FILE" t))))
    (unless file
      (setq file (read-file-name "Export table to: "))
      (unless (or (not (file-exists-p file))
		  (y-or-n-p (format "Overwrite file %s? " file)))
	(user-error "File not written")))
    (when (file-directory-p file)
      (user-error "This is a directory path, not a file"))
    (when (and (buffer-file-name (buffer-base-buffer))
	       (file-equal-p
		(file-truename file)
		(file-truename (buffer-file-name (buffer-base-buffer)))))
      (user-error "Please specify a file name that is different from current"))
    (let ((fileext (concat (file-name-extension file) "$"))
	  (format (or format (org-entry-get (point) "TABLE_EXPORT_FORMAT" t))))
      (unless format
	(let* ((formats '("ealm-orgtbl-to-csv" "orgtbl-to-tsv" "orgtbl-to-csv" "orgtbl-to-latex"
			  "orgtbl-to-html" "orgtbl-to-generic"
			  "orgtbl-to-texinfo" "orgtbl-to-orgtbl"
			  "orgtbl-to-unicode"))
	       (deffmt-readable
		 (replace-regexp-in-string
		  "\t" "\\t"
		  (replace-regexp-in-string
		   "\n" "\\n"
		   (or (car (delq nil
				  (mapcar
				   (lambda (f)
				     (and (string-match-p fileext f) f))
				   formats)))
		       org-table-export-default-format)
		   t t) t t)))
	  (setq format
		(org-completing-read
		 "Format: " formats nil nil deffmt-readable))))
      (if (string-match "\\([^ \t\r\n]+\\)\\( +.*\\)?" format)
	  (let ((transform (intern (match-string 1 format)))
		(params (and (match-end 2)
			     (read (concat "(" (match-string 2 format) ")"))))
		(table (org-table-to-lisp
			(buffer-substring-no-properties
			 (org-table-begin) (org-table-end)))))
	    (unless (fboundp transform)
	      (user-error "No such transformation function %s" transform))
	    (let (buf)
	      (with-current-buffer (find-file-noselect file)
		(setq buf (current-buffer))
		(erase-buffer)
		(fundamental-mode)
		(insert (funcall transform table params) "\n")
		(if (equal format "ealm-orgtbl-to-csv")
		    (while (search-forward "\\n" (beginning-of-buffer) t)
		      (replace-match "\n")))
		(save-buffer))
	      (kill-buffer buf))
	    (message "Export done."))
	(user-error "TABLE_EXPORT_FORMAT invalid")))))


(defun ealm-orgtbl-to-csv (table params)
  "Change the spearator into a ; because we are in EU"
  (orgtbl-to-generic table
		     (org-combine-plists '(:sep ";")
					 params)))
(put 'downcase-region 'disabled nil)

(defun ealm-export-csv-and-open-excel ()
  "Export into a csv file and open it with excel to be uploaded with the ALM add-in."
  (interactive)
  (if (org-at-table-p)
      (let (( buffcontent (buffer-substring (point-min) (point-max)) ))
        (with-temp-buffer
          (switch-to-buffer (current-buffer))
          (insert buffcontent)
          (while (re-search-forward "!REF!" (beginning-of-buffer) t)
            (ealm-ref-insert-ref-in-table))
          (ealm-table-export ealm-temptable-file "ealm-orgtbl-to-csv")
          (start-process-shell-command "excel ALM" nil (format "\"C:/Program Files (x86)/Microsoft Office/Office14/EXCEL.EXE\" %s" ealm-temptable-file))
          ))
    (user-error "No table at Point")))

;;;;;;;;;;;;;;;;;
;; INDENTATION ;;
;;;;;;;;;;;;;;;;;

(defun ealm-indent-line ()
  "Indent current line of EALM code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
	(indent (condition-case nil (max (ealm-calculate-indentation) 0)
		  (error 0))))
    (if savep (save-excursion (indent-line-to indent))
      (indent-line-to indent))))


(defvar ealm-indent-offset 2
  "Offset during indentation in ealm-mode.")

(defun ealm-calculate-indentation ()
  "Return appropriate indentation for current line in ealm mode.
   Returns an integer: the column to indent to."
  (let ((indent 0)
        (offset 0))
    (save-excursion
      (beginning-of-line)
      (unless (bobp)
	(unless (looking-at "\\(.*{\\|[\t ]*}\\)")
          (setq offset 1))
        (backward-char)
        (while (and (not (looking-at "{"))
                    (not (equal 0 (current-indentation))))
          (backward-char))
        (beginning-of-line)
        (when (or (looking-at ".*{")
                  (not (equal 0 (current-indentation))))
          (setq indent (+ (current-indentation) (* offset ealm-indent-offset))))))
    indent))

(defun ealm-shifttab ()
  (interactive)
  (if (org-at-table-p)
      (org-shifttab)
    (mode-line-other-buffer)))

;;;;;;;;;;;;;;;;;;;;;;
;; KEYBOARD MAPPING ;;
;;;;;;;;;;;;;;;;;;;;;;

(defvar ealm-mode-map nil "Keymap for `ealm-mode'")

(progn
  (setq ealm-mode-map (make-sparse-keymap))

  (define-key ealm-mode-map (kbd "C-c C-c") 'ealm-export-csv-and-open-excel)

  (define-key ealm-mode-map (kbd "C-c C-s") 'ealm-insert-table)

  (define-key ealm-mode-map (kbd "C-c C-r") 'ealm-ref-create-or-insert)

  (define-key ealm-mode-map (kbd "C-c M-r") 'ealm-ref-insert-all-ref)

  (define-key ealm-mode-map (kbd "C-c C-n") 'ealm-insert-new-step)

  (define-key ealm-mode-map (kbd "<tab>") 'ealm-tab)

  (define-key ealm-mode-map (kbd "<C-S-tab>") 'org-table-next-field)

  (define-key ealm-mode-map (kbd "<C-tab>") 'ealm-shifttab)

  (define-key ealm-mode-map (kbd "C-c i d") 'ealm-insert-default-folder)

  (define-key ealm-mode-map (kbd "C-c s c") 'ealm-set-current-folder)

  (define-key ealm-mode-map [remap backward-paragraph] nil)

  (define-key ealm-mode-map [remap forward-paragraph] nil)

  (define-key ealm-mode-map (kbd "<S-tab>") 'completion-at-point)
  )


;;;###autoload
(define-derived-mode ealm-mode org-mode "ealm mode"
  "Major mode for make ALM tests"


  (set (make-local-variable 'indent-line-function) 'ealm-indent-line)
  (set (make-local-variable 'indent-region-function) 'indent-region-line-by-line)
  ;; code for syntax highlighting
  (setq font-lock-defaults '((ealm-font-lock-keywords))))


;; add the mode to the `features' list
(provide 'ealm-mode)

;;; ealm-mode.el ends here
