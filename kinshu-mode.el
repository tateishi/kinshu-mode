;;; kinshu-mode.el --- Major mode for editing kinshu files. -*- lexical-binding: t; -*-

;; Copyright (C) 2021 TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Maintainer: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, files, tools
;; URL: https://github.com/tateishi/kinshu-mode

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Major mode for editing kinshu files.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defgroup kinshu nil
  "Major mode for editing kinshu files."
  :group 'text
  :prefix "kinshu-")

(defcustom kinshu-denominations '(10000 5000 2000 1000 500 100 50 10 5 1)
  "List of denominations used for calculation.
Each element is a bill/coin value.  The order must match the columns."
  :type '(repeat integer)
  :group 'kinshu)

(defcustom kinshu-tab-stops '(0 13 17 21 25 29 33 37 41 45 49)
  "Column stops used by `kinshu-next-tab' and `kinshu-prev-tab'."
  :type '(repeat integer)
  :group 'kinshu)

(defcustom kinshu-date-format "%Y-%m-%d"
  "Date format used by `format-time-string' in `kinshu-add-line'."
  :type 'string
  :group 'kinshu)

(defcustom kinshu-line-template "%s   0   0   0   0   0   0   0   0   0   0"
  "Template for a new kinshu line.
The first %s is replaced with today's date."
  :type 'string
  :group 'kinshu)

(defcustom kinshu-sum-template "   =%8d"
  "Template appended by `kinshu-sum'."
  :type 'string
  :group 'kinshu)

(defcustom kinshu-header-template
  (concat
   "#-------------------------------------------------------------\n"
   "#kinshu    10K  5K  2K  1K 500 100  50  10   5   1         sum\n"
   "#-------------------------------------------------------------\n")
  "Header inserted by `kinshu-add-header'."
  :type 'string
  :group 'kinshu)

(defun kinshu-amount (count-list)
  "Return the total amount calculated from COUNT-LIST.

COUNT-LIST is a list of integers representing how many units of
each denomination are present.  The function multiplies each
element of COUNT-LIST with the corresponding element of
`kinshu-denominations`, then returns the sum of all products."

  (apply #'+ (cl-mapcar #'* kinshu-denominations count-list)))

(defun kinshu-parse-string (text)
  "Parse TEXT as a kinshu record and return a plist describing its fields.

The expected format of TEXT is:

    DATE NUM1 NUM2 ... NUMk [= AMOUNT]

TEXT is split by spaces or tabs.  The first token is interpreted as
the date string.  Up to ten numeric tokens are then read.  If a
literal \"=\" appears, numeric parsing stops and the token following
\"=\" is interpreted as AMOUNT.

Return a plist containing:

  :date    - the date string
  :nums    - list of numeric values
  :amount  - the numeric value after \"=\" (or nil if none)"

  (let* ((tokens (split-string text "[ \t]+" t))
         date nums amount)
    (unless tokens
      (error "No tokens"))
    (setq date (car tokens))
    (setq tokens (cdr tokens))

    (while (and tokens
                (< (length nums) 10)
                (not (string= (car tokens) "=")))
      (let ((n (string-to-number (car tokens))))
        (when (and (= n 0) (not (string-equal (car tokens) "0")))
          (error "Invalid number: %S" (car tokens)))
        (push n nums)
        (setq tokens (cdr tokens))))
    (setq nums (reverse nums))

    (when (and tokens (string= "=" (car tokens)))
      (setq tokens (cdr tokens))
      (let ((n (string-to-number (car tokens))))
        (unless (numberp n)
          (error "Invalid number after '=': %S" (car tokens)))
        (setq amount n)))
    (list :date date
          :nums nums
          :amount amount)))

(defun kinshu-render-record (record)
  "Render a kinshu record RECORD as a formatted string.

RECORD is a plist containing at least the keys :date, :nums, and
optionally :amount.  The date string is printed first, followed by
the numeric fields in :nums, each right-aligned in a 4-character
column.  If :amount is present, it is appended after an '=' and
formatted in an 8-character field.

Return the resulting string."

  (let* ((date (plist-get record :date))
         (nums (plist-get record :nums))
         (amount (plist-get record :amount))
         (nums-string (let ((out ""))
                        (dolist (n nums)
                          (setq out (concat out (format "%4d" n))))
                        out))
         output)
    (setq output (format "%s%s" date nums-string))
    (if amount
        (setq output (concat output (format "   =%8d" amount))))
    output))

(defun kinshu-scan-fields (text)
  "Scan TEXT character-by-character and return a list of field descriptors.

Each field descriptor has the form:

    (KIND START END INDEX)

where:
  KIND   – field category, one of:
           'date  : initial date field at the beginning of TEXT
           'nums  : numeric fields following the date
           'other : fields appearing after '=' or any non-numeric tail
  START  – index in TEXT where the field begins
  END    – index in TEXT where the field ends (inclusive)
  INDEX  – 1-based counter for fields within the same KIND group.
           This counter resets to 1 whenever KIND transitions
           (date → nums, nums → other).

Field splitting rules:

  • A run of spaces terminates the current field:
      - push (KIND START (i-1) INDEX)
      - START becomes the position of the first space
      - skip all consecutive spaces
      - INDEX increments
      - if KIND was 'date, switch to 'nums and reset INDEX to 1

  • '=' terminates the current field:
      - push (KIND START (i-1) INDEX)
      - START becomes the position of '='
      - INDEX increments
      - if KIND was 'nums, switch to 'other and reset INDEX to 1

  • Other characters do not change KIND; scanning continues normally.

Because START is set at the first space and consecutive spaces are skipped
before continuing, a field may consist of leading spaces followed by digits,
e.g. \"   12\" is treated as a single 'nums field.

At the end of TEXT, the final field (KIND START (len-1) INDEX) is pushed.

If TEXT is empty or contains only whitespace, return:

    ((other 0 (length TEXT) 1))

Example:
  \"2026-01-01   12  34  56=   999\"
  ⇒
  ((date 0 9 1)
   (nums 10 14 1)   ; \"   12\"
   (nums 15 18 2)   ; \"  34\"
   (nums 19 22 3)   ; \"  56\"
   (other 23 23 1)  ; \"=\"
   (other 24 29 2)) ; \"   999\""

  (let ((fields ())
        (len (length text))
        (kind 'date)
        (start 0)
        (i 0)
        (index 1))
    (if (= (length (string-trim text)) 0)
        `((other 0 ,(length text) ,index))
      (while (< i len)
        (let ((c (aref text i)))
          (cond ((eq c ?\s)
                 (push (list kind start (1- i) index) fields)
                 (setq start i)
                 (setq index (1+ index))
                 (setq kind (if (eq kind 'date)
                                (progn
                                  (setq index 1)
                                  'nums)
                              kind))
                 (while (and (< i len) (eq (aref text i) ?\s))
                   (setq i (1+ i))))
                ((eq c ?\=)
                 (push (list kind start (1- i) index) fields)
                 (setq start i)
                 (setq index (1+ index))
                 (setq kind (if (eq kind 'nums)
                                (progn
                                  (setq index 1)
                                  'other)
                              kind))))
          (setq i (1+ i))))
      (push (list kind start (1- len) index) fields)
      (reverse fields))))

(defun kinshu-element-at-offet (text offset)
  "Return the element type at OFFSET within TEXT.

OFFSET is interpreted as a column position in a rendered kinshu line.
Positions 0–9 correspond to the date field.  Positions starting at 10
are divided into 4‑character numeric columns; the function returns
'(nums INDEX) for the numeric field at that column.  If OFFSET does not
fall within either the date field or one of the numeric fields, return
'(other)."


  (if (< offset 10) '(date)
    (let* ((off (- offset 10))
           (count (truncate (/ off 4))))
      (if (< count 10) (list 'nums count)
        '(other)))))

(defun kinshu-read-counts (from)
  "Read a sequence of numbers from the current line using FROM.

Move to the beginning of the current line, then read the first
object from FROM (typically a buffer or marker).  After that,
continue reading objects on the same line while skipping spaces
and tabs.  Stop when reaching the end of the line.  Return the
collected objects as a list in their original order."

  (beginning-of-line)
  (read from)
  (let ((res ()))
    (while (not (eolp))
      (push (read from) res)
      (skip-chars-forward " \t"))
    (reverse res)))

(defun kinshu-delete-sum ()
  (end-of-line)
  (while (and (not (bolp)) (not (eq (char-after) ?=))) (backward-char))
  (when (eq (char-after) ?=)
    (skip-chars-backward " \t")
    (kill-line)))

(defun kinshu-next (from tabs)
  (let ((next-list (seq-filter (lambda (m) (> m from)) tabs)))
    (if (null next-list)
        from
      (seq-min next-list))))

(defun kinshu-prev(from tabs)
  (let ((next-list (seq-filter (lambda (m) (< m from)) tabs)))
    (if (null next-list)
        from
      (seq-max next-list))))

(defun before-number ()
  (skip-chars-backward "0-9")
  (point))

(defun after-number ()
  (skip-chars-forward "0-9")
  (point))

(defun kinshu-inc ()
  (interactive)
  (save-excursion
    (let ((ch (char-after)))
      (when (and (>= ch ?0) (<= ch ?9))
          (let* ((from (before-number))
                 (to (after-number))
                 (num (1+ (string-to-number (buffer-substring from to))))
                 (len (min (length (format "%d" num)) (- to from))))
            (delete-char (- len))
            (insert (format "%d" num)))))))

(defun kinshu-dec ()
  (interactive)
  (save-excursion
    (let ((ch (char-after)))
      (when (and (>= ch ?0) (<= ch ?9))
          (let* ((from (before-number))
                 (to (after-number))
                 (num (1- (string-to-number (buffer-substring from to))))
                 (len (max (length (format "%d" num)) (- to from))))
            (delete-char (- len))
            (insert (format "%d" num)))))))

(defun kinshu-sum ()
  "一行分の金種から合計金額を計算する"
  (interactive)
  (save-excursion
    (kinshu-delete-sum)
    (end-of-line)
    (let ((amount (kinshu-amount (kinshu-read-counts (current-buffer)))))
      (insert (format kinshu-sum-template amount)))))

(defun kinshu-sum-region (min max)
  "一行分の金種からの合計金額計算をリージョンに対して行う"
  (interactive "r")
  (save-excursion
    (goto-char min)
    (while (and (not (eobp)) (< (point) max))
      (kinshu-sum)
      (forward-line))))

(defun kinshu-add-header ()
  "金種計算のヘッダーを追加する"
  (interactive)
  (goto-char (point-max))
  (insert kinshu-header-template))

(defun kinshu-add-line ()
  "金種計算の行を追加する"
  (interactive)
  (goto-char (point-max))
  (let ((today (format-time-string kinshu-date-format)))
    (insert (format kinshu-line-template today)))
  (beginning-of-line))

(defun kinshu-next-tab ()
  "金種のフォーマットに合わせてカーソルを右に移動する"
  (interactive)
  (let* ((col (current-column))
         (next (kinshu-next col kinshu-tab-stops)))
    (move-to-column next t)))

(defun kinshu-prev-tab ()
  "金種のフォーマットに合わせてカーソルを左に移動する"
  (interactive)
  (let* ((col (current-column))
         (prev (kinshu-prev col kinshu-tab-stops)))
    (move-to-column prev t)))

(defface kinshu-font-comment-face
  `((t :inherit font-lock-comment-face))
  "Default face for comments.")

(defface kinshu-font-date-face
  `((t :inherit font-lock-keyword-face))
  "Default face for date.")

(defface kinshu-font-number-face
  `((t :inherit font-lock-type-face))
  "Default face for number.")

(defface kinshu-font-sum-face
  `((t :inherit font-lock-constant-face :weight bold))
  "Default face for sum.")

(defvar kinshu-font-lock-keywords
  `(("^[#].*$" . 'kinshu-font-comment-face)
    (,(concat "\\([[:digit:]]\\{4\\}[/-][[:digit:]]\\{2\\}[/-][[:digit:]]\\{2\\}\\)"
              "[[:blank:]]+\\([[:digit:]]+\\)"
              "[[:blank:]]+\\([[:digit:]]+\\)"
              "[[:blank:]]+\\([[:digit:]]+\\)"
              "[[:blank:]]+\\([[:digit:]]+\\)"
              "[[:blank:]]+\\([[:digit:]]+\\)"
              "[[:blank:]]+\\([[:digit:]]+\\)"
              "[[:blank:]]+\\([[:digit:]]+\\)"
              "[[:blank:]]+\\([[:digit:]]+\\)"
              "[[:blank:]]+\\([[:digit:]]+\\)"
              "[[:blank:]]+\\([[:digit:]]+\\)"
              "[[:blank:]]+="
              "[[:blank:]]+\\([[:digit:]]+\\)")
     (1 'kinshu-font-date-face)
     (2 'kinshu-font-number-face)
     (3 'kinshu-font-number-face)
     (4 'kinshu-font-number-face)
     (5 'kinshu-font-number-face)
     (6 'kinshu-font-number-face)
     (7 'kinshu-font-number-face)
     (8 'kinshu-font-number-face)
     (9 'kinshu-font-number-face)
     (10 'kinshu-font-number-face)
     (11 'kinshu-font-number-face)
     (12 'kinshu-font-sum-face)))
  "Expressions for highlight in Kinshu mode.")

(defvar kinshu-mode-syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table in use in `kinshu-mode' buffers.")

(defvar kinshu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'kinshu-sum)
    (define-key map (kbd "C-c C-r") #'kinshu-sum-region)
    (define-key map (kbd "C-c C-h") #'kinshu-add-header)
    (define-key map (kbd "C-c C-j") #'kinshu-add-line)
    (define-key map (kbd "+") #'kinshu-inc)
    (define-key map (kbd "=") #'kinshu-inc)
    (define-key map (kbd "-") #'kinshu-dec)
    (define-key map (kbd "_") #'kinshu-dec)
    (define-key map (kbd "C-i") #'kinshu-next-tab)
    (define-key map (kbd "M-f") #'kinshu-next-tab)
    (define-key map (kbd "M-b") #'kinshu-prev-tab)
    map))

(autoload 'text-mode "text-mode")

(define-derived-mode kinshu-mode text-mode "Kinshu"
  "Kinshu-mode is a major mode for editing kinshu data.

\\{kinshu-mode-map}"
  :syntax-table kinshu-mode-syntax-table
  (setq font-lock-defaults '(kinshu-font-lock-keywords t nil nil nil))
  (setq-local comment-start "#"))

(provide 'kinshu-mode)

;;; kinshu-mode.el ends here
