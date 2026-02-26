;;; kinshu-mode.el --- Major mode for editing kinshu files. -*- lexical-binding: t; -*-

;; Copyright (C) 2021 TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Maintainer: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Url: https://github.com/tateishi/kinshu-mode/
;; Created: 2021/04/09
;; Version: 0.0.1

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

;;; Code:

(require 'cl-lib)
(require 'seq)

(defgroup kinshu nil
  "Major mode for editing kinshu files."
  :group 'text
  :prefix "kinshu-")

(defcustom kinshu-denominations '(10000 5000 2000 1000 500 100 50 10 5 1)
  "List of denominations used for calculation.
Each element is a bill/coin value. The order must match the columns."
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
  (apply #'+ (cl-mapcar #'* kinshu-denominations count-list)))

(defun kinshu-read-counts (from)
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
