;;; test-kinshu-scan-fields.el --- Tests for kinshu. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest kinshu-scan-fields/basic ()
  "Scan a typical kinshu line and detect date, nums, and amount fields."
  (let* ((text "2026-01-01    1   23  456=     999")
         (fields (kinshu-scan-fields text)))
    ;; date
    (should (equal (nth 0 fields)
                   '(:date 0 10 0)))
    ;; nums
    (should (equal (nth 1 fields)
                   '(:nums 10 15 0)))   ;; "    1"
    (should (equal (nth 2 fields)
                   '(:nums 15 20 1)))   ;; "   23"
    (should (equal (nth 3 fields)
                   '(:nums 20 25 2)))   ;; "  456"
    ;; other
    (should (equal (nth 4 fields)
                   '(:other 25 26 0)))   ;; "="
    (should (equal (nth 5 fields)
                   '(:other 26 34 1))))) ;; "     999"

(ert-deftest kinshu-scan-fields/no-amount ()
  "Scan a line without an amount field."
  (let* ((text "2026-01-01   1   23  456")
         (fields (kinshu-scan-fields text)))
    (should (equal (nth 0 fields)
                   '(:date 0 10 0)))
    (should (equal (nth 1 fields)
                   '(:nums 10 14 0)))
    (should (equal (nth 2 fields)
                   '(:nums 14 19 1)))
    (should (equal (nth 3 fields)
                   '(:nums 19 24 2)))
    ;; amount がないのでフィールドは 4 個
    (should (= (length fields) 4))))

(ert-deftest kinshu-scan-fields/only-date ()
  "Scan a line containing only a date."
  (let* ((text "2026-01-01")
         (fields (kinshu-scan-fields text)))
    (should (equal fields
                   '((:date 0 10 0))))))

(ert-deftest kinshu-scan-fields/spaces-only ()
  "Scan a line with only spaces (edge case)."
  (let* ((text "      ")
         (fields (kinshu-scan-fields text)))
    ;; 全部 other として扱われるべき
    (should (equal fields '()))))

;;; test-kinshu-scan-fields.el ends here
