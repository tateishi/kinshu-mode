;;; kinshu-scan-fields-regex.el --- Tests for kinshu. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest kinshu-scan-fields-regex/date-only ()
  (should
   (equal
    (kinshu-scan-fields-regex "2026-01-01")
    '((:date 0 10 0)))))

(ert-deftest kinshu-scan-fields-regex/date-and-nums ()
  (should
   (equal
    (kinshu-scan-fields-regex "2026-01-01   12  34")
    '((:date 0 10 0)
      (:nums 10 15 0)
      (:nums 15 19 1)))))

(ert-deftest kinshu-scan-fields-regex/date-nums-equal ()
  (should
   (equal
    (kinshu-scan-fields-regex "2026-01-01   12  34 =")
    '((:date 0 10 0)
      (:nums 10 15 0)
      (:nums 15 19 1)
      (:equal 19 21 0)))))

(ert-deftest kinshu-scan-fields-regex/full-line ()
  (should
   (equal
    (kinshu-scan-fields-regex "2026-01-01   12  34 =   999")
    '((:date 0 10 0)
      (:nums 10 15 0)
      (:nums 15 19 1)
      (:equal 19 21 0)
      (:amount 21 27 0)))))

(ert-deftest kinshu-scan-fields-regex/no-match ()
  (should
   (equal
    (kinshu-scan-fields-regex "      ")
    '())))

;;; test-kinshu-scan-fields-regex.el
