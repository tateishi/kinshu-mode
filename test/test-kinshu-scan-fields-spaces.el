;;; test-kinshu-scan-fields-spaces.el --- Tests for kinshu. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest kinshu-scan-fields-spaces/date-and-nums ()
  (should
   (equal
    (kinshu-scan-fields-spaces "2026-01-01   12")
    '((:date 0 10 0)
      (:nums 10 15 0)))))

(ert-deftest kinshu-scan-fields-spaces/multiple-nums ()
  (should
   (equal
    (kinshu-scan-fields-spaces "2026-01-01   12  34  56")
    '((:date 0 10 0)
      (:nums 10 15 0)
      (:nums 15 19 1)
      (:nums 19 23 2)))))

(ert-deftest kinshu-scan-fields-spaces/equals-transition ()
  (should
   (equal
    (kinshu-scan-fields-spaces "2026-01-01   12  34  56=   999")
    '((:date 0 10 0)
      (:nums 10 15 0)
      (:nums 15 19 1)
      (:nums 19 23 2)
      (:other 23 24 0)
      (:other 24 30 1)))))

(ert-deftest kinshu-scan-fields-spaces/only-spaces ()
  (should
   (equal
    (kinshu-scan-fields-spaces "      ")
    '())))

(ert-deftest kinshu-scan-fields-spaces/empty ()
  (should
   (equal
    (kinshu-scan-fields-spaces "")
    '())))

;;; test-kinshu-scan-fields-spaces.el ends here
