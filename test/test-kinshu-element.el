;;; test-kinshu-element.el --- Tests for kinshu. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest kinshu-field-contains-offset/basic ()
  (should (kinshu-field-contains-offset '(:nums 10 15 1) 10))
  (should (kinshu-field-contains-offset '(:nums 10 15 1) 12))
  (should (kinshu-field-contains-offset '(:nums 10 15 1) 14))
  (should-not (kinshu-field-contains-offset '(:nums 10 15 1) 9))
  (should-not (kinshu-field-contains-offset '(:nums 10 15 1) 15)))

(ert-deftest kinshu-element-at-offset/basic ()
  (let* ((text "2026-01-01   12  34  56=   999")
         (fields (kinshu-scan-fields text)))
    ;; date field
    (should (equal (kinshu-element-at-offset text 0)
                   (nth 0 fields)))
    (should (equal (kinshu-element-at-offset text 9)
                   (nth 0 fields)))

    ;; nums field 1 (“   12”)
    (should (equal (kinshu-element-at-offset text 10)
                   (nth 1 fields)))
    (should (equal (kinshu-element-at-offset text 14)
                   (nth 1 fields)))

    ;; '=' field
    (should (equal (kinshu-element-at-offset text 23)
                   (nth 4 fields)))

    ;; last field (“   999”)
    (should (equal (kinshu-element-at-offset text 24)
                   (nth 5 fields)))
    (should (equal (kinshu-element-at-offset text 29)
                   (nth 5 fields)))))

(ert-deftest kinshu-element-at-offset/out-of-range ()
  (should-not (kinshu-element-at-offset "123" -1))
  (should-not (kinshu-element-at-offset "123" 99)))

;;; test-kinshu-element.el ends here
