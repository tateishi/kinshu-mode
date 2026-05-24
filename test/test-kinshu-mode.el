;;; test-kinshu-mode.el --- Tests for kinshu. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'kinshu-mode)

(ert-deftest amount ()
  "Amount test."
  (should (equal (kinshu-amount '(1)) 10000))
  (should (equal (kinshu-amount '(1 1)) 15000))
  (should (equal (kinshu-amount '(1 1 1)) 17000))
  (should (equal (kinshu-amount '(1 1 1 1)) 18000))
  )

(ert-deftest move ()
  (should (= (kinshu-next 0 kinshu-tab-stops) 13))
  (should (= (kinshu-next 24 kinshu-tab-stops) 25))
  (should (= (kinshu-next 41 kinshu-tab-stops) 45))
  (should (= (kinshu-next 50 kinshu-tab-stops) 50))
  (should (= (kinshu-prev 0 kinshu-tab-stops) 0))
  (should (= (kinshu-prev 50 kinshu-tab-stops) 49))
  (should (= (kinshu-prev 49 kinshu-tab-stops) 45))
  (should (= (kinshu-prev 29 kinshu-tab-stops) 25))
  (should (= (kinshu-prev 80 kinshu-tab-stops) 49))
  )


;;; test-kinshu-mode.el ends here
