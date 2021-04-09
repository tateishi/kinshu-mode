;;; kinshu-test.el --- Tests for kinshu

(require 'kinshu-mode)
(ert-deftest amount ()
  "amount test"
  (should (equal (kinshu-amount '(1)) 10000))
  (should (equal (kinshu-amount '(1 1)) 15000))
  (should (equal (kinshu-amount '(1 1 1)) 17000))
  (should (equal (kinshu-amount '(1 1 1 1)) 18000))
  )

(ert-deftest move ()
  (should (= (kinshu-next 0 kinshu-tabs) 13))
  (should (= (kinshu-next 24 kinshu-tabs) 25))
  (should (= (kinshu-next 41 kinshu-tabs) 45))
  (should (= (kinshu-next 50 kinshu-tabs) 50))
  (should (= (kinshu-prev 0 kinshu-tabs) 0))
  (should (= (kinshu-prev 50 kinshu-tabs) 49))
  (should (= (kinshu-prev 49 kinshu-tabs) 45))
  (should (= (kinshu-prev 29 kinshu-tabs) 25))
  (should (= (kinshu-prev 80 kinshu-tabs) 49))
  )


;;; kinshu-test.el ends here
