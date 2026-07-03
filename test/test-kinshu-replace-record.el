;;; test-kinshu-replace-amount.el --- Tests for kinshu. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest kinshu-replace-amount/replace ()
  "Existing :amount should be replaced in a new plist."
  (let* ((rec '(:date "2026-06-30" :nums (1 2 3) :amount 999))
         (new (kinshu-replace-amount rec 12345)))
    ;; 新しい plist が正しい
    (should (equal new '(:date "2026-06-30" :nums (1 2 3) :amount 12345)))
    ;; 元の plist は変更されていない
    (should (equal rec '(:date "2026-06-30" :nums (1 2 3) :amount 999)))))

(ert-deftest kinshu-replace-amount/add ()
  "If :amount is missing, it should be added in the new plist."
  (let* ((rec '(:date "2026-06-30" :nums (1 2 3)))
         (new (kinshu-replace-amount rec 777)))
    (should (equal new '(:date "2026-06-30" :nums (1 2 3) :amount 777)))
    (should (equal rec '(:date "2026-06-30" :nums (1 2 3))))))

(ert-deftest kinshu-replace-amount/non-destructive ()
  "Ensure the original plist is not modified."
  (let ((rec '(:amount 1)))
    (kinshu-replace-amount rec 2)
    ;; 元の plist は変わらない
    (should (equal rec '(:amount 1)))))

(ert-deftest kinshu-replace-date/replace ()
  "Existing :date should be replaced in a new plist."
  (let* ((rec '(:date "2026-06-30" :nums (1 2 3) :amount 999))
         (new (kinshu-replace-date rec "2026-07-01")))
    ;; 新しい plist が正しい
    (should (equal new '(:date "2026-07-01" :nums (1 2 3) :amount 999)))
    ;; 元の plist は変更されていない
    (should (equal rec '(:date "2026-06-30" :nums (1 2 3) :amount 999)))))

(ert-deftest kinshu-replace-date/add ()
  "If :date is missing, it should be added in the new plist."
  (let* ((rec '(:nums (1 2 3) :amount 999))
         (new (kinshu-replace-date rec "2026-07-01")))
    (should (equal new '(:nums (1 2 3) :amount 999 :date "2026-07-01")))
    (should (equal rec '(:nums (1 2 3) :amount 999)))))

(ert-deftest kinshu-replace-date/non-destructive ()
  "Ensure the original plist is not modified."
  (let ((rec '(:date "2026-01-01")))
    (kinshu-replace-date rec "2026-02-01")
    ;; 元の plist は変わらない
    (should (equal rec '(:date "2026-01-01")))))

(ert-deftest kinshu-replace-nth-num/basic ()
  "Nth numeric value should be replaced in a new plist."
  (let* ((rec '(:date "2026-06-30" :nums (1 2 3) :amount 999))
         (new (kinshu-replace-nth-num rec 1 99)))
    ;; 新しい plist が正しい
    (should (equal new '(:date "2026-06-30" :nums (1 99 3) :amount 999)))
    ;; 元の plist は変更されていない
    (should (equal rec '(:date "2026-06-30" :nums (1 2 3) :amount 999)))))

(ert-deftest kinshu-replace-nth-num/additional-check ()
  "Ensure :nums list is copied before modification."
  (let* ((nums '(1 2 3))
         (rec `(:nums ,nums))
         (new (kinshu-replace-nth-num rec 0 42)))
    ;; 新しい nums が正しい
    (should (equal (plist-get new :nums) '(42 2 3)))
    ;; 元の nums は変更されていない
    (should (equal nums '(1 2 3)))))

(ert-deftest kinshu-replace-nth-num/out-of-range ()
  "Out-of-range index should signal an error."
  (should-error (kinshu-replace-nth-num '(:nums (1 2 3)) 5 99)))

;;; test-kinshu-replace-amount.el ends here
