;;; test-kinshu-mode.el --- Tests for kinshu. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest kinshu-parse-string-test-1 ()
  "日付+数値10個+=+数値."
  (should
   (equal (kinshu-parse-string "2024-01-01 1 2 3 4 5 6 7 8 9 10 = 100")
          '(:date "2024-01-01"
                  :nums (1 2 3 4 5 6 7 8 9 10)
                  :amount 100))))

(ert-deftest kinshu-parse-string-test-2 ()
  "数値が10個未満（= なし）."
  (should
   (equal (kinshu-parse-string "2024-01-01 10 20 30")
          '(:date "2024-01-01"
                  :nums (10 20 30)
                  :amount nil))))

(ert-deftest kinshu-parse-string-test-3 ()
  "= が途中にあり、extra が 1 個."
  (should
   (equal (kinshu-parse-string "2024-01-01 5 6 7 = 999")
          '(:date "2024-01-01"
                  :nums (5 6 7)
                  :amount 999))))

(ert-deftest kinshu-parse-string-test-4 ()
  "数値がちょうど10個で = がない."
  (
   should
   (equal (kinshu-parse-string "2024-01-01 1 2 3 4 5 6 7 8 9 10")
          '(:date "2024-01-01"
                  :nums (1 2 3 4 5 6 7 8 9 10)
                  :amount nil))))

(ert-deftest kinshu-parse-string-test-5 ()
  "不正な数値が混入した場合はエラー."
  (should-error (kinshu-parse-string "2024-01-01 1 2 X 4")))

(ert-deftest kinshu-render-record/basic ()
  "Render a record with date and numeric fields."
  (let* ((record '(:date "2026-01-01"
                         :nums (1 23 456)
                         :amount nil))
         (result (kinshu-render-record record)))
    (should (equal result "2026-01-01   1  23 456"))))

(ert-deftest kinshu-render-record/with-amount ()
  "Render a record including an amount field."
  (let* ((record '(:date "2026-06-24"
                         :nums (1 0 0 3 0 0 1 6 0 5)
                         :amount 13115))
         (result (kinshu-render-record record)))
    (should (equal result
                   "2026-06-24   1   0   0   3   0   0   1   6   0   5   =   13115"))))

(ert-deftest kinshu-render-record/empty-nums ()
  "Render a record with no numeric fields."
  (let* ((record '(:date "2026-01-01"
                         :nums ()
                         :amount nil))
         (result (kinshu-render-record record)))
    (should (equal result "2026-01-01"))))

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
