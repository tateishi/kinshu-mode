;;; test-kinshu-format.el --- Tests for kinshu. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'kinshu-mode) ;; 既にあなたの環境で使っている前提

(ert-deftest kinshu-format-basic ()
  "日付と数値のみの基本フォーマット。"
  (should
   (equal (kinshu-format "2024-01-01 1 2 3")
          "2024-01-01   1   2   3")))

(ert-deftest kinshu-format-with-amount ()
  "amount (= ...) がある場合のフォーマット。"
  (should
   (equal (kinshu-format "2024-01-01 10 20 30 = 5000")
          "2024-01-01  10  20  30   =    5000")))

(ert-deftest kinshu-format-empty-fields ()
  "数値フィールドが少ない場合でも正しく整形される。"
  (should
   (equal (kinshu-format "2024-01-01 7")
          "2024-01-01   7")))

(ert-deftest kinshu-format-many-fields ()
  "10 個の数値フィールドを持つ行のフォーマット。"
  (should
   (equal (kinshu-format "2024-01-01 1 2 3 4 5 6 7 8 9 10")
          "2024-01-01   1   2   3   4   5   6   7   8   9  10")))

(ert-deftest kinshu-format-many-fields-with-amount ()
  "10 個の数値フィールドと合計を持つ行のフォーマット。"
  (should
   (equal (kinshu-format "2026-07-04 0 1 0 1 1 3 1 3 0 1 = 6881")
          "2026-07-04   0   1   0   1   1   3   1   3   0   1   =    6881")))

(ert-deftest kinshu-format-with-zero-and-negative ()
  "0 や負数が含まれても正しく整形される。"
  (should
   (equal (kinshu-format "2024-01-01 0 -1 20 = -300")
          "2024-01-01   0  -1  20   =    -300")))

;;; test-kinshu-format.el ends here
