;;; test-kinshu-extract-nums.el --- Tests for kinshu. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest kinshu-extract-nums/basic ()
  (should (equal (kinshu-extract-nums "2026-06-30 1 2 3")
                 '(1 2 3))))

(ert-deftest kinshu-extract-nums/no-nums ()
  (should-not (kinshu-extract-nums "2026-06-30 = abc")))

(ert-deftest kinshu-extract-nums/spaces ()
  (should (equal (kinshu-extract-nums "2026-06-30   10   20   30")
                 '(10 20 30))))

;;; test-kinshu-extract-nums.el ends here
