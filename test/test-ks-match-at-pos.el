;;; test-ks-match-at-pos.el --- Tests for kinshu. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ert)

(ert-deftest ks--match-at-pos/basic-match ()
  (should
   (ks--match-at-pos "abc" "abcXYZ" 0)))

(ert-deftest ks--match-at-pos/match-at-pos ()
  ;; "abc" は pos=1 ではマッチする
  (should
   (ks--match-at-pos "abc" "Xabc" 1)))

(ert-deftest ks--match-at-pos/match-later-but-not-at-pos ()
  ;; pos=0 ではマッチしないが、pos=1 ならマッチする
  (should-not
   (ks--match-at-pos "abc" "Xabc" 0)))

(ert-deftest ks--match-at-pos/rx-pattern ()
  ;; rx の string-start を使った例
  (should
   (ks--match-at-pos (rx string-start (+ digit)) "123abc" 0)))

(ert-deftest ks--match-at-pos/rx-no-match ()
  ;; pos=1 では string-start (+ digit) はマッチしない
  (should-not
   (ks--match-at-pos (rx string-start (+ digit)) "123abc" 1)))

;;; test-ks-match-at-pos.el ends here .
