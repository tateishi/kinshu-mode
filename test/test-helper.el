;;; test-helper.el --- Helpers for kinshu-test.el
(require 'ert)

;; プロジェクトのルートから kinshu-mode.el をロード
(load-file (expand-file-name "../kinshu-mode.el"
                             (file-name-directory load-file-name)))
;;; test-helper.el ends here
