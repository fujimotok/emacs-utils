;;; window-split.el --- split 3 column window.
;;; Commentary:
;;; Code:

;;; interactive
;;;###autoload
(defun window-split-3 ()
  "Split horizontal 3 windows"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

(defun window-split-2:1 ()
  "Split horizontal 2:1"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (other-window 1)
  (delete-window))

(provide 'window-split)
;;; window-split.el ends here
