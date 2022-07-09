;;; rect-edit.el --- rectangle edit functions.
;;; Commentary:
;;; Code:
(require 'rect)
(defvar rectangle-number-line-custom-counter)

;;; interactive
;;;###autoload
(defun rectangle-number-lines-custom (format counter)
  (interactive (list (read-string "Format string: " "%d") (read-minibuffer "Counter func: " "(lambda (i) (+ i 1))")))
  (let ((rectangle-number-line-custom-counter 0))
    (apply-on-rectangle 'rectangle-number-line-custom-callback
			                  (region-beginning) (region-end) format counter)))

(defun rectangle-number-line-custom-callback (start _end format-string counter)
  (move-to-column start t)
  (insert (format format-string (funcall counter rectangle-number-line-custom-counter)))
  (setq rectangle-number-line-custom-counter
	      (1+ rectangle-number-line-custom-counter)))

(provide 'rect-edit)
;;; rect-edit.el ends here
