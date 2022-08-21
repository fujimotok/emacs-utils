;;; python-shell.el --- python shell sender
;;; Commentary:
;;; Code:
(require 'python)

;;; interactive
;;;###autoload
(defun my-python-shell-send-line ()
  "Send python shell pointd line."
  (interactive)
  (let ((string (replace-regexp-in-string
                 "^[ \t]+"
                 ""
                 (thing-at-point 'line t)))
        (process (python-shell-get-process)))
    (my-python-shell-send-string string process)))

;;;###autoload
(defun my-python-shell-send-region (s e)
  "Send python shell region.
S region start point
E region end point"
  (interactive "r")
  (let ((region-string (buffer-substring-no-properties s e))
        (process (python-shell-get-process)))
    (mapc (lambda (string)
            (my-python-shell-send-string string process)
            ;; Wait for python side standard output.
            (sleep-for 0.01))
          (split-string region-string "[\n\r]"))))

(defun my-python-shell-send-string (string process)
    "Send string to the process.
STRING substring up to newline is sent.
PROCESS process object"
    (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (car (split-string string "[\n\r]")))
    (comint-send-input)
    (goto-char (point-max))))

(provide 'python-shell)
;;; python-shell.el ends here
