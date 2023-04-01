;;; powershell-shell.el --- powershell shell sender
;;; Commentary:
;;; Code:
(require 'powershell)

;;; interactive
;;;###autoload
(defun my-powershell-shell-send-line ()
  "Send powershell shell pointd line."
  (interactive)
  (let ((string (replace-regexp-in-string
                 "^[ \t]+"
                 ""
                 (thing-at-point 'line t)))
        (process (get-buffer-process "*PowerShell*")))
    (my-powershell-shell-send-string string process)))

;;;###autoload
(defun my-powershell-shell-send-region (s e)
  "Send powershell shell region.
S region start point
E region end point"
  (interactive "r")
  (let ((region-string (buffer-substring-no-properties s e))
        (process (get-buffer-process "*PowerShell*")))
    (mapc (lambda (string)
            (my-powershell-shell-send-string string process)
            ;; Wait for powershell side standard output.
            (sleep-for 0.01))
          (split-string region-string "[\n\r]"))))

(defun my-powershell-shell-send-string (string process)
    "Send string to the process.
STRING substring up to newline is sent.
PROCESS process object"
    (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (car (split-string string "[\n\r]")))
    (comint-send-input)
    (goto-char (point-max))))

(provide 'powershell-shell)
;;; powershell-shell.el ends here
