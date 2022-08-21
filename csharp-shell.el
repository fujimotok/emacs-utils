;;; csharp-shell.el --- csharp shell sender
;;; Commentary:
;;; Code:
(require 'comint)

(defcustom csharp-shell-interpreter
  "c:/Program Files (x86)/Microsoft Visual Studio/2017/Community/MSBuild/15.0/Bin/Roslyn/csi.exe"
  "Default CSharp interpreter for shell."
  :type 'string
  :group 'csahrp-shell)

;;;###autoload
(defun run-csharp ()
  "Start inferior buffer."
  (interactive)
  (let ((buf (get-buffer "*CSharp REPL*")))
    (if buf
        (pop-to-buffer buf)
      (make-comint "CSharp REPL" csharp-shell-interpreter)
      (switch-to-buffer-other-window "*CSharp REPL*"))))

(defun csharp-shell-get-process ()
  "Return inferior csi process for current buffer."
  (get-buffer-process (get-buffer "*CSharp REPL*")))

;;; interactive
;;;###autoload
(defun my-csharp-shell-send-line ()
  "Send csharp shell pointd line."
  (interactive)
  (let ((string (replace-regexp-in-string
                 "^[ \t]+"
                 ""
                 (thing-at-point 'line t)))
        (process (csharp-shell-get-process)))
    (my-csharp-shell-send-string string process)))

;;;###autoload
(defun my-csharp-shell-send-region (s e)
  "Send csharp shell region.
S region start point
E region end point"
  (interactive "r")
  (let ((region-string (buffer-substring-no-properties s e))
        (process (csharp-shell-get-process)))
    (mapc (lambda (string)
            (my-csharp-shell-send-string string process)
            ;; Wait for csharp side standard output.
            (sleep-for 0.01))
          (split-string region-string "[\n\r]"))))

(defun my-csharp-shell-send-string (string process)
    "Send string to the process.
STRING substring up to newline is sent.
PROCESS process object"
    (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (car (split-string string "[\n\r]")))
    (comint-send-input)
    (goto-char (point-max))))

(provide 'csharp-shell)
;;; csharp-shell.el ends here
