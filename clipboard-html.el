;;; clipboard-html.el
;;; Commentary:
;;; Code:

(require 'dash)
(require 'htmlize)

(defvar clipboard-exec
  (->> load-file-name
       (file-name-directory)
       (expand-file-name "clipboard-html.ps1"))
  "Path to executable file providing copy to clipboard.")

(setq htmlize-use-images nil)
(setq htmlize-untabify nil)
(setq htmlize-face-overrides
      `(region (:foreground "inherit" :background "transparent")
        default (:foreground (,@ (face-attribute 'default :foreground))
                 :background "transparent")))


;;;###autoload
(defun copy-region-by-html (s e)
  "Copy region to clipboard in HTML format.
S region start point
E region end point"
  (interactive "r")
  (let* ((buffer (htmlize-region s e))
         (contents (with-current-buffer buffer
                     (buffer-substring-no-properties (point-min) (point-max))))
         (process (start-process "powershell-process" "*Powershell Output*"
                                 "powershell.exe" "-Command" clipboard-exec)))
        (process-send-string process contents)
        (process-send-eof process)
        (kill-buffer buffer)))

(provide 'clipboard-html)
;;; clipboard-html.el ends here
