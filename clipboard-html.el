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
(setq htmlize-output-type 'inline-css)
(setq htmlize-face-overrides
      `(region (:background "transparent")
        default (:foreground (,@ (face-attribute 'default :foreground))
                 :background "transparent")))

(defun replace-whitespace-and-newline-in-htmlize-buffer (buffer)
  "Replace whitespace and newline in BUFFER."
  (with-current-buffer buffer
    ;; Narrow buffer between <pre> tags
    (narrow-to-region (search-forward "<pre>") (search-forward "</pre>"))
    
    ;; Replace leading spaces to "&#160;" that space count.
    (goto-char (point-min))
    (while (re-search-forward "^ +" nil t)
      (let ((count (- (match-end 0) (match-beginning 0))))
        (--> (make-list count "&#160;")
             (mapconcat 'identity it)
             (replace-match it))))

    ;; newline to &#10;
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match "&#10;\n" t))

    ;; Cancel narrowing
    (widen)

    ;; remove <pre> tag
    (goto-char (point-min))
    (while (re-search-forward "<pre>" nil t)
      (replace-match "" t))
    (goto-char (point-min))
    (while (re-search-forward "</pre>" nil t)
      (replace-match "" t))

    ;; return buffer content
    (buffer-substring-no-properties (point-min) (point-max))
    ))

;;;###autoload
(defun copy-region-by-html (s e)
  "Copy region to clipboard in HTML format.
S region start point
E region end point"
  (interactive "r")
  (let* ((buffer (htmlize-region s e))
         (contents (replace-whitespace-and-newline-in-htmlize-buffer buffer))
         (process (start-process "powershell-process" "*Powershell Output*"
                                 "powershell.exe" "-Command" clipboard-exec)))
        (process-send-string process contents)
        (process-send-eof process)
        (kill-buffer buffer)))

;; end of clipboard-html.el


(provide 'clipboard-html)
;;; clipboard-html.el ends here
