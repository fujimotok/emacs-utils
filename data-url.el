;;; data-url.el --- data-url insert fuctions
;;; Commentary:
;;; Code:
(require 'mimetypes)

;;;###autoload
(defun base64-encode-file (nfile)
  "Base64 encode from file.
NFILE file path"
  (with-temp-buffer
    (insert-file-contents nfile)
    (base64-encode-string
      (encode-coding-string (buffer-string)
                            'raw-text)
      t)))

;;;###autoload
(defun conv-data-url (nfile)
  "Convert data-url.
NFILE file path"
  (let ((mimetype (mimetypes-guess-mime nfile))
        (data (base64-encode-file nfile)))
    (format "data:%s;base64,%s"
            mimetype
            data)))

;;; interactive
;;;###autoload
(defun insert-data-url (nfile)
  "Insert file which converts data-url.
NFILE file path"
  (interactive "f")
  (insert (conv-data-url nfile)))

(provide 'data-url)
;;; data-url.el ends here
