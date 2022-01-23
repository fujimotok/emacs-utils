;;; move.el --- point move functions.
;;; Commentary:
;;; Code:

;;; interactive
;;;###autoload
(defun move-beginning-alt ()
  "Move beginninng line-head or word-head."
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))

;;;###autoload
(defun backward-delete-word (arg)
  "Delete word not adding killring.
ARG times"
  (interactive "p")
  (delete-region
   (point)
   (progn
     (backward-word arg)
     (point))))

;;;###autoload
(defun forward-delete-char (arg)
  "Delete end of line smarter.
ARG times"
  (interactive "p")
  (if (eq (following-char) 10)
      (delete-indentation 1)
    (delete-char 1)))

;;;###autoload
(defun forward-to-symbol (arg)
  "Move forward symbol.
ARG times"
  (interactive "^p")
  (let ((cnt arg)
        (p (point)))
    (if (natnump cnt)
        (re-search-forward
         "\\(\\sw\\|\\s_\\)+"
         nil
         'move
         cnt)
      (while (< cnt 0)
        (if (re-search-backward
             "\\(\\sw\\|\\s_\\)+"
             nil
             'move)
            (skip-syntax-backward "w_"))
        (setq cnt (1+ cnt))))
    (if (eq (match-beginning 0) p)
        (re-search-forward
         "\\(\\sw\\|\\s_\\)+"
         nil
         'move
         cnt))
    (if (natnump arg)
        (goto-char (match-beginning 0)))))

;;;###autoload
(defun backward-to-symbol (arg)
  "Move backward symbol.
ARG times"
    (interactive "^p")
  (forward-to-symbol (- arg)))

(provide 'move)
;;; move.el ends here
