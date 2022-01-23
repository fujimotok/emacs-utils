;;; color-code.el --- color code convert hex or rgba.
;;; Commentary:
;;; Code:

;;; interactive
;;;###autoload
(defun color-hex2rgba (hex)
  "Input rrggbbaa -> r(0-255), g(0-255), b(0-255), a(0.0-1.0) .
HEX color code by HEX"
  (interactive "scolor code (#rrggbbaa): #")
  (let* ((conv (lambda (s e)
                 (string-to-number
                  (substring hex s e)
                  16)))
         (r (funcall conv 0 2))
         (g (funcall conv 2 4))
         (b (funcall conv 4 6))
         (a (/ (funcall conv 6 8) 255.0))
         (ret (format
               "rgba(%d, %d, %d, %.2f)"
               r
               g
               b
               a)))
    (if (not (called-interactively-p 'any))
        ret
      (kill-new ret)
      (message ret))))

;;;###autoload
(defun color-rgba2hex (r g b a)
  "Input r(0-255), g(0-255), b(0-255), a(0.0-1.0) -> rrggbbaa .
R 0-255
G 0-255
B 0-255
A 0.0-1.0"
  (interactive "nr(0-255): \nng(0-255): \nnb(0-255): \nna(0.0-1.0): ")
  (let ((ret (format
              "#%02X%02X%02X%02X"
              r
              g
              b
              (* a 255))))
    (if (not (called-interactively-p 'any))
        ret
      (kill-new ret)
      (message ret))))

;;; color-code.el ends here
