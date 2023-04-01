;;; base-check.el
;;; Commentary:
;;; Code:

(defun base-check (num &optional bits non-split)
  "指定されたbit数の2進数,8進数,10進数,16進数表記を文字列で返します"
  (let ((b (or bits 64))
        (n (if (< num 0)
               (+ (lsh 1 bits) num) ;; 2の補数に変換
             num))
        (int2oct (lambda (num bits)
                   (--> (+ (/ bits 3) 1) ;; bit数に適した0パディング数を算出
                        (concat "#o%0" (number-to-string it) "o")
                        (format it num))))
        (int2hex (lambda (num bits)
                   (--> (/ bits 4) ;; bit数に適した0パディング数を算出
                        (concat "#x%0" (number-to-string it) "X")
                        (format it num))))
        (int2bin (lambda (num bits)
                   (concat "#b" 
                           (let ((str ""))
                             (dotimes (i bits str)
                               (--> (lsh num (- i)) ;; 右ビットシフト
                                    (logand 1 it) ;; 1とビット論理和をとる
                                    (concat (number-to-string it) str)
                                    (setq str it)))))))
        (int2dec (lambda (num)
                   (number-to-string num)))
        (split (lambda (str n sep drop)
                 (--> (seq-drop str drop)
                      ;; splitを後ろからあてたいのでひっくり返す
                      (seq-reverse it)
                      (seq-partition it n)
                      ;; 順番が逆なので元に戻す
                      (seq-reverse it)
                      ;; partitionした中身も元に戻す
                      (seq-map (lambda (x) (seq-reverse x)) it)
                      (string-join it sep)))))
    (if non-split (setq split (lambda (str n sep drop) str)))
    (format "\nHEX %s\nDEC %s\nOCT %s\nBIN %s\n"
            (funcall split (funcall int2hex n b) 4 " " 2)
            (funcall split (funcall int2dec n) 3 "," 0)
            (funcall split (funcall int2oct n b) 3 " " 2)
            (funcall split (funcall int2bin n b) 4 " " 2))))


(provide 'base-check)
;;; base-check.el ends here
