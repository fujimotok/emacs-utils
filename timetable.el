;;; timetable.el --- timetable auto calc.
;;; Commentary:
;;; Code:

;;; interactive
;;;###autoload
(defun timetable (s e)
  "Update timetable formated 'HH:MM MM' Title in region.
before
13:00 [10] opening
13:00 [20] presentation
13:00 [10] ending

after
13:00 [10] opening
13:10 [20] presentation
13:30 [10] ending
13:40

S region start point
E region end point"
  (interactive "r")
  (let ((region-string (buffer-substring-no-properties
                        s
                        e))
        tree
        before-elem
        result
        hour
        min
        period)
    ;; 行とスペース区切りの列に分割
    (setq tree
          (mapcar
           (lambda (string)
             (split-string string " "))
           (split-string
            region-string
            "\n")))
    ;; 前の要素を参照するためmap使えない
    (while tree
      ;; 現在の行の処理をきめて保持
      (setq before-elem
            (if (not (nth 1 before-elem))
                (car tree)
              ;; 前の行がなければ、今の行をそのまま記憶
              ;; 前の行の時間と期間から、今の行の時間を求めて記憶
              (setq hour
                    (string-to-number
                     (nth 0 (split-string
                             (nth 0 before-elem)
                             ":"))))
              (setq min
                    (string-to-number
                     (nth 1 (split-string
                             (nth 0 before-elem)
                             ":"))))
              (setq period
                    (string-to-number
                     (replace-regexp-in-string
                      "[^0-9]"
                      ""
                      (nth 1 before-elem))))
              (setq hour
                    (mod
                     (+ hour (/ (+ min period) 60))
                     24))
              (setq min
                    (mod (+ min period) 60))
              (append
               (list
                (format "%02d:%02d" hour min))
               (cdr (car tree)))))
      ;; 変更後の結果を保持
      (setq result
            (append
             result
             (list before-elem)))
      ;; treeを1行進める
      (setq tree (cdr tree)))
    (delete-region s e)
    (mapcar
     (lambda (list)
       (insert
        (mapconcat #'identity list " "))
       (newline))
     result)))

(provide 'timetable)
;;; timetable.el ends here
