;;; title-capitalize.el --- convert title case
;;; Commentary:
;;; Code:
(require 'dash)

(defgroup title-capitalize nil
  "convert title case")

(defcustom title-capitalize-down-case-words
  '(;; article
    "a" "an" "the"
    ;; preposition
    "and" "but" "or" "nor " "for" "yet" "so" "as" "at" "but" "by" "for" "from"
    "in" "into" "of" "off" "on" "out" "over" "than" "till" "to" "up" "with")
  "define down case words"
  :type 'list
  :group 'title-capitalize)

(defun title-capitalize--regexp ()
  (--> title-capitalize-down-case-words
       (mapconcat 'identity it "\\|") ;; Concatenate with 'OR' symbol
       (concat "\\(" it "\\)") ;; Grouping
       (concat "\\b" it "\\b") ;; Match word by word
       ))

(defun title-capitalize-string (str)
  (let ((fixed-case t) ;; Do not convert according to the case of the matched word
        (regexp (title-capitalize--regexp)))
    (--> str
        (capitalize it) ;; Upcase the first letter of each word
        (replace-regexp-in-string regexp
                                  (lambda (match) (downcase match))
                                  it
                                  fixed-case))))

;;; interactive
;;;###autoload
(defun title-capitalize-regexp (s e)
    "Convert title cace in region.
S region start point
E region end point"
  (interactive "r")
  (let ((region-string (buffer-substring-no-properties s e)))
    (delete-region s e)
    (insert (title-capitalize-string region-string))))

(provide 'title-capitalize)
;;; title-capitalize.el ends here
