;;; mode-line.el
;;; Commentary:
;;; Code:
(require 'all-the-icons)
(require 'flycheck)
(require 'flymake)

(defun over-propertize (string prop func)
  "Change a specific property with a function.
STRING is string object.
PROP is text property like face.
FUNC is function that returns new property value."
  (let* ((plist (text-properties-at 0 string))
         (value (funcall func (plist-get plist prop))))
    (apply 'propertize string
                   (plist-put plist prop value))))

(defconst mml-mode-name
  '(:eval
    (-> (all-the-icons--icon-info-for-buffer)
        (over-propertize 'display (lambda (x) '(raise -0.1))))))

(defconst mml-branch-icon
  (-> (all-the-icons-octicon "git-branch")
      (over-propertize 'display (lambda (x) '(raise -0.0)))
      (over-propertize 'face (lambda (x) (plist-put x :foreground "green")))))

(defconst mml-error-icon
  (-> (all-the-icons-faicon "times-circle")
      (over-propertize 'display (lambda (x) '(raise -0.1)))
      (over-propertize 'face (lambda (x) (plist-put x :foreground "red")))))

(defconst mml-warning-icon
  (-> (all-the-icons-faicon "exclamation-triangle")
      (over-propertize 'display (lambda (x) '(raise -0.1)))
      (over-propertize 'face (lambda (x) (plist-put x :foreground "yellow")))))

(defconst mml-info-icon
  (-> (all-the-icons-faicon "info-circle")
      (over-propertize 'display (lambda (x) '(raise -0.1)))
      (over-propertize 'face (lambda (x) (plist-put x :foreground "cyan")))))

(defun flymake-count (type)
  "Return error count number.
TYPE is :error, :warning, :note."
  (->> (flymake-diagnostics)
       (seq-count
        (lambda (x)
          (= (flymake--severity type)
             (flymake--severity (flymake-diagnostic-type x)))))
       (number-to-string)))

(defun flycheck-count (type)
  "Return error count number.
TYPE is \='error, \='warning, \='note."
  (--> (flycheck-count-errors flycheck-current-errors)
                 (alist-get type it)
                 (or it 0)
                 (number-to-string it)))

(defun mml-checker-error ()
  "Return mode line error count string."
  (cond
   (flycheck-mode (flycheck-count 'error))
   (flymake-mode (flymake-count :error))
   (t "-")))

(defun mml-checker-warning ()
  "Return mode line warning count string."
  (cond
   (flycheck-mode (flycheck-count 'warning))
   (flymake-mode (flymake-count :warning))
   (t "-")))

(defun mml-checker-info ()
  "Return mode line info count string."
  (cond
   (flycheck-mode (flycheck-count 'info))
   (flymake-mode (flymake-count :info))
   (t "-")))

(defconst mml-checker
  '(mml-error-icon
    (:eval (mml-checker-error))
    mml-warning-icon
    (:eval (mml-checker-warning))
    mml-info-icon
    (:eval (mml-checker-info))
    ))

(provide 'mode-line)
;;; mode-line.el ends here
