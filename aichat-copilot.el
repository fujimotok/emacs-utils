;;; aichat-copilot.el --- aichat fuctions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'aichat-bingai)
(require 'dash)
(require 'all-the-icons)

;;; mode-line feature
(defconst aichat-copilot--info-hourglass
  (-> (all-the-icons-faicon "hourglass")
      (over-propertize 'display (lambda (_) '(raise -0.1)))
      (over-propertize 'face (lambda (x) (plist-put x :foreground "light yellow")))))

(defun aichat-copilot--aichat-copilot-info ()
  "Return mode line info count string."
  (cond
   ((aichat-bingai-conversationing-p) aichat-copilot--info-hourglass)
   (t "")))

;;;###autoload
(defun aichat-copilot-set-mode-line ()
  (with-eval-after-load 'aichat-bingai
    (setq-default mode-line-format
                  (add-to-list 'mode-line-format
                               '(:eval (aichat-copilot--aichat-copilot-info))
                               t))))
;;;

;;; internal functions
(defun aichat-copilot--get-lang ()
  "Get the language name from the major mode."
  (string-remove-suffix "-mode" (symbol-name major-mode)))

(defun aichat-copilot--get-region ()
  "Get the selected region from the buffer."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun aichat-copilot--replace-placeholders (text)
  "Replace placeholders in the given text."
  (->> text
       (replace-regexp-in-string "{lang}" (aichat-copilot--get-lang))
       (replace-regexp-in-string "{sel}" (aichat-copilot--get-region))))

(defun aichat-copilot--extract-markdown-code-blocks (markdown-string)
  "Get the sentence from the markdown code block."
  (let ((regex "```[^`]*```"))
    (string-match regex markdown-string)
    (->> (match-string 0 markdown-string)
         (replace-regexp-in-string "```.*$" ""))))

(defun aichat-copilot--replace-content (content cur-buf reg-beg reg-end)
  (let ((replace-fn (lambda ()
                      (-> content
                          (aichat-copilot--extract-markdown-code-blocks)))))
    (with-current-buffer cur-buf
      (replace-region-contents reg-beg reg-end replace-fn))))

(defun aichat-copilot--output-buffer (content _ _ _)
  (switch-to-buffer-other-window "*Bing AI*")
  (markdown-mode)
  (insert content))

;; reg-begとかキャプチャできてない →レキシカルスコープが有効になってなかった
(defun aichat-copilot--send (text success-fn &optional style)
  "style:[creative, balanced, precise] "
  (let* ((cur-buf (current-buffer)) ;; 非同期で元のバッファに書きに行くために現在情報を保持
         (reg-beg (when (use-region-p) (region-beginning)))
         (reg-end (when (use-region-p) (region-end))))
    (aichat-bingai-conversation
     text
     :style style
     :on-success (lambda (msg)
                   (when-let ((content (aichat-bingai-message-type-2-text msg)))
                     (funcall success-fn content cur-buf reg-beg reg-end))
                   (aichat-bingai-conversation-reset))
     :on-error (lambda (err)
                 (message "Error: %s" err)))))

;;; interactive

(defvar aichat-copilot--gen-code-prompt
  "あなたは{lang}言語のプロフェッショナルです。
要求に応じたコードを生成してください。
分かりやすくコメント行も付けてください。
要求：
{sel}")

;;;###autoload
(defun aichat-copilot-gen-code ()
  "Generate and replace code."
  (interactive)
  (aichat-copilot--send (-> aichat-copilot--gen-code-prompt
                            (aichat-copilot--replace-placeholders))
                        #'aichat-copilot--replace-content
                        'balanced))

(defvar aichat-copilot--gen-doc-prompt
  "あなたは{lang}言語のプロフェッショナルです。
以下のコードのドキュメントを生成してください。
{sel}")

;;;###autoload
(defun aichat-copilot-gen-doc ()
  "Generate Doc."
  (interactive)
  (aichat-copilot--send (-> aichat-copilot--gen-doc-prompt
                            (aichat-copilot--replace-placeholders))
                        #'aichat-copilot--replace-content
                        'balanced))

(defvar aichat-copilot--refactoring-code-prompt
  "あなたは{lang}言語のプロフェッショナルです。
以下のコードをリファクタリングしてください。
{sel}")

;;;###autoload
(defun aichat-copilot-refactoring-code ()
  "Refactor code."
  (interactive)
  (aichat-copilot--send (-> aichat-copilot--refactoring-code-prompt
                            (aichat-copilot--replace-placeholders))
                        #'aichat-copilot--output-buffer
                        'balanced))

(defvar aichat-copilot--gen-test-prompt
  "あなたは{lang}言語のプロフェッショナルです。
以下のコードをテストするコードを生成してください。
{sel}")

;;;###autoload
(defun aichat-copilot-gen-test ()
  "Generate Doc."
  (interactive)
  (aichat-copilot--send (-> aichat-copilot--gen-test-prompt
                            (aichat-copilot--replace-placeholders))
                        #'aichat-copilot--output-buffer
                        'balanced))


(defvar aichat-copilot--review-code-prompt
  "あなたは{lang}言語のプロフェッショナルです。
以下のコードの問題点を指摘してください。
{sel}")

;;;###autoload
(defun aichat-copilot-review-code ()
  "Generate Doc."
  (interactive)
  (aichat-copilot--send (-> aichat-copilot--review-code-prompt
                            (aichat-copilot--replace-placeholders))
                        #'aichat-copilot--output-buffer
                        'balanced))

(defvar aichat-copilot--explain-code-prompt
  "あなたは{lang}言語のプロフェッショナルです。
以下のコードを説明してください。
{sel}")

;;;###autoload
(defun aichat-copilot-explain-code ()
  "Generate Doc."
  (interactive)
  (aichat-copilot--send (-> aichat-copilot--explain-code-prompt
                            (aichat-copilot--replace-placeholders))
                        #'aichat-copilot--output-buffer
                        'balanced))

(defvar aichat-copilot--bug-fix-prompt
  "あなたは{lang}言語のプロフェッショナルです。
%s
{sel}")

;;;###autoload
(defun aichat-copilot-bug-fix (issue)
  "Generate Doc."
  (interactive "sisssue:")
  (aichat-copilot--send (-> aichat-copilot--bug-fix-prompt
                            (format issue)
                            (aichat-copilot--replace-placeholders))
                        #'aichat-copilot--output-buffer
                        'balanced))

(provide 'aichat-copilot)
;;; aichat-copilot.el ends here
