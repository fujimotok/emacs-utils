;;; eglot-powershell.el --- eglot powershell setting
;;; Commentary:
;;; Code:
(require 'eglot)

(defgroup eglot-powershell nil
  "Eglot support for PowerShell, using the PowerShellEditorServices.")

(defcustom eglot-powershell-pwsh-exe (or (executable-find "pwsh") (executable-find "powershell"))
  "PowerShell executable."
  :type 'string
  :group 'eglot-powershell)

(defcustom eglot-powershell-language-server-dir "~/.emacs.d/.cache/lsp/pwsh"
  "Language server install dir."
  :type 'string
  :group 'eglot-powershell)

;;;###autoload
(defun eglot-powershell-lsp-command ()
  "Return lsp server command.
You shoud used it follows.
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     \\=`(powershell-mode . ,(eglot-powershell-lsp-command))))"
  `(,eglot-powershell-pwsh-exe
   "-NoProfile" "-NonInteractive" "-NoLogo"
   ,@(if (eq system-type 'windows-nt) '("-ExecutionPolicy" "Bypass"))
   "-OutputFormat" "Text"
   "-File"
   ,(expand-file-name "PowerShellEditorServices/Start-EditorServices.ps1" eglot-powershell-language-server-dir)
   "-HostName" "\"Emacs Host\""
   "-HostProfileId" "'Emacs.LSP'"
   "-HostVersion" "8.0.1"
   "-LogPath" ,(expand-file-name "emacs-powershell.log" eglot-powershell-language-server-dir)
   "-LogLevel" "Normal"
   "-SessionDetailsPath" ,(expand-file-name (format "PSES-VSCode-%d" (emacs-pid))
                                            eglot-powershell-language-server-dir)
   ;; "-AdditionalModules" "@('PowerShellEditorServices.VSCode')"
   "-Stdio"
   "-BundledModulesPath" ,(expand-file-name eglot-powershell-language-server-dir)
   "-FeatureFlags" "@()"))

;;; interactive
;;;###autoload
(defun eglot-powershell-install-server ()
  (interactive)
  (make-directory eglot-powershell-language-server-dir t)
  (async-shell-command (format "%s -Command \"Invoke-WebRequest %s -OutFile %s; Expand-Archive -Path %s -DestinationPath %s; Remove-Item -Path %s\""
                         eglot-powershell-pwsh-exe
                         "https://github.com/PowerShell/PowerShellEditorServices/releases/latest/download/PowerShellEditorServices.zip"
                         "PowerShellEditorServices.zip"
                         "PowerShellEditorServices.zip"
                         (expand-file-name eglot-powershell-language-server-dir)
                         "PowerShellEditorServices.zip")))

(provide 'eglot-powershell)
;;; eglot-powershell.el ends here
