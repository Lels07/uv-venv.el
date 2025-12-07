;;; uv-venv.el --- uv venv manager -*- lexical-binding: t -*-

;; Copyright (C) 2025-  Lels07

;; Author: Lels07

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'project)
(require 'python)
(require 'eglot)

(defgroup uv-venv nil "Manage python venvs via uv." :group 'python)

(defcustom uv-venv-command "uv" "Executable for uv." :type 'string :group 'uv-venv)
(defcustom uv-venv-folder-name ".venv" "Default venv folder." :type 'string :group 'uv-venv)
(defcustom uv-venv-lsp-server "basedpyright" 
  "The LSP package to install (e.g. basedpyright, pyright, python-lsp-server)."
  :type 'string :group 'uv-venv)

(defvar uv-venv-current-name "" "Modeline indicator.")

;;; --- Internal Helpers ---

(defun uv-venv--get-project-root ()
  (if-let ((project (project-current))) (project-root project) default-directory))

(defun uv-venv--bin-dir (venv-path)
  (let ((bin (expand-file-name "bin" venv-path))
        (scripts (expand-file-name "Scripts" venv-path)))
    (if (file-directory-p bin) bin scripts)))

(defun uv-venv--update-modeline ()
  (if (getenv "VIRTUAL_ENV")
      (let ((venv-path (getenv "VIRTUAL_ENV")))
        (setq uv-venv-current-name 
              (propertize (format " [uv:%s]" (file-name-nondirectory venv-path))
                          'face 'font-lock-string-face
                          ;; Tooltip content:
                          'help-echo (format "Active venv: %s" venv-path)
                          ;; Visual highlight on hover:
                          'mouse-face 'mode-line-highlight)))
    (setq uv-venv-current-name ""))
  (force-mode-line-update t))

;;; --- Core Logic ---

;;;###autoload
(defun uv-venv-create (python-version)
  "Create venv via uv, then prompt to activate."
  (interactive "sPython Version (e.g. 3.12, leave empty for default): ")
  (let* ((root (uv-venv--get-project-root))
         (venv-path (expand-file-name uv-venv-folder-name root))
         (args (list "venv" venv-path)))
    
    (unless (string-empty-p python-version)
      (setq args (append args (list "--python" python-version))))
    
    (message "Creating venv...")
    (make-process
     :name "uv-create" :buffer "*uv-output*"
     :command (cons uv-venv-command args)
     :sentinel (lambda (_ event)
                 (when (string= event "finished\n")
                   (message "Venv created.")
                   (when (y-or-n-p "Activate and check for LSP? ")
                     (uv-venv-activate venv-path)))))))

;;;###autoload
(defun uv-venv-activate (&optional path)
  "Activate venv at PATH (or default) and ensure LSP is installed."
  (interactive)
  (let* ((root (uv-venv--get-project-root))
         (venv-path (expand-file-name (or path uv-venv-folder-name) root)))
    
    (unless (file-directory-p venv-path)
      (user-error "No venv found at %s" venv-path))

    (let* ((bin-dir (uv-venv--bin-dir venv-path))
           (python-exe (expand-file-name "python" bin-dir)))

      ;; 1. Set Emacs Environment
      (setq python-shell-interpreter python-exe)
      (setq python-shell-virtualenv-root venv-path)
      (setenv "VIRTUAL_ENV" venv-path)
      (setenv "PATH" (concat bin-dir path-separator (getenv "PATH")))
      (setq exec-path (cons bin-dir exec-path))
      
      (uv-venv--update-modeline)
      (message "Activated: %s" (file-name-nondirectory venv-path))

      ;; 2. Check for LSP and Install if missing
      (uv-venv--ensure-lsp venv-path))))

(defun uv-venv--ensure-lsp (venv-path)
  "Check if LSP is installed in VENV-PATH, install if missing."
  (let* ((bin-dir (uv-venv--bin-dir venv-path))
         (lsp-exe (expand-file-name uv-venv-lsp-server bin-dir)))
    
    (if (file-exists-p lsp-exe)
        (uv-venv--start-eglot)
      (when (y-or-n-p (format "LSP (%s) missing in venv. Install it now? " uv-venv-lsp-server))
        (uv-venv-install-lsp venv-path)))))

(defun uv-venv-install-lsp (venv-path)
  "Install the configured LSP server into VENV-PATH using uv pip."
  (message "Installing %s..." uv-venv-lsp-server)
  (make-process
   :name "uv-install-lsp"
   :buffer "*uv-lsp-install*"
   :command (list uv-venv-command "pip" "install" uv-venv-lsp-server "--python" venv-path)
   :sentinel (lambda (_ event)
               (if (string= event "finished\n")
                   (progn
                     (message "Installed %s. Starting Eglot..." uv-venv-lsp-server)
                     (uv-venv--start-eglot))
                 (message "LSP installation failed. Check *uv-lsp-install*.")))))

(defun uv-venv--start-eglot ()
  "Restart Eglot to pick up the new environment."
  (when (derived-mode-p 'python-mode)
    (if (fboundp 'eglot-reconnect)
        (eglot-reconnect)
      (eglot-ensure))))

;;;###autoload
(defun uv-venv-deactivate ()
  "Deactivate venv."
  (interactive)
  (setq python-shell-interpreter "python")
  (setq python-shell-virtualenv-root nil)
  (setenv "VIRTUAL_ENV" nil)
  (uv-venv--update-modeline)
  (message "Deactivated venv."))

;;;###autoload
(defun uv-venv-auto-activate ()
  "Hook to auto-activate venv if it exists in the project root."
  (let* ((root (uv-venv--get-project-root))
         (venv-path (expand-file-name uv-venv-folder-name root)))
    (when (and (file-directory-p venv-path)
               (not (string= (getenv "VIRTUAL_ENV") venv-path)))
      (uv-venv-activate venv-path))))

(provide 'uv-venv)
