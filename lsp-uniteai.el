;;; lsp-uniteai.el --- LSP Clients for UniteAI  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Josh Freckleton
;; Copyright (C) 2023-2024  Shen, JenChieh

;; Author: Josh Freckleton <freckletonj@gmail.com>
;;         Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: JenChieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-openai/lsp-uniteai
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (lsp-mode "6.1"))
;; Keywords: convenience ai

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; LSP Clients for UniteAI.
;;

;;; Code:

(require 'lsp-mode)

(defgroup lsp-uniteai nil
  "Settings for the UniteAI Language Server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/emacs-openai/lsp-uniteai"))

(defcustom lsp-uniteai-active-modes
  '( markdown-mode org-mode python-mode bibtex-mode clojure-mode coffee-mode
     c-mode c++-mode csharp-mode css-mode diff-mode dockerfile-mode fsharp-mode
     go-mode groovy-mode html-mode web-mode java-mode js-mode js2-mode json-mode
     LaTeX-mode less-css-mode lua-mode makefile-mode objc-mode perl-mode
     php-mode text-mode powershell-mode ess-mode ruby-mode rust-mode scss-mode
     sass-mode sh-mode sql-mode swift-mode typescript-mode TeX-mode nxml-mode
     yaml-mode sh-mode toml-mode)
  "List of major mode that work with UniteAI."
  :type 'list
  :group 'lsp-uniteai)

(defcustom lsp-uniteai-connection-method 'stdio
  "Method to connect to the UniteAI language server."
  :type '(choice (const :tag "--stdio" stdio)
                 (const :tag "--tcp" tcp))
  :group 'lsp-uniteai)

;;
;; (@* "Client" )
;;

(defun lsp-uniteai--install-server (_client callback error-callback update?)
  "Install/update UniteAI language server using `pip'.

Will invoke CALLBACK or ERROR-CALLBACK based on result.
Will update if UPDATE? is t"
  (lsp-async-start-process
   callback
   error-callback
   "pip" (if update? "install --upgrade" "install") "uniteai[all]"))

;;;###autoload
(defun lsp-uniteai-update-server ()
  "Update UniteAI Language Server.

On Windows, if the server is running, the updating will fail.
After stopping or killing the process, retry to update."
  (interactive)
  (lsp-uniteai--install-server nil #'ignore #'lsp--error t))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (cl-case lsp-uniteai-connection-method
    (`stdio (lsp-stdio-connection "uniteai_lsp --stdio"
                                  (lambda (&rest _)
                                    (zerop (shell-command "uniteai_lsp")))))
    (`tcp   (lsp-tcp-connection
             (lambda (port)
               `("uniteai_lsp" "--tcp" "--lsp_port" ,(number-to-string port))))))
  :priority -2
  :major-modes lsp-uniteai-active-modes
  :server-id 'uniteai-lsp
  :add-on? t
  :download-server-fn #'lsp-uniteai--install-server))

;;
;; (@* "Util" )
;;

(defun lsp-uniteai--range ()
  "Return the current region in LSP scope."
  (unless (region-active-p)
    (user-error "No region selected"))
  (list :start (lsp--point-to-position (region-beginning))
        :end   (lsp--point-to-position (region-end))))

;;
;; (@* "Commands" )
;;

;;
;;; Global stopping
(defun lsp-uniteai-stop ()
  "Stop the UniteAI."
  (interactive)
  (let ((doc (lsp--text-document-identifier)))
    (lsp-request "workspace/executeCommand"
                 `( :command "command.stop"
                    :arguments ,(vector doc)))))

;;
;;; Example Counter
(defun lsp-uniteai-example-counter ()
  "TODO: .."
  (interactive)
  (let ((doc (lsp--text-document-identifier))
        (pos (lsp--cur-position)))
    (lsp-request "workspace/executeCommand"
                 `( :command "command.exampleCounter"
                    :arguments ,(vector doc pos)))))

;;
;;; Document
(defun lsp-uniteai-document ()
  "TODO: .."
  (interactive)
  (let ((doc (lsp--text-document-identifier))
        (range (lsp-uniteai--range)))
    (lsp-request "workspace/executeCommand"
                 `( :command "command.document"
                    :arguments ,(vector doc range)))))

;;
;;; Local LLM
(defun lsp-uniteai-local-llm ()
  "TODO: .."
  (interactive)
  (let* ((doc (lsp--text-document-identifier))
         (range (lsp-uniteai--range)))
    (lsp-request "workspace/executeCommand"
                 `( :command "command.localLlmStream"
                    :arguments ,(vector doc range)))))

;;
;;; Transcription
(defun lsp-uniteai-transcribe ()
  "TODO: .."
  (interactive)
  (let ((doc (lsp--text-document-identifier))
        (pos (lsp--cur-position)))
    (lsp-request "workspace/executeCommand"
                 `( :command "command.transcribe"
                    :arguments ,(vector doc pos)))))

;;
;;; OpenAI
(defun lsp-uniteai-openai-gpt ()
  "TODO: .."
  (interactive)
  (let ((doc (lsp--text-document-identifier))
        (range (lsp-uniteai--range)))
    (lsp-request "workspace/executeCommand"
                 `( :command "command.openaiAutocompleteStream"
                    :arguments ,(vector doc range "FROM_CONFIG_COMPLETION" "FROM_CONFIG")))))

(defun lsp-uniteai-openai-chatgpt ()
  "TODO: .."
  (interactive)
  (let ((doc (lsp--text-document-identifier))
        (range (lsp-uniteai--range)))
    (lsp-request "workspace/executeCommand"
                 `(:command "command.openaiAutocompleteStream"
                            :arguments ,(vector doc range "FROM_CONFIG_CHAT" "FROM_CONFIG")))))

(provide 'lsp-uniteai)
;;; lsp-uniteai.el ends here
