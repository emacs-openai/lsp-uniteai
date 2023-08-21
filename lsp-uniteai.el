;;; lsp-uniteai.el --- LSP Clients for UniteAI  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Josh Freckleton
;; Copyright (C) 2023  Shen, JenChieh

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
  '(python-mode markdown-mode org-mode)
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

(lsp-register-client
 (make-lsp-client
  :new-connection
  (cl-case lsp-uniteai-connection-method
    (`stdio (lsp-stdio-connection "uniteai_lsp --stdio"))
    (`tcp   (lsp-tcp-connection
             (lambda (port)
               `("uniteai_lsp" "--tcp" "--lsp_port" ,(number-to-string port))))))
  :priority -2
  :major-modes lsp-uniteai-active-modes
  :server-id 'uniteai-lsp
  :add-on? t))

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
