;;; lsp-ruby-mzy-lsp.el --- lsp-mode for the Ruby ruby-lsp gem -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Šimon Lukašík

;; Author: Šimon Lukašík
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; LSP client for the Ruby ruby-lsp - an optionated language server for Ruby.
;; Not to be confused with lsp-ruby that has been deprecated for a while.

;;; Code:

(require 'lsp-mode)

(defgroup lsp-ruby-mzy-lsp nil
  "LSP support for the ruby-lsp language server."
  :group 'lsp-mode
  :link '(url-link "https://github.com/shopify/ruby-lsp"))

(defcustom lsp-ruby-mzy-lsp-use-bundler nil
  "Run ruby-lsp using bundler."
  :type 'boolean
  :safe #'booleanp
  :group 'lsp-ruby-mzy-lsp)

(defun lsp-ruby-mzy-lsp--build-command ()
  (append
   (if lsp-ruby-mzy-lsp-use-bundler '("bundle" "exec"))
   '("ruby-lsp")))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection #'lsp-ruby-mzy-lsp--build-command)
  :activation-fn (lsp-activate-on "ruby")
  :priority -2
  :server-id 'ruby-mzy-lsp-ls))

(lsp-consistency-check lsp-ruby-mzy-lsp)

(provide 'lsp-ruby-mzy-lsp)
;;; lsp-ruby-mzy-lsp.el ends here
