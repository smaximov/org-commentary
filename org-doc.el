;;; org-doc.el --- generate or update conventional library headers using Org mode files -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Sergei Maximov

;; Author: Sergei Maximov <s.b.maximov@gmail.com>
;; Created: 20 Jul 2016
;; Version: 0.1.0
;; Package-Requires: ((dash "2.0") (emacs "24.4") (org "8.0"))
;; Keywords: convenience, docs, tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Table of Contents
;; ─────────────────

;; 1 org-doc
;; .. 1.1 Why?
;; .. 1.2 Getting started
;; .. 1.3 API
;; ..... 1.3.1 Command line interface
;; .. 1.4 Similar projects


;; 1 org-doc
;; ═════════

;;   `org-doc' — generate or update conventional [library headers] using
;;   Org mode files.


;;   [library headers]
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html


;; 1.1 Why?
;; ────────

;;   If you have a README file with the description of your Emacs Lisp
;;   package (which you definetely should have), you may as well want to
;;   use that file as the canonical source of the documentation of the
;;   package. However, there is another place which needs this
;;   documentation: the commentary section in your main library file; you
;;   can update it manually, but it's tedious and error prone (not to
;;   mention it's a violation of the [DRY] principle).

;;   Org mode has built-in export facilities which can be used to convert
;;   Org documents into various formats, including a simple plain text
;;   format (`ascii' backend).

;;   This package employs these facilities to generate library headers from
;;   Org files automatically; it may be used either from inside of Emacs or
;;   from the command line.


;;   [DRY] https://en.wikipedia.org/wiki/Don't_repeat_yourself


;; 1.2 Getting started
;; ───────────────────

;;   /Note/: these steps are written with assumption you're using [Cask]
;;   for project management.

;;   1. [Optional] If you have installed `org-doc' manually (the only
;;      option at the moment), create a link to `org-doc':

;;      ┌────
;;      │ $ cask link org-doc path/to/org-doc/installation
;;      └────

;;   2. Add `org-doc' to the development dependencies of your library:

;;      ┌────
;;      │ (development
;;      │  (depends-on "org-doc"))
;;      └────

;;      Fetch dependencies:

;;      ┌────
;;      │ $ cask install
;;      └────

;;   3. Put the [library header] boilerplate in your ELisp file.

;;   4. Generate /Commentary/ section of the library headers:

;;      ┌────
;;      │ $ cask exec org-doc README.org your-package.el
;;      └────

;;   5. [Optional] Generate /Change Log/ section of the library headers:

;;      ┌────
;;      │ $ cask exec org-doc --section changelog CHANGELOG.org your-package.el
;;      └────

;;   6. Commit.


;;   [Cask] https://github.com/cask/cask

;;   [library header]
;;   https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html


;; 1.3 API
;; ───────

;;   Use `M-x describe-function <NAME>' for details.

;;   • *command* `org-doc:update'

;;     Update library headers using the content of an Org document.

;;   • *function* `org-doc:export-buffer-as-string'.

;;     Export the Org document opened in the current buffer as a string.

;;   • *function* `org-doc:export-file-as-string'.

;;     Export an Org document as a string.


;; 1.3.1 Command line interface
;; ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

;;   `org-doc' provides an executable script which can be invoked like
;;   this:

;;   ┌────
;;   │ $ cask exec org-doc [OPTION]... ORG-FILE ELISP-FILE
;;   └────

;;   Run `cask exec org-doc --help' to see available options.


;; 1.4 Similar projects
;; ────────────────────

;;   • [org2elcomment] - provides an interactive function to update the
;;     commentary section of an Emacs Lisp file using the contents of an
;;     Org file opened in the current buffer.
;;   • [make-readme-markdown] - in contrast to `org-doc', this package
;;     treats an Emacs Lisp file as the canonical source of
;;     documentation. That file is used to generate `README' in the
;;     Markdown format. The package provides additional features like
;;     auto-detected badges and API documentation of public functions.


;;   [org2elcomment] https://github.com/cute-jumper/org2elcomment

;;   [make-readme-markdown] https://github.com/mgalgs/make-readme-markdown

;;; Code:

(require 'org-doc-export)
(require 'org-doc-headers)
(require 'org-doc-util)

(defconst org-doc:version "0.1.0")

(defun org-doc:update (section-name org elisp &optional ext-plist)
  "Update library headers using the content of an Org document.

SECTION-NAME is a string indicating which section of the header to update.
Valid values are defined in `org-doc::section-names'.

ORG is a name of Org document which contents will be exported.

ELISP is a name of the Emacs Lisp file which comment header will be updated.

Optional argument EXT-PLIST, when provided, is a property list
with external parameters overriding Org default settings, but
still inferior to file-local settings.

Function returns the converted content of the ORG file."
  (interactive
   (list (completing-read "Section [commentary, changelog, or history]: "
                          org-doc::section-names)
         (read-file-name "Org document: " nil nil 'confirm)
         (read-file-name "ELisp file: " nil nil 'confirm)))

  (unless (org-doc::valid-section-name? section-name)
    (error "Invalid section name: `%s'" section-name))

  (let* ((export-result
          (org-doc:export-file-as-string org ext-plist))

         ;; the buffer associated with the `elisp' file;
         ;; nil if no buffers visit that file:
         (elisp-buffer-visited? (get-file-buffer elisp))

         ;; create a new buffer if necessary
         (elisp-buffer (or elisp-buffer-visited?
                           (find-file-noselect (expand-file-name elisp)))))

    (unwind-protect
        (with-current-buffer elisp-buffer
          (org-doc::update-comment-header (org-doc::section-symbol section-name)
                                          export-result)
          (basic-save-buffer (called-interactively-p))
          export-result)

      ;; kill the buffer associated with the `elisp' file
      ;; if we created it manually.
      (unless elisp-buffer-visited?
        (kill-buffer elisp-buffer)))))

(provide 'org-doc)
;;; org-doc.el ends here
