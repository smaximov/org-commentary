;;; org-doc-cli.el --- command-line interface for `org-doc' -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Sergei Maximov

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

;; See the commentary section in `org-doc.el'.

;;; Code:

(require 'org)
(require 'subr-x)

(require 'org-doc)

(defconst org-doc::cli-args-spec
  ;; ((NAME...) DEFAULT KEY OPTION-P VALIDATE HANDLER DATUM...)
  `((("-h" "--help") nil nil nil nil org-doc::usage)
    (("-v" "--version") nil nil nil nil org-doc::version)
    (("-n" "--dry-run") nil :dry-run)
    (("-q" "--silent") nil :silent)
    (("-s" "--section") "commentary" :section t ,org-doc::section-names))
  "Specification of command line arguments.

Each element has the form
    ((NAME...) DEFAULT KEY OPTION-P VALIDATOR HANDLER DATUM...)
where
    - NAME is the argument name string (e.g., \"-h\", \"--help\");
    - DEFAULT is the default value of the argument, if defined;
    - KEY (optional) is a keyword which defines the key under which
      the argument's value will be stored in the parsed arguments plist;
    - OPTION-P is a boolean indicating whether the argument
      is an option (takes value) or a flag;
    - VALIDATOR (optional) is either
          - a function to validate and transform the value;
          - a list of valid values;
          - an association list to map valid values to transformed values;
    - HANDLER (optional) is either a function to invoke or a signal to fire
      when the argument is encountered;
    - DATUM (optional) extra arguments to pass when invoking HANDLER.")

(defconst org-doc::cli-args-alist
  (-mapcat (-lambda ((names _ . rest))
             (-map (lambda (name)
                     `(,name ,@rest))
                   names))
           org-doc::cli-args-spec))

(defconst org-doc::cli-args-defaults-plist
  (let (defaults-plist)
    (-each org-doc::cli-args-spec
      (-lambda ((_ default key . _))
        (when (and default key)
          (message "default %s key %S" default key)
          (setf defaults-plist
                (plist-put defaults-plist key default)))))
    defaults-plist))

(defconst org-doc::usage
  (format "org-doc %s
Update comment headers of elisp files using Org mode documents.

Usage:
    cask emacs --batch -l org-doc-cli -f org-doc -- [OPTION]... ORG-FILE ELISP-FILE

Flags:
    -h, --help       display this message
    -v, --version    display version information
    -n, --dry-run    don't update ELISP-FILE, just display the ORG-FILE export result
    -q, --silent     don't display the ORG-FILE export result

Options:
    -s, --section [commentary]    specify which comment section to update
                                  [values: commentary, changelog, history]"
          org-doc:version))

(define-error 'org-doc::cli-argument-error
  "Invalid command-line argument" 'user-error)
(define-error 'org-doc::duplicate-argument
  "Duplicate option or flag" 'org-doc::cli-argument-error)
(define-error 'org-doc::invalid-option-value
  "Invalid option value" 'org-doc::cli-argument-error)
(define-error 'org-doc::unknown-argument
  "Unkown option or flag" 'org-doc::cli-argument-error)
(define-error 'org-doc::missing-value
  "Missing option value" 'org-doc::cli-argument-error)
(define-error 'org-doc::positional-arg-count-mismatch
  "Too few or too many positional arguments" 'org-doc::cli-argument-error)

(define-error 'org-doc::usage "")
(define-error 'org-doc::version "")

(defun org-doc::handle (handler value datum)
  "Invoke HANDLER with VALUE and extra data DATUM.

HANDLER is either a function or a name of a signal defined
with `define-error'.  If HANDLER is none of the above, raise an error."
  (cond ((get handler 'error-conditions)
         (signal handler datum))
        ((functionp handler)
         (apply handler value datum))
        (t
         (error "Invalid handler: %S" handler))))

(defun org-doc::validate (value validator)
  "Validate (and transform) VALUE using VALIDATOR.

VALIDATOR should be either
    - a function to return a transformed value
      or nil if the value is invalid;
    - a list of of valid values;
    - an association list (VALUE . TRANSFORMED-VALUE) where
      VALUE is a valid value and TRANSFORMED-VALUE is
      the value it should be mapped to.

The function returns the transformed value or nil
if VALUE is invalid."
  (cond ((functionp validator)
         (funcall validator value))
        ((listp validator)
         (-when-let (pair (assoc-string value validator))
           (if (consp pair)
               (cdr pair)
             pair)))
        (t
         (error "Invalid validator: %S" validator))))

(defun org-doc::parse-args (args)
  "Parse command line arguments ARGS.

Result is a property list
    (:org ORG-FILE
     :elisp ELISP-FILE
     :section SECTION
     :silent SILENT-P
     :dry-run DRY-RUN-P)."
  (setf args (-remove-item "--" args))
  (when (null args)
    (signal 'org-doc::usage nil))
  (let (positional args-plist)
    (while args
      (let ((argi (pop args))
            value)
        ;; normalize long options
        (when (string-match "^\\(--[^=]+\\)=" argi)
          (setf value (substring argi (match-end 0))
                argi (match-string 1 argi))

          (unless (or (null value)
                      (string-empty-p value))
            (push value args)))

        (-if-let (arg-spec (assoc argi org-doc::cli-args-alist))
            ;; handle known arguments
            (let ((key (nth 1 arg-spec))
                  (option? (nth 2 arg-spec))
                  (validator (nth 3 arg-spec))
                  (handler (nth 4 arg-spec))
                  (datum (nthcdr 5 arg-spec)))

              ;; handle duplicate arguments
              (when (plist-get args-plist key)
                (signal 'org-doc::duplicate-argument
                        (format "option `%s' defined multiple times"
                                argi)))

              ;; handle options
              (when option?
                (setf value (pop args))

                ;; missing values
                (unless value
                  (signal 'org-doc::missing-value
                          (format "option `%s' requires a value"
                                  argi)))

                ;; validation
                (when validator
                  (setf value
                        (or (org-doc::validate value validator)
                            (signal 'org-doc::invalid-option-value
                                    (format "invalid value for option `%s': %s"
                                            argi value))))))

              (when key
                (setf args-plist
                      (plist-put args-plist key (if option? value t))))

              (when handler
                (org-doc::handle handler (if option? value t) datum)))
          ;; handle unknown arguments
          (if (string-prefix-p "-" argi)
              ;; unknown options and flags
              (signal 'org-doc::unknown-argument
                      (format "unknown option or flag: `%s'" argi))

            ;; treat rest values as positional arguments
            (push argi positional)))))

    (when (/= 2 (length positional))
      (signal 'org-doc::positional-arg-count-mismatch
              (format "expected exactly 2 positional arguments; %s given"
                      (length positional))))
    (setf positional (reverse positional))

    (setf args-plist (plist-put args-plist :org (nth 0 positional)))
    (setf args-plist (plist-put args-plist :elisp (nth 1 positional)))

    (org-combine-plists org-doc::cli-args-defaults-plist args-plist)))

(defun org-doc::usage (&optional exit-code)
  "Display `org-doc' usage information and exit.

EXIT-CODE is an integer used as the exit status (defaults to 0)."
  (message "%s" org-doc::usage)
  (kill-emacs (or exit-code 0)))

(defun org-doc ()
  "Parse command line arguments and update elisp comment headers accordingly."
  (unwind-protect
      (condition-case error
          (let* ((args (org-doc::parse-args argv))
                 (export-result
                  (if (plist-get args :dry-run)
                      (org-doc:export-file-as-string (plist-get args :org))
                    (org-doc:update-file-header (plist-get args :section)
                                                (plist-get args :org)
                                                (plist-get args :elisp)))))
            (unless (plist-get args :silent)
              (message "%s" export-result)))
        (org-doc::cli-argument-error
         (message "cli-error: %s." (cdr error))
         (org-doc::usage 1))
        (org-doc::usage
         (org-doc::usage))
        (org-doc::version
         (message "org-doc %s" org-doc:version))
        (error
         (message "%s" (string-join (mapcar (lambda (elt)
                                              (format "%s" elt))
                                            error)
                                    ": "))))
    (kill-emacs)))

(provide 'org-doc-cli)
;;; org-doc-cli.el ends here
