;;; test-cli.el --- test command-line arguments parsing -*- lexical-binding: t; -*-

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

(require 'setup-tests)

(require 'dash)

(describe "Validate command line arguments"
  (it "should validate using a list of valid values"
    (expect (org-doc::validate "present" '("present" "value"))
            :to-equal "present")

    (expect (org-doc::validate "absent" '("present" "value"))
            :to-be nil))

  (it "should validate and transform using a validation function"
    (expect (org-doc::validate "value" (lambda (value)
                                         (when (string-equal value "value")
                                           "transformed value")))
            :to-equal "transformed value")

    (expect (org-doc::validate "absent" (lambda (value)
                                          (when (string-equal value "value")
                                            "transformed value")))
            :to-be nil))

  (it "should validate and transform using an association list"
    (expect (org-doc::validate "present" '(("present" . present)
                                           ("value" . value)))
            :to-be 'present)

    (expect (org-doc::validate "absent" '(("present" . present)
                                          ("value" . value)))
            :to-be nil))

  (it "should raise an error if a validator is not a function or a list"
    (expect (lambda () (org-doc::validate "value" 'not-a-list))
            :to-throw 'error)))

(describe "Parse command line arguments"
  (it "should exit immediately if `--help' or `--version' are provided"
    (-each '((("-h" "--help") . org-doc::usage)
             (("-v" "--version") . org-doc::version))
      (-lambda ((flags . signal))
        (--each flags
          (expect (lambda () (org-doc::parse-args `(,it)))
                  :to-throw signal)))))
  (it "should assume `--help' flag if no arguments are provided (sans `--')"
    (--each '(() ("--") ("--" "--"))
     (expect (lambda () (org-doc::parse-args it))
             :to-throw 'org-doc::usage)))

  (it "should assume `--section' is `commentary' by default"
    (expect (plist-get (org-doc::parse-args '("readme.org" "file.el"))
                       :section)
            :to-equal "commentary"))

  (it "should fail if `--section' is provided with invalid value"
    (--each '(("-s" "code") ("--section" "code") ("--section=code"))
      (expect (lambda () (org-doc::parse-args `("readme.org" "file.el" ,@it)))
              :to-throw 'org-doc::invalid-option-value)))

  (it "should support separating long options from values using both space and `='"
    (--each '(("--section" "history") ("--section=history"))
      (expect (plist-get (org-doc::parse-args `("readme.org" "file.el" ,@it)) :section)
              :to-equal "history")))

  (it "should fail when an option is defined multiple times"
    (expect (lambda () (org-doc::parse-args '("-s" "commentary" "readme.org"
                                              "--section" "history" "file.el")))
            :to-throw 'org-doc::duplicate-argument))

  (it "should fail when provided with unknown option or flag"
    (--each '(("-z") ("-z" "value") ("--zzz" "value") ("--zzz=value"))
      (expect (lambda () (org-doc::parse-args `("readme.org" "file.el" ,@it)))
              :to-throw 'org-doc::unknown-argument)))

  (it "should require values for options"
    (--each '("--section=" "--section" "-s")
      (expect (lambda () (org-doc::parse-args `("readme.org" "file.el" ,it)))
              :to-throw 'org-doc::missing-value)))

  (it "should fail when provided with an incorrect number of positional arguments"
    (--each '(("--section" "history" "readme.org")
              ("readme.org" "file.el" "extra-argument"))
      (expect (lambda () (org-doc::parse-args it))
              :to-throw 'org-doc::positional-arg-count-mismatch))))

(provide 'test-cli)
;;; test-cli.el ends here
