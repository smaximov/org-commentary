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
    (expect (org-doc--validate "present" '("present" "value"))
            :to-equal "present")

    (expect (org-doc--validate "absent" '("present" "value"))
            :to-be nil))

  (it "should validate and transform using a validation function"
    (let ((validator (lambda (value)
                       (when (string-equal value "value")
                         "transformed value"))))
      (expect (org-doc--validate "value" validator)
              :to-equal "transformed value")

      (expect (org-doc--validate "absent" validator)
             :to-be nil)))

  (it "should validate and transform using an association list"
    (let ((validator '(("present" . present)
                       ("value" . value))))
      (expect (org-doc--validate "present" validator)
              :to-be 'present)

      (expect (org-doc--validate "absent" validator)
              :to-be nil)))

  (it "should raise an error if a validator is not a function or a list"
    (expect (lambda () (org-doc--validate "value" 'not-a-list-or-a-function))
            :to-throw 'error)))

(describe "Handle values"
  (it "should invoke a function handler"
    (expect (org-doc--handle (lambda (value) (1+ value)) 0 nil)
            :to-equal 1))

  (it "should fire a signal handler"
    (expect (lambda () (org-doc--handle 'org-doc--version nil nil))
            :to-throw 'org-doc--version))

  (it "should fire a signal when provided with an arbitrary symbol"
    (expect (lambda () (org-doc--handle 'arbitrary-symbol nil nil))
            :not :to-throw 'arbitrary-symbol))

  (it "should throw an error when provided with an invalid handler"
    (expect (lambda () (org-doc--handle 'arbitrary-symbol nil nil))
            :to-throw 'error)))

(describe "Parse command line arguments"
  (it "should exit immediately if `--help' or `--version' are provided"
    (-each '((("-h" "--help") . org-doc--usage)
             (("-v" "--version") . org-doc--version))
      (-lambda ((flags . signal))
        (--each flags
          (expect (lambda () (org-doc--parse-args `(,it)))
                  :to-throw signal)))))

  (it "should fail when an option is defined multiple times"
    (expect (lambda () (org-doc--parse-args '("-s" "commentary" "readme.org"
                                              "--section" "history" "file.el")))
            :to-throw 'org-doc--duplicate-argument))

  (it "should fail when provided with unknown option or flag"
    (--each '(("-z") ("-z" "value") ("--zzz" "value") ("--zzz=value"))
      (expect (lambda () (org-doc--parse-args `("readme.org" "file.el" ,@it)))
              :to-throw 'org-doc--unknown-argument)))

  (it "should require values for options"
    (--each '("--section=" "--section" "-s")
      (expect (lambda () (org-doc--parse-args `("readme.org" "file.el" ,it)))
              :to-throw 'org-doc--missing-value)))

  (it "should fail when provided with an incorrect number of positional arguments"
    (--each '(("--section" "history" "readme.org")
              ("readme.org" "file.el" "extra-argument"))
      (expect (lambda () (org-doc--parse-args it))
              :to-throw 'org-doc--positional-arg-count-mismatch)))

  (it "should support separating long options from values using both space and `='"
    (--each '(("--section" "history") ("--section=history"))
      (expect (plist-get (org-doc--parse-args `("readme.org" "file.el" ,@it)) :section)
              :to-equal "history")))

  (describe "Defaults"
    (it "should assume `--help' flag if no arguments are provided (sans `--')"
      (--each '(() ("--") ("--" "--"))
        (expect (lambda () (org-doc--parse-args it))
                :to-throw 'org-doc--usage)))

    (it "should assume `--section' is \"commentary\" by default"
      (expect (plist-get (org-doc--parse-args '("readme.org" "file.el"))
                         :section)
              :to-equal "commentary"))

    (it "should assume `--charset' is `ascii' by default"
      (expect (plist-get (org-doc--parse-args '("readme.org" "file.el")) :ascii-charset)
              :to-be 'ascii))

    (it "should transform valid `--charset' values to the corresponding symbols"
      (expect (plist-get (org-doc--parse-args '("readme.org" "file.el" "--charset=utf-8"))
                         :ascii-charset)
              :to-be 'utf-8)))

  (describe "Validation"
    (it "should fail if `--section' is invalid with"
      (--each '(("-s" "code") ("--section" "code") ("--section=code"))
        (expect (lambda () (org-doc--parse-args `("readme.org" "file.el" ,@it)))
                :to-throw 'org-doc--invalid-option-value)))

    (it "should fail if `--charset' is invalid"
      (--each '(("-c" "cp1251") ("--charset" "cp1251") ("--charset=cp1251"))
        (expect (lambda () (org-doc--parse-args `("readme.org" "file.el" ,@it)))
                :to-throw 'org-doc--invalid-option-value)))))

(provide 'test-cli)
;;; test-cli.el ends here
