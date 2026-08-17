;;;; lang.asd

(asdf:defsystem #:lang
  :description "Code for generating conlangs."
  :author "Eric Moore <eemoore@fyndo.com>"
  :license  "GPL-3.0-or-later"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:iterate #:parse-number #:fare-csv #:closer-mop)
  :components ((:file "package")
               (:file "lang")
               (:file "freq")
               (:file "loanwords")
               (:file "semantics")
               (:file "grammar")
               (:file "vocabulary")
               (:file "english"))
  :in-order-to ((asdf:test-op (asdf:test-op #:lang/tests))))

;;; The test scripts run their own assertions on load, so TEST-OP just loads
;;; them; a failure signals out of ASDF:TEST-SYSTEM.  test-deep-chain.lisp is
;;; not here — it is a manual driver with no assertions, run it by hand.

(asdf:defsystem #:lang/tests
  :description "Self-checking scripts for lang; run with (asdf:test-system :lang)."
  :depends-on (#:lang)
  :perform (asdf:test-op (o c)
             (declare (ignore o c))
             (dolist (name '("test-borrow" "test-disambiguate"))
               (load (asdf:system-relative-pathname
                      :lang (concatenate 'string name ".lisp"))))))
