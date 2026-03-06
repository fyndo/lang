;;;; lang.asd

(asdf:defsystem #:lang
  :description "Code for generating conlangs."
  :author "Eric Moore <eemoore@fyndo.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:iterate #:parse-number #:fare-csv #:closer-mop)
  :components ((:file "package")
               (:file "lang")
               (:file "freq")
               (:file "loanwords")
               (:file "semantics")
               (:file "grammar")
               (:file "vocabulary")))
