;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; checker.asd
;;;; license: llgpl

(asdf:defsystem #:checker
  :components ((:file "checker"))
  :name "checker"
  :version "0.0.1"
  :maintainer "Paul Nathan"
  :author "Paul Nathan"
  :licence "LLGPL"
  :description "Small and sharp unit tester"
  :long-description
  "Unit testing framework without magic. Implement
  checker:run-checker-tests and then execute it with your continuous
  integration system. For rendering, override render-checker-results
  with your own class for dispatch, then override *expected-stream*
  with an instance of your dispatching class.")
