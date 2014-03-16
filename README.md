checker
---

checker. unit testing for simple people.

Unit testing in Common Lisp is not a solved problem. Existing
libraries inject magic and macroology into your testing system. This
is both fragile and can be result in unacceptable behavior. For
instance, I have experience with a particular library that does not
allow LET forms in its test forms. This isn't reasonable.

checker is a library for simple and reasonable people.

Its API isn't finalized, but should be roughly stable.


 Examples
===

Example entry point usage:

     CL-USER> (let ((checker:*verbosity* 3)) (clink:run-tests))

     Testing: (STRING= (CANONICAL-NAME HEADER) Column 0) ... succeeded
     Testing: (= (CANONICAL-INDEX HEADER) 0) ... succeeded
     Testing: (EQ (EQUALG HEADER HEADER) T) ... succeeded
     Testing: (EQUALG (FIRST HEADERS) HEADER-ONE) ... succeeded
     Testing: (EQUALG (SECOND HEADERS) HEADER-TWO) ... succeeded
     Testing: (EQUALG HEADER id) ... succeeded
     Testing: (EQUALG id HEADER) ... succeeded
     Testing: (THING-TO-HEADER-P HEADER id) ... succeeded
     Testing: (THING-TO-HEADER-P HEADER 30) ... succeeded
     Testing: (THING-TO-HEADER-P HEADER ID) ... succeeded
     Testing: (THING-TO-HEADER-P HEADER identity) ... succeeded
     Testing: (THING-TO-HEADER-P HEADER 'ID) ...
     Testing: (THING-TO-HEADER-P HEADER ID) ...
     Tests run: 13
     Tests failed: 2
     13
     2
     ((CLINK::THING-TO-HEADER-P CLINK::HEADER :ID)
     (CLINK::THING-TO-HEADER-P CLINK::HEADER 'CLINK::ID))

Note that the dynamic variable \*verbosity\* is exposed - this allows
you to control your printing to standard out.  If this doesn't suit
you, a `checker:render-checker-results` generic method is exposed,
which you can override and modify to your heart's desire.

Anyway, for the above example we have the following definitions, roughly:

      (in-package :clink)

      (defun check-headers-test ()
        ;; Can we get at the important information?
        (let ((header (make-instance 'header
                               :canonical-name "Column 0"
                               :canonical-index 0)))
         (checker:check (string= (canonical-name header)
                        "Column 0"))
         (checker:check (= (canonical-index header)
                        0)))
         ;; .. we continue on with more checks...
         )

       (defmethod checker:run-checker-tests ()
          (check-headers-test))

       (defun run-tests ()
          (checker:run-checker-tests))
