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

API
===

#:check

`check` is the core operation of `checker`. Check simply verifies the
truthiness & non-condition-throwing-ness of its `test-form`. It has
two optional positional parameters following `test-form`: `name`, a
human readable test name and `identifier`, a machine-readable
identifier. These will be generated if not supplied.

Check will enter its results in the current `*test-group*` field of
the `*test-groups-table*`. However, the overlaying macro and CLOS
machinery should render that nearly invisible to the user.

Check should not be placed in the middle of tight loops, as it
generates a surprising amount of logic involving recording data
against dynamic variables.

#:*verbosity*

`*verbosity*` controls how spammy `check` is to
`*standard-output*`. Valid values are [0-3] on the integers.

#:run-checker-tests

`run-checker-tests` is a CLOS method whose *primary* method should be
overridden for your package's specifier. This specifier will be used
in the `*test-groups-table*` as the `*test-group*`.

#:render-checker-results

`render-checker-results` should be overridden using a specializer
bound to `*expected-stream*` if you need to write your own result
renderer/analysis tool.

#:*expected-stream*

See above.

#:with-test-group

`with-test-group` is a macro that rebinds test group name lexically
and returns total numbers of runs and successes as values.

#:*test-group*

`*test-group*` is the current key in the test-groups-table.

#:*test-groups-table*

`*test-groups-table*` is the primary entry point for all data recorded
in the current test session.


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

       (defmethod checker:run-checker-tests ((specializer (eql :clink)))
          (check-headers-test))

       (defun run-tests ()
          (checker:run-checker-tests :clink))


Observe that checker:run-checker-tests requires a specializer - this
is to distinguish your fine tests from others extant in your system.
