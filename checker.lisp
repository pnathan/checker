;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; checker.lisp
;;;; licence: llgpl
;;;; author: paul nathan
;;;;
;;;; a simple unit test framework without magic

(defpackage :checker
  (:use :cl)
  (:export #:check
           #:*verbosity*
           #:run-checker-tests

           #:render-checker-results
           #:*expected-stream*

           #:with-test-group
           #:*test-group*
           #:*test-groups-table*))

(in-package :checker)

;;; defparameters are chosen to be dynamically overloaded at will by
;;; the end user to ensure clever and interesting capabilities.
(defparameter *verbosity* 1)
(defparameter *expected-stream* t)
(defparameter *test-group* 'default)
(defparameter *test-groups-table* (make-hash-table))

(defmacro with-test-group ((test-suite &optional (verbosity *verbosity*)) &body body)
  "Specifies the test group to execute under, along with the possible
verbosity of this lexical body. Test-suite in *test-groups-table* will
be overwritten."
  `(let ((*test-group* ,test-suite)
         (*verbosity* ,verbosity))
     (remhash ,test-suite *test-groups-table*)
     (progn
       ,@body
       (let ((results (gethash ,test-suite *test-groups-table*)))
         (values
          (length results)
          (count-if #'(lambda (result) (cdr (assoc :success result))) results))))))

(defmacro check (test-form &optional (name :default) identifier)
  "Verify that the expansion and evaluation of test-form results in
something truthy. Falsity or a condition raised will result in test
failure. Test-form will be expanded exactly once. "

  (let ((form-data (loop for element in test-form collect `',element))
        (form (gensym))
        (success (gensym))
        (identifier-sym (gensym "I")))

   `(let ((,form (list ,@form-data))
          (,identifier-sym (if ,identifier ,identifier (gensym)))
          (,success nil))

      (flet ((test-failure (form)
               (cond ((and (< *verbosity* 3)
                           (> *verbosity* 0))
                      (format t "~&Test failure: ~a~%" form))
                     ((>= *verbosity* 3)
                      (format t "failed~&"))
                     ((= *verbosity* 0)
                      t))
               (setf ,success nil))

             (test-success (form)
               (declare (ignore form))
               (if (>= *verbosity* 3)
                   (format t "succeeded~&"))
               (setf ,success t)))

        ;; logging
        (cond ((and (>= *verbosity* 1) (< *verbosity* 3))
               (format t "."))
              ((>= *verbosity* 3)
               (format t "~&Testing: ~a ... " ,form)))

        (handler-case
            (cond
              ;; actual execution of test-form
              (,test-form
               (test-success ,form))
              (t
               (test-failure ,form)))
          (error (condition)
            (test-failure ,form)
            (values nil condition)))

        ;; record execution

        (let ((results (list (cons  :id ,identifier-sym)
                             (cons :name ,name)
                             (cons :form ,form)
                             (cons :success ,success))))
          (if (gethash *test-group* *test-groups-table*)
              (push results
                   (gethash *test-group* *test-groups-table*))
              (setf (gethash *test-group* *test-groups-table*)
                    (list results))))

        (gethash *test-group* *test-groups-table*)))))

(defgeneric run-checker-tests (specializer)
  (:documentation "Overload this method for your own package. Within
  this method, call directly or indirectly the checker macro. For the
  specializer, pass in your test suite name as an specializer. The
  test results will be stored under this suite's name"))

(defmethod run-checker-tests :before (specializer)
  (format t "~&Using test group ~a~%" specializer))

(defmethod run-checker-tests :around (specializer)
  (with-test-group (specializer)
    (call-next-method)))

(defmethod run-checker-tests :after (specializer)
  (render-checker-results (gethash specializer *test-groups-table*)
                          *expected-stream*))

(defgeneric render-checker-results (result stream)
  (:documentation "Kludge of the CLOS dispatch system - create your
  own 'stream' and you too can handle the results as you see
  fit. Results is a list of alists. Keys
  are :id (identifier), :name (human readable name), :form (form
  used, and :success (the success or failure of the check)."))

(defmethod render-checker-results (result (stream t))
  (when (> *verbosity* 0)
    (values
     (format t "~&Tests run: ~a~%" (length result))
     (format t "~&Tests succeeded: ~a~%"
             (count-if #'(lambda (r) (cdr (assoc :success r))) result)))))
