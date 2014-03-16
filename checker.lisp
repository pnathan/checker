(defpackage :checker
  (:use :cl)
  (:export #:check
           #:run-checker-tests
           #:*expected-stream*
           #:*verbosity*
           #:render-checker-results))

(in-package :checker)

(defparameter *verbosity* 1)
(defparameter *expected-stream* t)

(defparameter *tests-run* 0)
(defparameter *tests-failed* 0)
(defparameter *erroroneous-tests* nil)

(defmacro check (test-form &optional name identifier)
  "Verify that the expansion and evaluation of test-form results in
something truthy. Falsity or a condition raised will result in test
failure. Test-form will be expanded exactly once. "
  ;; TODO: restructure internals...
  (declare (ignore name)
           (ignore identifier))

  (let ((form-data (loop for element in test-form collect `',element))
        (form (gensym)))

   `(flet ((test-failure (form)
             (cond ((and (< *verbosity* 3)
                         (> *verbosity* 0))
                    (format t "~&Test failure: ~a~%" form))
                   ((> *verbosity* 3)
                    (format t "failed~&"))
                   ((= *verbosity* 0)
                    t))
             (push form *erroroneous-tests*)
             (incf *tests-failed*))

           (test-success (form)
             (declare (ignore form))
             (if (>= *verbosity* 3)
                 (format t "succeeded~&"))))


      (let ((,form (list ,@form-data)))
        (cond ((and (>= *verbosity* 1) (< *verbosity* 3))
               (format t "."))
              ((>= *verbosity* 3)
               (format t "~&Testing: ~a ... " ,form)))

        (handler-case
           (cond
             (,test-form
              (test-success ,form))
             (t
              (test-failure ,form)))
         (error (condition)
           (test-failure ,form)
           (values nil condition))))
      (incf *tests-run*)
      (values *tests-run* *tests-failed* *erroroneous-tests*))))


(defgeneric run-checker-tests ()
  :documentation "")

(defmethod run-checker-tests :before ()
  (setf *tests-run* 0)
  (setf *tests-failed* 0)
  (setf *erroroneous-tests* nil))

(defmethod run-checker-tests :after ()
  (let ((result `((:run ,*tests-run*)
                  (:failed ,*tests-failed*)
                  (:tests ,*erroroneous-tests*))))
    (render-checker-results result *expected-stream*)))

(defgeneric render-checker-results (result stream)
  (:documentation "Kludge of the CLOS dispatch system - create your
  own 'stream' and you too can handle the results as you see
  fit. Results is an alist with keys :run, :failed, and :tests. :run is number
  of tests executed. :tests are the forms which failed"))

(defmethod render-checker-results (result (stream t))
  (when (> *verbosity* 0)
    (format t "~&Tests run: ~a~%" (cadr (assoc :run result)))
    (format t "~&Tests failed: ~a~%" (cadr (assoc :failed result)))))
