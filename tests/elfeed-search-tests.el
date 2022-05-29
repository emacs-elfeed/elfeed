;;; elfeed-search-tests.el --- search tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'elfeed-search)

(defmacro test-search-parse-filter-duration (filter after-days &optional before-days)
  (declare (debug (stringp numberp &optional numberp)))
  (let ((day (* 24 60 60)))
    `(should (equal ',(cl-concatenate 'list
                                      (when before-days
                                        (list :before (float (* day before-days))))
                                      (list :after (float (* day after-days))))
                    (elfeed-search-parse-filter ,filter)))))

(ert-deftest elfeed-parse-filter-time-durations ()
  (let ((test-time (encode-time 0 0 0 24 6 2019 t))
        (orig-float-time (symbol-function 'float-time)))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional time)
                 (funcall orig-float-time (or time test-time)))))
      (test-search-parse-filter-duration "@5days--3days" 5 3)
      (test-search-parse-filter-duration "@5-days-ago--3-days-ago" 5 3)
      (test-search-parse-filter-duration "@3days--5days" 5 3)
      (test-search-parse-filter-duration "@3-days-ago--5-days-ago" 5 3)
      (test-search-parse-filter-duration "@2019-06-01" 23)
      (test-search-parse-filter-duration "@2019-06-20--2019-06-01" 23 4)
      (test-search-parse-filter-duration "@2019-06-01--2019-06-20" 23 4)
      (test-search-parse-filter-duration "@2019-06-01--4-days-ago" 23 4)
      (test-search-parse-filter-duration "@4days--2019-06-01" 23 4)
      (test-search-parse-filter-duration "@4-days-ago--2019-06-01" 23 4))))

(defun run-date-filter (filter entry-datetime test-datetime)
  "Test if time FILTER passes ENTRY-DATETIME against TEST-DATETIME."
  (let* ((test-secs (elfeed-parse-simple-iso-8601 test-datetime))
         (test-time (seconds-to-time test-secs))
         (entry-secs (elfeed-parse-simple-iso-8601 entry-datetime))
         (entry-time (seconds-to-time entry-secs))
         (orig-float-time (symbol-function 'float-time))
         (entry (elfeed-entry--create
                 :title "test-entry"
                 :date (float-time entry-time))))
    (cl-letf (((symbol-function 'current-time)
               (lambda () test-time))
              ((symbol-function 'float-time)
               (lambda (&optional time)
                 (funcall orig-float-time (or time test-time)))))
      (catch 'elfeed-db-done
        (let* ((parsed (elfeed-search-parse-filter filter))
               (filter (elfeed-search-compile-filter parsed)))
          (funcall filter entry nil 0))))))

(ert-deftest elfeed-search-compile-filter ()
  (should-not (run-date-filter "@1day" "2019-06-23" "2019-06-25"))
  (should-not (run-date-filter "@1-days-ago" "2019-06-23" "2019-06-25"))
  (should     (run-date-filter "@3day" "2019-06-23" "2019-06-25"))
  (should     (run-date-filter "@3-days-ago" "2019-06-23" "2019-06-25"))
  (should-not (run-date-filter "@30day--@10day"
                               "2019-06-23" "2019-06-25"))
  (should-not (run-date-filter "@30-days-ago--10-days-ago"
                               "2019-06-23" "2019-06-25"))
  (should     (run-date-filter "@2019-06-01"
                               "2019-06-23" "2019-06-25"))
  (should-not (run-date-filter "@2019-06-01--2019-06-20"
                               "2019-06-23" "2019-06-25")))

(ert-deftest elfeed-search-unparse-filter ()
  (should (string-equal "@5mins" (elfeed-search-unparse-filter '(:after 300))))
  (should (string-equal "@5mins--1min" (elfeed-search-unparse-filter '(:after 300 :before 60)))))

(provide 'elfeed-search-tests)

;;; elfeed-search-tests.el ends here
