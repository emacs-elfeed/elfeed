;;; elfeed-db-tests.el --- database tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'url-parse)
(require 'elfeed)
(require 'elfeed-db)
(require 'elfeed-lib)
(require 'jka-compr)

(defvar elfeed-test-random-state (record 'cl--random-state -1 30 267466518)
  "Use the same random state for each run.")

(defun elfeed-random* (x)
  "Generate a random number from `elfeed-test-random-state'."
  (cl-random x elfeed-test-random-state))

(defun elfeed-test-generate-letter (&optional multibyte)
  "Generate a single character from a-z or unicode."
  (cl-flet ((control-p (char)
              (or (<= char #x001F) (and (>= char #x007F) (<= char #x009F)))))
    (if multibyte
        (cl-loop for char = (elfeed-random* (1+ #x10FF))
                 unless (control-p char) return char)
      (+ ?a (elfeed-random* 26)))))

(cl-defun elfeed-test-random (n &optional (variance 1.0))
  "Generate a random integer around N, minimum of 1."
  (max 1 (floor (+ n (- (elfeed-random* (* 1.0 variance n))
                        (* variance 0.5 n))))))

(cl-defun elfeed-test-generate-word (&optional multibyte (length 6))
  "Generate a word around LENGTH letters long."
  (apply #'string
         (cl-loop repeat (elfeed-test-random length)
                  collect (elfeed-test-generate-letter multibyte))))

(cl-defun elfeed-test-generate-title (&optional multibyte (length 8))
  "Generate a title around LENGTH words long, capitalized."
  (mapconcat
   #'identity
   (cl-loop repeat (elfeed-test-random length)
            collect (elfeed-test-generate-word multibyte) into words
            finally (return (cons (capitalize (car words)) (cdr words))))
   " "))

(defun elfeed-test-generate-url ()
  "Generate a random URL."
  (let* ((tlds '(".com" ".net" ".org"))
         (tld (nth (elfeed-random* (length tlds)) tlds))
         (path (downcase (elfeed-test-generate-title nil 3))))
    (url-recreate-url
     (url-parse-make-urlobj
      "http" nil nil
      (concat (elfeed-test-generate-word nil 10) tld)
      nil
      (concat "/" (replace-regexp-in-string " " "/" path))
      nil nil :full))))

(defmacro with-elfeed-test (&rest body)
  "Run BODY with a fresh, empty database that will be destroyed on exit."
  (declare (indent defun))
  `(let* ((elfeed-db nil)
          (elfeed-db-feeds nil)
          (elfeed-db-entries nil)
          (elfeed-db-index nil)
          (elfeed-feeds nil)
          (temp-dir (make-temp-file "elfeed-test-" t))
          (elfeed-db-directory temp-dir)
          (elfeed-new-entry-hook nil)
          (elfeed-db-update-hook nil)
          (elfeed-initial-tags '(unread)))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir :recursive))))

(defun elfeed-test-generate-feed ()
  "Generate a random feed. Warning: run this in `with-elfeed-test'."
  (let* ((url (elfeed-test-generate-url))
         (id url)
         (feed (elfeed-db-get-feed id)))
    (prog1 feed
      (push url elfeed-feeds)
      (setf (elfeed-feed-title feed) (elfeed-test-generate-title))
      (setf (elfeed-feed-url feed) url))))

(cl-defun elfeed-test-generate-date (&optional (within "1 year"))
  "Generate an epoch time within WITHIN time before now."
  (let* ((duration (elfeed-time-duration within))
         (min-time (- (float-time) duration)))
    (+ min-time (elfeed-random* duration))))

(cl-defun elfeed-test-generate-entry (feed &optional (within "1 year"))
  "Generate a random entry. Warning: run this in `with-elfeed-test'."
  (let* ((feed-id (elfeed-feed-id feed))
         (namespace (elfeed-url-to-namespace feed-id))
         (link (elfeed-test-generate-url)))
    (elfeed-entry--create
     :id (cons namespace link)
     :title (elfeed-test-generate-title)
     :link link
     :date (elfeed-test-generate-date within)
     :tags (list 'unread)
     :feed-id feed-id)))

(ert-deftest elfeed-db-size ()
  (let ((count 143))
    (with-elfeed-test
     (let ((feed (elfeed-test-generate-feed)))
       (elfeed-db-add
        (cl-loop repeat count collect (elfeed-test-generate-entry feed))))
     (should (= (elfeed-db-size) count)))))

(ert-deftest elfeed-db-delete ()
  (with-elfeed-test
   (let* ((feed (elfeed-test-generate-feed))
          (keep (cl-loop repeat 10 collect (elfeed-test-generate-entry feed)))
          (del (cl-loop repeat 10 collect (elfeed-test-generate-entry feed))))
     (elfeed-db-add keep)
     (elfeed-db-add del)
     (should (= (elfeed-db-size) 20))
     (dolist (entry (append keep del))
       (should (eq entry (elfeed-db-get-entry (elfeed-entry-id entry)))))
     (elfeed-db-delete del)
     (should (= (elfeed-db-size) 10))
     (dolist (entry keep)
       (should (eq entry (elfeed-db-get-entry (elfeed-entry-id entry)))))
     (dolist (entry del)
       (should (null (elfeed-db-get-entry (elfeed-entry-id entry))))))))

(ert-deftest elfeed-db-merge ()
  (with-elfeed-test
    (let* ((feed (elfeed-test-generate-feed))
           (entry (elfeed-test-generate-entry feed))
           (update (copy-sequence entry)))
      (should (eq (elfeed-entry-merge entry update) nil))
      (setf (elfeed-entry-title update) (elfeed-test-generate-title))
      (should (eq (elfeed-entry-merge entry update) t)))
    (let ((a (elfeed-entry--create :tags '(a b c) :meta '(:a 1 :b 2)))
          (b (elfeed-entry--create :tags '(c d) :meta '(:b 3 :c 4))))
      (elfeed-entry-merge a b)
      (should (equal (elfeed-entry-tags a) '(a b c)))
      (should (eql (plist-get (elfeed-entry-meta a) :a) 1))
      (should (eql (plist-get (elfeed-entry-meta a) :b) 3))
      (should (eql (plist-get (elfeed-entry-meta a) :c) 4)))))

(ert-deftest elfeed-db-tag ()
  (with-elfeed-test
   (let* ((feed (elfeed-test-generate-feed))
          (entry (elfeed-test-generate-entry feed))
          (tags (elfeed-normalize-tags '(foo bar baz))))
     (apply #'elfeed-tag entry tags)
     (elfeed-untag entry 'unread)
     (should (equal (elfeed-entry-tags entry) tags))
     (should (elfeed-tagged-p 'foo entry))
     (should (elfeed-tagged-p 'bar entry))
     (should (elfeed-tagged-p 'baz entry))
     (should-not (elfeed-tagged-p 'unread entry)))))

(ert-deftest elfeed-db-visit ()
  (with-elfeed-test
   (cl-loop for feed in (cl-loop repeat 8 collect (elfeed-test-generate-feed))
            do (elfeed-db-add
                (cl-loop repeat 10 collect (elfeed-test-generate-entry feed))))
   (let ((entries nil)
         (feeds nil))
     (elfeed-db-visit (entry feed)
       (push (elfeed-entry-date entry) entries)
       (cl-pushnew feed feeds :test #'equal))
     ;; All entries should have appeared.
     (should (= (length entries) 80))
     ;; All feeds should have appeared.
     (should (= (length feeds) 8))
     ;; All entries should have appeared in date order
     (should (equal (sort (copy-sequence entries) #'<) entries)))
   (let ((entries nil))
     (elfeed-db-visit (entry)
       (push (elfeed-entry-date entry) entries))
     ;; All entries should have appeared.
     (should (= (length entries) 80))
     ;; All entries should have appeared in date order
     (should (equal (sort (copy-sequence entries) #'<) entries)))))

(ert-deftest elfeed-db-dates ()
  (with-elfeed-test
   (let* ((feed (elfeed-test-generate-feed))
          (entries (cl-loop repeat 100 collect
                            (elfeed-test-generate-entry feed)))
          (updated-p nil))
     (elfeed-db-add entries)
     (add-hook 'elfeed-new-entry-hook
               (apply-partially #'error "No new entries expected!"))
     (add-hook 'elfeed-db-update-hook
               (lambda () (setf updated-p t)))
     (elfeed-db-add
      (cl-loop for entry in entries
               for update = (copy-sequence entry)
               do (setf (elfeed-entry-date update) (elfeed-test-generate-date))
               collect update))
     (should updated-p)
     (let ((collected nil)
           (sorted nil))
       (elfeed-db-visit (entry)
         (push (elfeed-entry-date entry) collected))
       (setf sorted (sort (copy-sequence collected) #'<))
       (should (equal collected sorted))))))

(ert-deftest elfeed-ref ()
  (with-elfeed-test
   (let* ((content (cl-loop repeat 25 collect (elfeed-test-generate-title t)))
          (refs (mapcar #'elfeed-ref content))
          (derefs (mapcar #'elfeed-deref refs)))
     (should (equal content derefs)))
   (let ((string "naïveté"))
     (should (string= string (elfeed-deref (elfeed-ref string)))))))

(ert-deftest elfeed-ref-pack ()
  (catch 'test-abort
    (with-elfeed-test
     (let ((jka-compr-verbose nil)
           (matcher "^[a-z0-9]\\{2\\}$")
           (feed (elfeed-test-generate-feed))
           (data (expand-file-name "data" elfeed-db-directory)))
       (unless (elfeed-gzip-supported-p)
         (message "warning: gzip auto-compression unsupported, skipping")
         (throw 'test-abort nil))
       (cl-flet ((make-entries (n)
                   (cl-loop repeat n
                            for entry = (elfeed-test-generate-entry feed)
                            do (setf (elfeed-entry-title entry)
                                     (elfeed-test-generate-title :multibyte))
                            do (setf (elfeed-entry-content entry)
                                     (elfeed-entry-title entry))
                            collect entry)))
         (let ((entries-a (make-entries 20))
               (entries-b (make-entries 20)))
           (elfeed-db-add entries-a)
           (should (directory-files data nil matcher))
           (elfeed-db-pack)
           (elfeed-db-add entries-b)
           (elfeed-db-pack)
           (elfeed-db-gc)
           (should-not (directory-files data nil matcher))
           (dolist (entry (append entries-a entries-b))
             (let ((title (elfeed-entry-title entry))
                   (content (elfeed-deref (elfeed-entry-content entry))))
               (should (string= title content))))))))))

(ert-deftest elfeed-ref-pack-metadata ()
  (catch 'test-abort
    (with-elfeed-test
     (let* ((jka-compr-verbose nil)
            (feed (elfeed-test-generate-feed))
            (entry (elfeed-test-generate-entry feed))
            ;; Keep these refs reachable only through metadata.
            (content-ref (elfeed-ref "entry content"))
            (entry-ref (elfeed-ref "entry metadata content"))
            (feed-ref (elfeed-ref "feed metadata content"))
            (check
             (lambda ()
               (let (found)
                 (elfeed-db--scan
                  (lambda (ref) (push (elfeed-ref-id ref) found)))
                 (should (equal (delete-consecutive-dups (sort found #'string<))
                                (sort (list (elfeed-ref-id content-ref)
                                            (elfeed-ref-id entry-ref)
                                            (elfeed-ref-id feed-ref)) #'string<)))
                 (should (equal "entry content"
                                (elfeed-deref (elfeed-entry-content entry))))
                 (should (equal "entry metadata content"
                                (elfeed-deref
                                 (cdr (assq 'test-ref (elfeed-meta entry :saved))))))
                 (should (equal "feed metadata content"
                                (elfeed-deref
                                 (cdr (assq 'test-ref (elfeed-meta feed :saved))))))))))
       (unless (elfeed-gzip-supported-p)
         (message "warning: gzip auto-compression unsupported, skipping")
         (throw 'test-abort nil))
       (elfeed-db-add (list entry))
       (setf (elfeed-entry-content entry) content-ref)
       ;; Use cons-based metadata so this exercises the supported scan model.
       (setf (elfeed-meta entry :saved) `((test-ref . ,entry-ref))
             (elfeed-meta feed  :saved) `((test-ref . ,feed-ref)))

       (funcall check)
       (elfeed-db-unload)

       (elfeed-db-ensure)
       (elfeed-db-pack)
       (funcall check)
       (elfeed-db-unload)

       (elfeed-db-ensure)
       (elfeed-db-gc)
       (funcall check)))))

(ert-deftest elfeed-db-scan ()
  (with-elfeed-test
   (let* ((x (elfeed-ref "x"))
          (y (elfeed-ref "y"))
          (test-list (list x 1 "foo" y :bar))
          (test-alist `((a . 0) (b . ,x) (c . ,y) (d . 1)))
          (test-dotted `(,x 1 "foo" . ,y))
          (test-nested (list test-list test-alist test-dotted))
          (refs ())
          (cb (lambda (ref) (push (elfeed-ref-id ref) refs))))
     (setq refs ())
     (elfeed-db--scan-1 cb nil)
     (elfeed-db--scan-1 cb "str")
     (elfeed-db--scan-1 cb ["vec"])
     (elfeed-db--scan-1 cb x)
     (should (equal refs (list (elfeed-ref-id x))))
     (dolist (test-obj (list test-list test-alist
                             test-dotted test-nested))
       (setq refs ())
       (elfeed-db--scan-1 cb test-obj)
       (should (equal (delete-consecutive-dups (sort refs #'string<))
                      (sort (list (elfeed-ref-id x) (elfeed-ref-id y)) #'string<)))))))

(ert-deftest elfeed-db-meta ()
  (with-elfeed-test
   (let* ((feed (elfeed-db-get-feed (elfeed-test-generate-url)))
          (entry (elfeed-test-generate-entry feed)))
     (should (null (elfeed-meta feed :status)))
     (should (null (elfeed-meta entry :rating)))
     (should (= (elfeed-meta entry :errors 10) 10))
     (setf (elfeed-meta feed :status) 'down
           (elfeed-meta entry :rating) 4)
     (incf (elfeed-meta entry :errors 0))
     (should (equal 'down (elfeed-meta feed :status)))
     (should (equal 4 (elfeed-meta entry :rating)))
     (should (= (elfeed-meta entry :errors) 1))
     (should-error (setf (elfeed-meta entry :rating) (current-buffer))))))

(ert-deftest elfeed-db-feed-entries ()
  "Test `elfeed-feed-entries'."
  (with-elfeed-test
    (cl-flet ((tsort (x) (sort (mapcar #'elfeed-entry-title x) #'string<)))
      (let* ((feed-a (elfeed-test-generate-feed))
             (feed-a-entries
              (cl-loop repeat 10 collect (elfeed-test-generate-entry feed-a)))
             (feed-b (elfeed-test-generate-feed))
             (feed-b-id (elfeed-feed-id feed-b))
             (feed-b-entries
              (cl-loop repeat 10 collect (elfeed-test-generate-entry feed-b))))
        (elfeed-db-add feed-a-entries)
        (elfeed-db-add feed-b-entries)
        ;; Fetch the entries using `elfeed-feed-entries'
        (should (equal (tsort (elfeed-feed-entries feed-a))
                       (tsort feed-a-entries)))
        (should (equal (tsort (elfeed-feed-entries feed-b-id))
                       (tsort feed-b-entries)))))))

(provide 'elfeed-db-tests)

;;; elfeed-db-tests.el ends here
