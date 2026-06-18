;;; elfeed-lib.el --- misc functions for elfeed -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>

;;; Commentary:

;; These are general functions that aren't specific to web feeds.  It's
;; a library of useful functions to Elfeed.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'compat)
(require 'cl-lib)
(require 'thingatpt)
(require 'time-date)
(require 'xml)
(require 'shr)

(defun elfeed-keyword->symbol (keyword)
  "If a keyword, convert KEYWORD into a plain symbol (remove the colon)."
  (declare (obsolete nil "4.0.0"))
  (if (keywordp keyword)
      (intern (substring (symbol-name keyword) 1))
    keyword))

(defun elfeed-strip-properties (string)
  "Return a copy of STRING with all properties removed.
If STRING is nil, returns nil."
  (declare (obsolete #'substring-no-properties "4.0.0"))
  (when string
    (substring-no-properties string)))

(defun elfeed-expose (function &rest args)
  "Return an interactive version of FUNCTION, \"exposing\" it to the user.
ARGS are passed to FUNCTION."
  (declare (obsolete nil "4.0.0"))
  (lambda () (interactive) (apply function args)))

(defun elfeed-kill-buffer ()
  "Kill the current buffer."
  (declare (obsolete #'kill-current-buffer "4.0.0"))
  (interactive)
  (kill-buffer (current-buffer)))

(defun elfeed-kill-line ()
  "Clear out the current line without touching anything else."
  (declare (obsolete #'delete-region "4.0.0"))
  (delete-region (pos-bol) (pos-eol)))

(defun elfeed-goto-line (n)
  "Like `goto-line' but for non-interactive use.
N is the destination line."
  (goto-char (point-min))
  (forward-line (1- n)))

(defun elfeed-time-duration (time &optional now)
  "Turn a TIME expression into a number of seconds.

Uses `timer-duration' but allows a bit more flair.  If NOW is non-nil,
use it as the current time (`float-time').  This is mostly useful for
testing."
  (cond
   ((numberp time) time)
   ((when-let* ((iso-time (elfeed-parse-simple-iso-8601 time)))
      (- (or now (float-time)) iso-time)))
   ((string-match-p "[[:alpha:]]" time)
    (let ((clean (replace-regexp-in-string "\\(ago\\|old\\|-\\)" " " time)))
      (when-let* ((duration (timer-duration clean)))
      ;; convert to float since float-time is used elsewhere
        (float duration))))))

(defun elfeed-looks-like-url-p (string)
  "Return non-nil if STRING looks like it could be a URL."
  (and (stringp string)
       (not (string-match-p "[ \n\t\r]" string))
       (not (null (url-type (url-generic-parse-url string))))))

(defun elfeed-compute-base (url)
  "Return the base URL for URL, useful for relative paths."
  (let ((obj (url-generic-parse-url url)))
    (setf (url-filename obj) nil)
    (setf (url-target obj) nil)
    (url-recreate-url obj)))

(defun elfeed-format-column (string width &optional align)
  "Return STRING truncated or padded to WIDTH following alignment.
ALIGN should be a keyword :left or :right."
  (if (<= width 0)
      ""
    (let ((w (string-width string)))
      (cond
       ((= w width) string)
       ((> w width) (truncate-string-to-width string width))
       ((eq align :left) (concat string (make-string (- width w) ?\s)))
       (t (concat (make-string (- width w) ?\s) string))))))

(defun elfeed-clamp (min value max)
  "Clamp VALUE between MIN and MAX."
  (min max (max min value)))

(defun elfeed-valid-regexp-p (regexp)
  "Return t if REGEXP is a valid REGEXP."
  (ignore-errors
    (prog1 t
      (string-match-p regexp ""))))

(defsubst elfeed-cleanup (name)
  "Trim trailing and leading spaces and collapse multiple spaces in NAME string."
  (string-clean-whitespace (or name "")))

(defun elfeed-parse-simple-iso-8601 (string)
  "Attempt to parse STRING as a simply formatted ISO 8601 date.
Examples: 2015-02-22, 2015-02, 20150222"
  (let* ((re (cl-flet ((re-numbers (num) (format "\\([0-9]\\{%s\\}\\)" num)))
               (format "\\`%s-?%s-?%s?\\(T%s:%s:?%s?\\)?"
                       (re-numbers 4)
                       (re-numbers 2)
                       (re-numbers 2)
                       (re-numbers 2)
                       (re-numbers 2)
                       (re-numbers 2))))
         (matches (save-match-data
                    (when (string-match re string)
                      (cl-loop for i from 1 to 7
                               collect (let ((match (match-string i string)))
                                         (and match (string-to-number match))))))))
    (when matches
      (cl-multiple-value-bind (year month day _ hour min sec) matches
        (float-time (encode-time (or sec 0) (or min 0) (or hour 0)
                                 (or day 1) month year t))))))

(defun elfeed-new-date-for-entry (old-date new-date)
  "Decide entry date, given an existing OLD-DATE (nil for new) and a NEW-DATE.
Existing entries' dates are unchanged if the new date is not
parseable.  New entries with unparseable dates default to the
current time."
  (or (elfeed-float-time new-date)
      old-date
      (float-time)))

(defun elfeed-float-time (date)
  "Like `float-time' but accept anything reasonable for DATE.
Defaults to nil if DATE could not be parsed.  Date is allowed to
be relative to now (`elfeed-time-duration')."
  (cl-typecase date
    (string
     (let ((iso-8601 (elfeed-parse-simple-iso-8601 date)))
       (if iso-8601
           iso-8601
         (let ((duration (elfeed-time-duration date)))
           (if duration
               (- (float-time) duration)
             (let ((time (ignore-errors (date-to-time date))))
               ;; check if date-to-time failed, silently or otherwise
               (unless (or (null time) (equal time '(14445 17280)))
                 (float-time time))))))))
    (integer date)
    (otherwise nil)))

(defun elfeed-xml-parse-region (&optional beg end)
  "Decode (if needed) and parse XML between BEG and END.
Uses coding system from XML encoding declaration."
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (goto-char beg)
  (when (re-search-forward
         "<\\?xml.*?encoding=[\"']\\([^\"']+\\)[\"'].*?\\?>" nil t)
    (let ((coding-system (intern-soft (downcase (match-string 1)))))
      (when (ignore-errors (check-coding-system coding-system))
        (let ((mark-beg (make-marker))
              (mark-end (make-marker)))
          ;; Region changes with encoding, so use markers to track it.
          (set-marker mark-beg beg)
          (set-marker mark-end end)
          (set-buffer-multibyte t)
          (recode-region mark-beg mark-end coding-system 'raw-text)
          (setf beg (marker-position mark-beg)
                end (marker-position mark-end))))))
  (if (and (bound-and-true-p elfeed-use-libxml) (libxml-available-p))
      (when-let* ((root (libxml-parse-xml-region beg end)))
        (list
         (if (eq (car root) 'top)
             (cl-loop for node in (cddr root)
                      if (and (consp node) (not (eq (car-safe node) 'comment)))
                      return node)
           root)))
    (let ((xml-default-ns ()))
      (xml-parse-region beg end nil nil 'symbol-qnames))))

(defun elfeed-xml-parse-file (file)
  "Parse XML from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (elfeed-xml-parse-region)))

(defun elfeed-xml-unparse (element)
  "Inverse of `elfeed-xml-parse-region', writing XML ELEMENT to the buffer."
  (cl-destructuring-bind (tag attrs . body) element
    (insert (format "<%s" tag))
    (dolist (attr attrs)
      (cl-destructuring-bind (key . value) attr
        (insert (format " %s='%s'" key (xml-escape-string value)))))
    (if (null body)
        (insert "/>")
      (insert ">")
      (dolist (sub body)
        (if (stringp sub)
            (insert (xml-escape-string sub))
          (elfeed-xml-unparse sub)))
      (insert (format "</%s>" tag)))))

(defun elfeed-slurp (file &optional literally)
  "Return the contents of FILE as a string.
If LITERALLY is non-nil return the content literally."
  (with-temp-buffer
    (if literally
        (insert-file-contents-literally file)
      (insert-file-contents file))
    (buffer-string)))

(cl-defun elfeed-spit (file string &key fsync append (encoding 'utf-8))
  "Write STRING to FILE."
  (let ((coding-system-for-write encoding)
        (write-region-inhibit-fsync (not fsync)))
    (with-temp-buffer
      (insert string)
      (write-region nil nil file append 0))))

(defvar elfeed-gzip-supported-p--cache :unknown
  "To avoid running the relatively expensive test more than once.")

(defun elfeed-gzip-supported-p ()
  "Return non-nil if `auto-compression-mode' can handle gzip."
  (if (not (eq elfeed-gzip-supported-p--cache :unknown))
      elfeed-gzip-supported-p--cache
    (setf elfeed-gzip-supported-p--cache
          (and (executable-find "gzip")
               (ignore-errors
                 (save-window-excursion
                   (let ((file (make-temp-file "gziptest" nil ".gz"))
                         (data (cl-loop for i from 32 to 3200
                                        collect i into chars
                                        finally
                                        (return (apply #'string chars)))))
                     (unwind-protect
                         (progn
                           (elfeed-spit file data)
                           (and (string= data (elfeed-slurp file))
                                (not (string= data (elfeed-slurp file t)))))
                       (delete-file file)))))))))

(defun elfeed-resize-vector (vector length)
  "Return a copy of VECTOR set to size LENGTH."
  (let ((new-vector (make-vector length nil)))
    (dotimes (i (min (length new-vector) (length vector)))
      (setf (aref new-vector i) (aref vector i)))
    new-vector))

(defun elfeed-readable-p (value)
  "Return non-nil if VALUE can be serialized."
  (condition-case _
      (prog1 t (read (prin1-to-string value)))
    (error nil)))

(defun elfeed-clipboard-get ()
  "Try to get a sensible value from the system clipboard.
It will try the `interprogram-paste-function' first and otherwise fall
back to `current-kill'."
  (when-let* ((str (or (and (functionp interprogram-paste-function)
                            (funcall interprogram-paste-function)
                       (ignore-errors (current-kill 0 :non-destructively))))))
    (substring-no-properties str)))

(defsubst elfeed-add-properties (str &rest props)
  "Add properties PROPS to STR destructively and return STR."
  (add-text-properties 0 (length str) props str)
  str)

(defun elfeed-get-link-at-point ()
  "Try to a link at point and return its URL."
  (or (get-text-property (point) 'shr-url)
      (and (fboundp 'eww-current-url)
           (funcall 'eww-current-url))
      (get-text-property (point) :nt-link)))

(defun elfeed-move-to-first-empty-line ()
  "Place point after first blank line, for use with `url-retrieve'.
If no such line exists, point is left in place."
  (let ((start (point)))
    (goto-char (point-min))
    (unless (search-forward-regexp "\r?\n\r?\n" nil t)
      (goto-char start))))

(defun elfeed-shuffle (seq)
  "Destructively shuffle SEQ."
  (let ((n (length seq)))
    (dotimes (i n)
      (cl-rotatef (elt seq i) (elt seq (+ i (cl-random (- n i))))))
    seq))

(defun elfeed-split-ranges-to-numbers (str n)
  "Convert STR containing enclosure numbers into a list of numbers.
STR is a string; N is the highest possible number in the list.
This includes expanding e.g. 3-5 into 3,4,5.  If the letter
\"a\" ('all')) is given, that is expanded to a list with numbers [1..n]."
  (let ((str-split (split-string str))
        beg end list)
    (dolist (elem str-split list)
      ;; special number "a" converts into all enclosures 1-N.
      (when (equal elem "a")
        (setf elem (concat "1-" (int-to-string n))))
      (if (string-match "\\([0-9]+\\)-\\([0-9]+\\)" elem)
          ;; we have found a range A-B, which needs converting
          ;; into the numbers A, A+1, A+2, ... B.
          (progn
            (setf beg (string-to-number (match-string 1 elem))
                  end (string-to-number (match-string 2 elem)))
            (while (<= beg end)
              (setf list (nconc list (list beg))
                    beg (1+ beg))))
        ;; else just a number
        (push (string-to-number elem) list)))))

(defun elfeed-remove-dot-segments (input)
  "Remove dots from INPUT path.
The relative URL algorithm is described in RFC 3986 §5.2.4."
  (cl-loop
   with output = ""
   for s = input
   then (cond
         ((string-match-p "\\`\\.\\./" s)
          (substring s 3))
         ((string-match-p "\\`\\./" s)
          (substring s 2))
         ((string-match-p "\\`/\\./" s)
          (substring s 2))
         ((string-match-p "\\`/\\.\\'" s) "/")
         ((string-match-p "\\`/\\.\\./" s)
          (setf output (replace-regexp-in-string "/?[^/]*\\'" "" output))
          (substring s 3))
         ((string-match-p "\\`/\\.\\.\\'" s)
          (setf output (replace-regexp-in-string "/?[^/]*\\'" "" output))
          "/")
         ((string-match-p "\\`\\.\\.?\\'" s)
          "")
         ((string-match "\\`/?[^/]*" s)
          (setf output (concat output (match-string 0 s)))
          (replace-regexp-in-string "\\`/?[^/]*" "" s)))
   until (zerop (length s))
   finally return output))

(defun elfeed-update-location (old-url new-url)
  "Return full URL for maybe-relative NEW-URL based on full OLD-URL."
  (if (null new-url)
      old-url
    (setq new-url (string-trim new-url))
    (let ((old (url-generic-parse-url old-url))
          (new (url-generic-parse-url new-url)))
      (cond
       ;; Is new URL absolute already?
       ((url-type new) new-url)
       ;; Empty is a special case (clear fragment)
       ((equal new-url "")
        (setf (url-target old) nil)
        (url-recreate-url old))
       ;; Does it start with //? Append the old protocol.
       ((url-fullness new) (concat (url-type old) ":" new-url))
       ;; Is it a relative path?
       ((not (string-match-p "\\`/" new-url))
        (let* ((old-dir (or (file-name-directory (url-filename old)) "/"))
               (concat (concat old-dir new-url))
               (new-file (elfeed-remove-dot-segments concat)))
          (setf (url-filename old) nil
                (url-target old) nil
                (url-attributes old) nil
                (url-filename old) new-file)
          (url-recreate-url old)))
       ;; Replace the relative part.
       (t
        (setf (url-filename old) (elfeed-remove-dot-segments new-url)
              (url-target old) nil
              (url-attributes old) nil)
        (url-recreate-url old))))))

(defun elfeed-url-to-namespace (url)
  "Compute an ID namespace from URL."
  (let* ((urlobj (url-generic-parse-url url))
         (host (url-host urlobj)))
    (if (= 0 (length host))
        url
      host)))

(defun elfeed-browse-url (url secondary)
  "Browse URL with `browse-url'.
If SECONDARY is non-nil, use the `browse-url-secondary-browser-function'."
  (let ((browse-url-browser-function
         (if secondary
             browse-url-secondary-browser-function
           browse-url-browser-function)))
    (message "Sent to %sbrowser: %s" (if secondary "secondary " "") url)
    (browse-url url)))

(defun elfeed-insert-html (html &optional base-url)
  "Converted HTML markup to a propertized string.
Links are relative to BASE-URL if non-nil."
  (let ((doc (if (libxml-available-p)
                 (with-temp-buffer
                   ;; insert <base> to work around libxml-parse-html-region bug
                   (when base-url
                     (insert (format "<base href=\"%s\">" base-url)))
                   (insert html)
                   (libxml-parse-html-region (point-min) (point-max) base-url))
               '(i () "Elfeed: libxml2 functionality is unavailable"))))
    (run-hook-wrapped 'elfeed-transform-html-functions
                      (lambda (hook)
                        (setq doc (funcall hook doc))
                        nil))
    ;; HACK: Ensure that inserted images are not outdated, if the buffer content
    ;; has changed in the meantime.  There should be a better solution in Emacs.
    ;; See Emacs bug#80945 and https://github.com/emacs-elfeed/elfeed/issues/550.
    (cl-letf* ((orig (symbol-function 'url-queue-retrieve))
               ((symbol-function 'url-queue-retrieve)
                (lambda (url cb &rest args)
                  (let ((cb (if (eq cb #'shr-image-fetched)
                                (lambda (status buffer start end &rest args)
                                  (when (and start end (> end start))
                                    (apply #'shr-image-fetched status buffer start end args)))
                              cb)))
                    (apply orig url cb args)))))
      (shr-insert-document doc))))

(defun elfeed-insert-link (url &optional content truncate)
  "Insert a clickable hyperlink to URL titled CONTENT.
Optionally TRUNCATE content if too wide."
  (setq content (or content url))
  (when (and truncate
             (integerp shr-width)
             (> (length content) (- shr-width 8)))
    (let ((len (- (/ shr-width 2) 10)))
      (setq content (format "%s[…]%s"
                            (substring content 0 len)
                            (substring content (- len))))))
  (shr-tag-a `(a ((href . ,url)) ,content)))

(defun elfeed--position-save (prop)
  "Save PROP value, line and column."
  (list (get-text-property (pos-bol) prop)
        (line-number-at-pos)
        (current-column)))

(defun elfeed--position-restore (prop pos)
  "Restore PROP value, line and column from saved POS."
  (pcase-let* ((`(,val ,line ,column) pos))
    (goto-char (point-min))
    (while (and val (not (eobp))
                (not (equal (get-text-property (point) prop) val)))
      (forward-line))
    (unless (and val (equal (get-text-property (point) prop) val))
      (elfeed-goto-line line))
    (move-to-column column)))

(defvar-local elfeed--position-restore-wpoint nil
  "Restore window point before redisplay.")

(defun elfeed--position-sync-wpoint ()
  "Synchronize window points after `elfeed-with-position'."
  (let ((pt (point)))
    (dolist (win (get-buffer-window-list nil nil t))
      (set-window-point win pt))
    (remove-hook 'pre-redisplay-functions
                 elfeed--position-restore-wpoint 'local)
    (setq elfeed--position-restore-wpoint
          (lambda (win)
            (remove-hook 'pre-redisplay-functions
                         elfeed--position-restore-wpoint 'local)
            (set-window-point win pt)))
    (add-hook 'pre-redisplay-functions
              elfeed--position-restore-wpoint nil 'local)))

(defun elfeed--with-position-f (prop fun)
  "See `elfeed-with-position' for PROP and FUN."
  ;; TODO: When we get the new function `markers-in' in Emacs 32 we should take
  ;; advantage of it to restore all markers. See Emacs bug#81153.
  (let* ((point-pos (elfeed--position-save prop))
         (mark-pos (cons (when-let* ((m (marker-position (mark-marker))))
                            (save-excursion
                              (goto-char m)
                              (elfeed--position-save prop)))
                          mark-active)))
    (unwind-protect
        (funcall fun)
      (elfeed--position-restore prop point-pos)
      (elfeed--position-sync-wpoint)
      (when-let* ((m (car mark-pos)))
        (setcar mark-pos (save-excursion
                           (elfeed--position-restore prop m)
                           (copy-marker (point)))))
      (save-mark-and-excursion--restore mark-pos))))

(defmacro elfeed-with-position (prop &rest body)
  "Like `save-mark-and-excursion' around BODY.
Keep line and column instead of only point.  First the line is
determined by finding a line with a matching property value for PROP.
Make sure that window points are updated properly."
  (declare (indent defun) (debug t))
  `(elfeed--with-position-f ',prop (lambda () ,@body)))

(defmacro elfeed-save-excursion (&rest body)
  "Obsolete in favor of more flexible `elfeed-with-position'.
`elfeed-with-position' takes a PROP argument in addition to BODY."
  (declare (obsolete nil "4.0.0"))
  `(elfeed-with-position elfeed-entry ,@body))

;; Keep old names to avoid breakage.
(define-obsolete-function-alias 'elfeed-directory-empty-p
  #'directory-empty-p "4.0.0")
(define-obsolete-function-alias 'elfeed-libxml-supported-p
  #'libxml-available-p "4.0.0")
(define-obsolete-function-alias 'elfeed-get-url-at-point
  #'thing-at-point-url-at-point "4.0.0")

(provide 'elfeed-lib)
;;; elfeed-lib.el ends here
