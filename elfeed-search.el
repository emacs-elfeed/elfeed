;;; elfeed-search.el --- list feed entries -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>

;;; Commentary:

;; Code to list feed entries.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'hl-line)
(require 'wid-edit) ; widget-inactive face

(require 'bookmark)
(bookmark-maybe-load-default-file)

(require 'elfeed)

(defvar elfeed-search-entries ()
  "List of the entries currently on display.")

(defvar elfeed-search-filter-history nil
  "Filter history for `completing-read'.")

(defvar elfeed-search--update-timer nil
  "Timer to debounce search buffer updates.")

(defvar elfeed-search--resize-timer nil
  "Timer to debounce search window resizing.")

(defvar elfeed-search-last-update 0
  "The last time the buffer was redrawn in epoch seconds.")

(defvar elfeed-search-update-hook ()
  "List of functions to run immediately following a search buffer update.")

(defcustom elfeed-search-update-delay 0.2
  "Delay search buffer updates to avoid redundant redraws.
The delay is in seconds."
  :group 'elfeed
  :type 'number)

(defcustom elfeed-search-filter "@6months +unread"
  "Query string filtering shown entries."
  :group 'elfeed
  :type 'string)

(defcustom elfeed-search-completion t
  "Enable tag and search filter completion."
  :group 'elfeed
  :type 'boolean)

(define-obsolete-variable-alias 'elfeed-sort-order
  'elfeed-search-sort-order "3.4.2")

(defcustom elfeed-search-sort-order 'descending
  "The order in which entries should be displayed.

Changing this from the default will lead to misleading results
during live filter editing, but the results be will correct when
live filter editing is exited."
  :group 'elfeed
  :type '(choice (const descending) (const ascending)))

(defcustom elfeed-search-sort-function nil
  "Sort predicate applied to the list of entries before display.

This function must take two entries as arguments, an interface
suitable as the predicate for `sort'.

Changing this from the default will lead to misleading results
during live filter editing, but the results be will correct when
live filter editing is exited."
  :group 'elfeed
  :type '(choice function (const nil)))

(defcustom elfeed-search-remain-on-entry nil
  "When non-nil, keep point at entry after performing a command.

When nil, always move to next entry after a command.  The variable can
be set to a list of symbols show, browse, tag and yank, such that point
does not move after the listed operations.  Example:

  (setq elfeed-search-remain-on-entry \\='(browse show yank))"
  :group 'elfeed
  :type '(choice (const nil) (const t)
                 (repeat symbol)))

(defcustom elfeed-search-clipboard-type 'PRIMARY
  "Selects the clipboard `elfeed-search-yank' should use.
Choices are the symbols PRIMARY, SECONDARY, or CLIPBOARD."
  :group 'elfeed
  :type '(choice (const PRIMARY) (const SECONDARY) (const CLIPBOARD)))

(defcustom elfeed-search-date-format '("%Y-%m-%d" 10 :left)
  "The `format-time-string' format, target width, and alignment for dates.

This should be (string integer keyword) for (format width alignment).
Possible alignments are :left and :right."
  :group 'elfeed
  :type '(list string integer (choice (const :left) (const :right))))

(defcustom elfeed-search-compile-filter t
  "If non-nil, compile search filters into bytecode on the fly."
  :group 'elfeed
  :type 'boolean)

(defvar elfeed-search-filter-active nil
  "When non-nil, Elfeed is currently reading a filter from the minibuffer.
When live editing the filter, it is bound to :live.")

(defvar elfeed-search-filter-overflowing nil
  "When non-nil, the current live filter overflows the window.")

(defvar elfeed-search-header-function #'elfeed-search--header
  "Function that returns the string to be used for the Elfeed search header.")

(defvar elfeed-search-print-entry-function #'elfeed-search-print-entry--default
  "Function to print entries into the *elfeed-search* buffer.")

(defun elfeed-search-tag-all-unread ()
  "Add the `unread' tag to all selected entries."
  (interactive nil elfeed-search-mode)
  (elfeed-search-tag-all 'unread))

(defun elfeed-search-untag-all-unread ()
  "Remove the `unread' tag from all selected entries."
  (interactive nil elfeed-search-mode)
  (elfeed-search-untag-all 'unread))

(defun elfeed-search-last-entry ()
  "Place point on last entry."
  (interactive nil elfeed-search-mode)
  (goto-char (point-max))
  (forward-line -1))

(defun elfeed-search-first-entry ()
  "Place point on first entry."
  (interactive nil elfeed-search-mode)
  (goto-char (point-min)))

(defvar-keymap elfeed-search-mode-map
  :doc "Keymap for `elfeed-search-mode'."
  :parent special-mode-map
  "G" #'elfeed-search-fetch
  "RET" #'elfeed-search-show-entry
  "<mouse-1>" #'elfeed-search-click
  "<header-line> <mouse-1>" #'elfeed-search-header-click
  "s" #'elfeed-search-live-filter
  "S" #'elfeed-search-set-filter
  "c" #'elfeed-search-clear-filter
  "b" #'elfeed-search-browse-url
  "B" #'elfeed-search-browse-url-secondary
  "y" #'elfeed-search-yank
  "u" #'elfeed-search-tag-all-unread
  "r" #'elfeed-search-untag-all-unread
  "n" #'next-line
  "p" #'previous-line
  "t" #'elfeed-search-set-entry-title
  "T" #'elfeed-search-set-feed-title
  "+" #'elfeed-search-tag-all
  "-" #'elfeed-search-untag-all
  "<" #'elfeed-search-first-entry
  ">" #'elfeed-search-last-entry)

(easy-menu-define elfeed-search-mode-menu elfeed-search-mode-map
  "Menu for `elfeed-search-mode'."
  '("Elfeed Search"
    ["Show entry" elfeed-search-show-entry]
    ["Browse entry" elfeed-search-browse-url]
    ["Browse secondary" elfeed-search-browse-url-secondary]
    ["Copy URL" elfeed-search-yank]
    "--"
    ["Add tag" elfeed-search-tag-all]
    ["Remove tag" elfeed-search-untag-all]
    ["Mark as unread" elfeed-search-tag-all-unread]
    ["Mark as read" elfeed-search-untag-all-unread]
    "--"
    ["Live filter" elfeed-search-live-filter]
    ("Filter"
     ["Set filter" elfeed-search-set-filter]
     ["Clear filter" elfeed-search-clear-filter])
    "--"
    ["Fetch all" elfeed-search-fetch]
    ("Fetch"
     ["Fetch visible" elfeed-search-fetch-visible]
     ["Fetch feed" elfeed-update-feed])
    "--"
    ("Set title"
     ["Set feed title" elfeed-search-set-feed-title]
     ["Set entry title" elfeed-search-set-entry-title])
    "--"
    ("Maintenance"
     ["Apply auto tags" elfeed-apply-autotags-now]
     ["Apply hooks" elfeed-apply-hooks-now]
     ["Compact database" elfeed-db-compact]
     ["Unjam queue" elfeed-unjam])
    "--"
    ["Revert buffer" revert-buffer]
    ["Quit window" quit-window]
    "--"
    ["Customize" (customize-group 'elfeed)]))

(defun elfeed-search--remain-on-entry-p (action)
  "Remain on current entry for ACTION?"
  (and elfeed-search-remain-on-entry
       (or (not (consp elfeed-search-remain-on-entry))
           (memq action elfeed-search-remain-on-entry))))

(defun elfeed-search--after-action (action)
  "Possibly move to next entry after ACTION.
Movement is configured by `elfeed-search-remain-on-entry'."
  ;; Ignore region for the `show' action, like the related commands, which also
  ;; ignore the region and show the entry at point only.
  (when (and (or (eq action 'show) (not (use-region-p)))
             (not (elfeed-search--remain-on-entry-p action)))
    (forward-line)))

(defun elfeed-search--header-button (command &optional text)
  "Create header line button for COMMAND with optional TEXT."
  (propertize (or text (symbol-name command))
              'elfeed-header-button command
              'help-echo (format "Run %s" command)
              'mouse-face 'highlight))

(defun elfeed-search--intro-header ()
  "Return the header shown to new users."
  (concat "Database empty. Use "
          (elfeed-search--header-button #'elfeed-add-feed) ", or "
          (elfeed-search--header-button #'elfeed-load-opml) ", or "
          (elfeed-search--header-button #'elfeed-update) "."))

(defun elfeed-search--count-unread ()
  "Count the number of entries and feeds being currently displayed."
  (if (and elfeed-search-filter-active elfeed-search-filter-overflowing)
      (substring "?/?:?")
    (cl-loop with feeds = (make-hash-table :test 'equal)
             for entry in elfeed-search-entries
             for feed = (elfeed-entry-feed entry)
             for url = (elfeed-feed-url feed)
             count entry into entry-count
             count (elfeed-tagged-p 'unread entry) into unread-count
             do (puthash url t feeds)
             finally
             (cl-return
              (format "%d/%d:%d"
                      unread-count entry-count
                      (hash-table-count feeds))))))

(defun elfeed-search--header ()
  "Computes the string to be used as the Elfeed header."
  (cond
   ((not elfeed-db) "Database not loaded.")
   ((zerop (elfeed-db-last-update))
    (elfeed-search--intro-header))
   ((let ((total (elfeed-queue-count-total)))
      (when (> total 0)
        (let ((active (elfeed-queue-count-active)))
          (format "%d jobs pending, %d active..."
                  (- total active) active)))))
   ((let ((update (elfeed-add-properties
                   (format-time-string
                    "%Y-%m-%d %H:%M"
                    (seconds-to-time (elfeed-db-last-update)))
                   'face 'elfeed-search-last-update-face))
          (unread (elfeed-add-properties
                   (elfeed-search--count-unread)
                   'face 'elfeed-search-unread-count-face))
          (filter (cond
                   (elfeed-search-filter-active "")
                   ((string-match-p "[^ ]" elfeed-search-filter)
                    (mapconcat
                     (lambda (x)
                       (elfeed-add-properties
                        x 'mouse-face 'highlight
                        'help-echo "Remove filter"
                        'elfeed-header-filter x))
                     (split-string elfeed-search-filter) " "))
                   (""))))
      (concat
       (elfeed-search--header-button #'elfeed-update
                                     (concat "Updated " update))
       ", " unread (and (not (equal filter "")) ", ")
       (propertize filter 'face 'elfeed-search-filter-face))))))

(define-derived-mode elfeed-search-mode special-mode "elfeed-search"
  "Major mode for listing elfeed feed entries."
  :syntax-table nil :abbrev-table nil :interactive nil
  (setq-local truncate-lines t
              desktop-save-buffer #'elfeed-search-desktop-save
              ;; Provide format string via symbol value slot so that it will
              ;; not be %-construct interpolated. The symbol is uninterned
              ;; so that it's not *really* a global variable.
              header-line-format
              (let ((symbol (make-symbol "dummy")))
                (put symbol 'risky-local-variable t)
                `(:eval
                  (prog1 ',symbol
                    (set ',symbol (funcall elfeed-search-header-function)))))
              bookmark-make-record-function
              #'elfeed-search-bookmark-make-record
              revert-buffer-function #'elfeed-search--update-force
              default-directory (elfeed-default-directory))
  (buffer-disable-undo)
  (hl-line-mode)
  (make-local-variable 'elfeed-search-entries)
  (make-local-variable 'elfeed-search-filter)
  (add-hook 'elfeed-update-hooks #'elfeed-search--update-debounce)
  (add-hook 'elfeed-update-init-hooks #'elfeed-search--update-force)
  (add-hook 'kill-buffer-hook #'elfeed-db-save t 'local)
  (add-hook 'window-size-change-functions #'elfeed-search--resize nil 'local)
  (add-hook 'elfeed-db-unload-hook #'elfeed-search--unload)
  (add-hook 'quit-window-hook 'elfeed-db-save nil 'local)
  (elfeed-search-update :force))

;;;###autoload
(defun elfeed-search ()
  "Enter `elfeed-search' buffer."
  (interactive)
  (switch-to-buffer (elfeed-search-buffer))
  (unless (eq major-mode 'elfeed-search-mode)
    (elfeed-search-mode)))

(defun elfeed-search-buffer ()
  "Create and return search buffer."
  (get-buffer-create "*elfeed-search*"))

(defun elfeed-search--unload ()
  "Hook function for `elfeed-db-unload-hook'."
  (with-current-buffer (elfeed-search-buffer)
    ;; don't try to save the database in this case
    (remove-hook 'kill-buffer-hook #'elfeed-db-save t)
    (kill-buffer)))

(defun elfeed-search-format-date (date)
  "Format DATE for printing in `elfeed-search-mode'.
The customization `elfeed-search-date-format' sets the formatting."
  (cl-destructuring-bind (format target alignment) elfeed-search-date-format
    (let* ((string (format-time-string format (seconds-to-time date)))
           (width (string-width string)))
      (cond
       ((> width target)
        (if (eq alignment :left)
            (substring string 0 target)
          (substring string (- width target) width)))
       ((< width target)
        (let ((pad (make-string (- target width) ?\s)))
          (if (eq alignment :left)
              (concat string pad)
            (concat pad string))))
       (string)))))

(defface elfeed-search-date-face
  '((((class color) (background light)) (:foreground "#aaa"))
    (((class color) (background dark))  (:foreground "#77a")))
  "Face used in search mode for dates."
  :group 'elfeed)

(defface elfeed-search-title-face
  '((((class color) (background light)) (:foreground "#000"))
    (((class color) (background dark))  (:foreground "#fff")))
  "Face used in search mode for titles."
  :group 'elfeed)

(defface elfeed-search-unread-title-face
  '((t :weight bold))
  "Face used in search mode for unread entry titles."
  :group 'elfeed)

(defface elfeed-search-feed-face
  '((((class color) (background light)) (:foreground "#aa0"))
    (((class color) (background dark))  (:foreground "#ff0")))
  "Face used in search mode for feed titles."
  :group 'elfeed)

(defface elfeed-search-tag-face
  '((((class color) (background light)) (:foreground "#070"))
    (((class color) (background dark))  (:foreground "#0f0")))
  "Face used in search mode for tags."
  :group 'elfeed)

(defface elfeed-search-last-update-face
  '((t))
  "Face for showing the date and time the database was last updated."
  :group 'elfeed)

(defface elfeed-search-unread-count-face
  '((((class color) (background light)) (:foreground "#000"))
    (((class color) (background dark))  (:foreground "#fff")))
  "Face used in search mode for the unread counter in the header."
  :group 'elfeed)

(defface elfeed-search-filter-face
  '((t :inherit mode-line-buffer-id))
  "Face for showing the current Elfeed search filter."
  :group 'elfeed)

(defcustom elfeed-search-title-max-width 70
  "Maximum column width for titles in the `elfeed-search' buffer."
  :group 'elfeed
  :type 'integer)

(defcustom elfeed-search-title-min-width 16
  "Minimum column width for titles in the `elfeed-search' buffer."
  :group 'elfeed
  :type 'integer)

(defcustom elfeed-search-trailing-width 30
  "Space reserved for displaying the feed and tag information."
  :group 'elfeed
  :type 'integer)

(defcustom elfeed-search-face-alist
  '((unread elfeed-search-unread-title-face))
  "Mapping of tags to faces in the Elfeed entry listing."
  :group 'elfeed
  :type '(alist :key-type symbol :value-type (repeat face)))

(defun elfeed-search--faces (tags)
  "Return all the faces that apply to an entry with TAGS."
  (nconc (cl-loop for (tag . faces) in elfeed-search-face-alist
                  when (memq tag tags)
                  append faces)
         (list 'elfeed-search-title-face)))

(defun elfeed-search--format-tags (tags)
  "Format TAGS string."
  (mapconcat
   (lambda (s)
     (propertize (symbol-name s)
                 'face 'elfeed-search-tag-face
                 'mouse-face 'highlight 'elfeed-tag s))
   tags ","))

(defun elfeed-search-print-entry--default (entry)
  "Print ENTRY to the buffer."
  (let* ((tags (elfeed-entry-tags entry))
         (date-float (elfeed-entry-date entry))
         (date-str (elfeed-search-format-date date-float))
         (title (or (elfeed-meta--title entry) ""))
         (title-faces (elfeed-search--faces tags))
         (feed (elfeed-entry-feed entry))
         (feed-title (and feed (elfeed-meta--title feed)))
         (window (get-buffer-window))
         (title-width (- (if window (window-width window) (frame-width))
                         10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left)))
    (insert (elfeed-add-properties date-str
                                   'face 'elfeed-search-date-face
                                   'mouse-face 'highlight 'elfeed-date date-float)
            " "
            (elfeed-add-properties title-column
                                   'face title-faces 'kbd-help title
                                   'mouse-face 'highlight 'elfeed-entry-title t))
    (when feed-title
      (insert " " (propertize feed-title 'face 'elfeed-search-feed-face
                              'mouse-face 'highlight 'elfeed-feed feed)))
    (when tags
      (insert " (" (elfeed-search--format-tags tags) ")"))))

(defun elfeed-search-parse-filter (filter)
  "Parse the elements of a search FILTER into a plist."
  (let ((must-have ())
        (must-not-have ())
        (before nil)
        (after nil)
        (matches ())
        (not-matches ())
        (limit nil)
        (feeds ())
        (not-feeds ()))
    (cl-loop for element in (split-string filter)
             for type = (aref element 0)
             do (cl-case type
                  (?+
                   (let ((symbol (intern (substring element 1))))
                     (unless (eq '## symbol)
                       (push symbol must-have))))
                  (?-
                   (let ((symbol (intern (substring element 1))))
                     (unless (eq '## symbol)
                       (push symbol must-not-have))))
                  (?@ (cl-multiple-value-bind (a b)
                          (split-string (substring element 1) "--")
                        (let ((duration-a (elfeed-time-duration a))
                              (duration-b (and b (elfeed-time-duration b))))
                          (when (and duration-b (> duration-b duration-a))
                            (cl-rotatef duration-a duration-b))
                          (when duration-b (setf before duration-b))
                          (setf after duration-a))))
                  (?! (let ((re (substring element 1)))
                        (when (elfeed-valid-regexp-p re)
                          (push re not-matches))))
                  (?# (setf limit (string-to-number (substring element 1))))
                  (?= (let ((re (substring element 1)))
                        (when (elfeed-valid-regexp-p re)
                          (push re feeds))))
                  (?~ (let ((re (substring element 1)))
                        (when (elfeed-valid-regexp-p re)
                          (push re not-feeds))))
                  (otherwise (when (elfeed-valid-regexp-p element)
                               (push element matches)))))
    `(,@(and before (list :before before))
      ,@(and after (list :after after))
      ,@(and must-have (list :must-have must-have))
      ,@(and must-not-have (list :must-not-have must-not-have))
      ,@(and matches (list :matches matches))
      ,@(and not-matches (list :not-matches not-matches))
      ,@(and limit (list :limit limit))
      ,@(and feeds (list :feeds feeds))
      ,@(and not-feeds (list :not-feeds not-feeds)))))

(defun elfeed-search--recover-time (seconds)
  "Pick a reasonable filter representation for SECONDS."
  (let ((units '((60   1 "min")
                 (60   1 "hour")
                 (24   1 "day")
                 (7    1 "week")
                 (30   7 "month")
                 (1461 120 "year")))
        (value (float seconds))
        (name "second"))
    (cl-loop for (n d unit) in units
             for next-value = (/ (* value d) n)
             when (< next-value 1.0)
             return t
             do (setf name unit
                      value next-value))
    (let ((count (format "%.4g" value)))
      (concat count name (if (equal count "1") "" "s")))))

(defun elfeed-search--recover-units (after-seconds &optional before-seconds)
  "Stringify the age or optionally a date range.
The date range is specified by AFTER-SECONDS and BEFORE-SECONDS."
  (apply 'concat "@"
          (elfeed-search--recover-time after-seconds)
          (when before-seconds
            (list "--"(elfeed-search--recover-time before-seconds)))))

(defun elfeed-search-unparse-filter (filter)
  "Inverse of `elfeed-search-parse-filter', turning FILTER into a string.

The time (@n-units-ago) filter may not exactly match the
original, but will be equal in its effect."
  (let ((output ()))
    (cl-destructuring-bind (&key after     before
                                 must-have must-not-have
                                 matches   not-matches
                                 feeds     not-feeds
                                 limit &allow-other-keys)
        filter
      (when after
        (push (elfeed-search--recover-units after before) output))
      (dolist (tag must-have)
        (push (format "+%S" tag) output))
      (dolist (tag must-not-have)
        (push (format "-%S" tag) output))
      (dolist (re matches)
        (push re output))
      (dolist (re not-matches)
        (push (concat "!" re) output))
      (when limit
        (push (format "#%d" limit) output))
      (dolist (feed feeds)
        (push (format "=%s" feed) output))
      (dolist (feed not-feeds)
        (push (format "~%s" feed) output))
      (string-join (nreverse output) " "))))

(defun elfeed-search-filter (filter entry feed &optional count)
  "Return non-nil if ENTRY and FEED pass FILTER.

COUNT is the total number of entries collected so far, for
filtering against a limit filter (example: #10).

See `elfeed-search-set-filter' for format/syntax documentation.
This function must *only* be called within the body of
`elfeed-db-visit' because it may perform a non-local exit."
  (cl-destructuring-bind (&key must-have must-not-have
                               matches   not-matches
                               feeds     not-feeds
                               after limit &allow-other-keys)
      filter
    (let* ((tags (elfeed-entry-tags entry))
           (date (elfeed-entry-date entry))
           (age (- (float-time) date))
           (title (elfeed-meta--title entry))
           (link (elfeed-entry-link entry))
           (feed-title (or (elfeed-meta--title feed) ""))
           (feed-id (elfeed-feed-id feed)))
      (when (or (and after (> age after))
                (and limit (<= limit 0))
                (and limit count (>= count limit)))
        (elfeed-db-return))
      (and (cl-every  (lambda (tag) (memq tag tags)) must-have)
           (cl-notany (lambda (tag) (memq tag tags)) must-not-have)
           (or (null matches)
               (cl-every
                (lambda (m)
                  (or (and title      (string-match-p m title))
                      (and link       (string-match-p m link))))
                matches))
           (cl-notany (lambda (m)
                        (or (and title      (string-match-p m title))
                            (and link       (string-match-p m link))))
                      not-matches)
           (or (null feeds)
               (cl-some (lambda (f)
                          (or (string-match-p f feed-id)
                              (string-match-p f feed-title)))
                        feeds))
           (cl-notany (lambda (f)
                        (or (string-match-p f feed-id)
                            (string-match-p f feed-title)))
                      not-feeds)))))

(defun elfeed-search-compile-filter (filter)
  "Compile FILTER into a lambda function for `byte-compile'.

Executing a filter in bytecode form is generally faster than
\"interpreting\" the filter with the function `elfeed-search-filter'."
  (cl-destructuring-bind (&key after     before
                               must-have must-not-have
                               matches   not-matches
                               feeds     not-feeds
                               limit &allow-other-keys)
      filter
    `(lambda (,(if (or after matches not-matches must-have must-not-have)
                   'entry
                 '_entry)
              ,(if (or feeds not-feeds)
                   'feed
                 '_feed)
              ,(if limit
                   'count
                 '_count))
       (let* (,@(when after
                  '((date (elfeed-entry-date entry))
                    (age (- (float-time) date))))
              ,@(when (or must-have must-not-have)
                  '((tags (elfeed-entry-tags entry))))
              ,@(when (or matches not-matches)
                  '((title (elfeed-meta--title entry))
                    (link (elfeed-entry-link entry))))
              ,@(when (or feeds not-feeds)
                  '((feed-id (elfeed-feed-id feed))
                    (feed-title (or (elfeed-meta--title feed) "")))))
         ,@(when after
             `((when (> age ,after)
                 (elfeed-db-return))))
         ,@(when limit
             `((when (>= count ,limit)
                 (elfeed-db-return))))
         (and ,@(cl-loop for forbid in must-not-have
                         collect `(not (memq ',forbid tags)))
              ,@(cl-loop for forbid in must-have
                         collect `(memq ',forbid tags))
              ,@(cl-loop for regex in matches collect
                         `(or (string-match-p ,regex title)
                              (string-match-p ,regex link)))
              ,@(cl-loop for regex in not-matches collect
                         `(not
                           (or (string-match-p ,regex title)
                               (string-match-p ,regex link))))
              ,@(when feeds
                  `((or ,@(cl-loop
                           for regex in feeds
                           collect `(string-match-p ,regex feed-id)
                           collect `(string-match-p ,regex feed-title)))))
              ,@(when not-feeds
                  `((not
                     (or ,@(cl-loop
                            for regex in not-feeds
                            collect `(string-match-p ,regex feed-id)
                            collect `(string-match-p ,regex feed-title))))))
              ,@(when before
                  `((> age ,before))))))))

(defun elfeed-search--completion-table ()
  "Completion table for the search filter."
  (let (cache)
    (lambda (_str)
      (let ((input
             (if-let* ((win (active-minibuffer-window)))
                 (with-current-buffer (window-buffer win)
                   ;; Obtain input from the minibuffer to compute dynamic
                   ;; candidates.  We cannot use _str since the argument is
                   ;; always empty for non-basic completion styles like
                   ;; substring or orderless.
                   (save-excursion
                     (goto-char (point-max))
                     (when (re-search-backward
                            "\\s-" (minibuffer-prompt-end) 'noerror)
                       (goto-char (min (1+ (point)) (point-max))))
                     (buffer-substring-no-properties (point) (point-max))))
               "")))
        (append
         ;; Dynamically computed @age candidates.
         (when (string-match "\\`@[0-9]+" input)
           (let* ((n (substring input 1 (match-end 0)))
                  (p (if (equal n "1") "" "s")))
             (mapcar (lambda (x) (concat "@" n x p))
                     '("hour" "day" "week" "month" "year"))))
         ;; Cached, but static candidates, not depending on prefix.
         (unless (equal input "")
           (with-memoization cache
             (delete-dups
              (nconc
               ;; +tag and -tag candidate strings
               (mapcan
                (lambda (x) (list (format "+%s" x) (format "-%s" x)))
                (elfeed-db-get-all-tags))
               ;; Old words from history
               (mapcan
                (lambda (h)
                  (delq nil
                        (mapcar (lambda (x) (and (length> x 1) x))
                                (split-string h))))
                elfeed-search-filter-history))))))))))

(defun elfeed-search--prompt (current)
  "Prompt for a new filter, starting with CURRENT."
  (unless (or (equal "" current) (string-suffix-p " " current))
    (setq current (concat current " ")))
  (if elfeed-search-completion
      (dlet ((crm-separator "[ \t]+")
             (crm-prompt "%p")
             (completion-show-inline-help nil))
        (string-join
         (completing-read-multiple
          "Filter: "
          (completion-table-with-metadata
           (completion-table-dynamic
            (elfeed-search--completion-table))
           '((category . elfeed-search)))
          nil nil current 'elfeed-search-filter-history)
         " "))
    (read-from-minibuffer
     "Filter: " current nil nil 'elfeed-search-filter-history)))

(defun elfeed-search--prompt-tags (prompt &optional entries)
  "Prompt for tags in the minibuffer.
PROMPT is the prompt string and ENTRIES is a list of entries to take the
tags from."
  (let* ((all-tags
          (or (if entries
                  (delete-dups
                   (copy-sequence
                    (apply #'append
                           (mapcar #'elfeed-entry-tags
                                   (ensure-list entries)))))
                (elfeed-db-get-all-tags))
              (user-error "No tags found")))
         (all-tags (mapcar #'symbol-name all-tags))
         (initial (and entries (length= all-tags 1) (car all-tags)))
         (tags (if elfeed-search-completion
                   (completing-read-multiple
                    prompt
                    (completion-table-with-metadata
                     all-tags
                     '((category . elfeed-tag)))
                    nil entries nil initial)
                 (split-string (read-from-minibuffer prompt initial)
                               "[ \t]*,[ \t]*" t))))
    (unless tags
      (user-error "No tags given!"))
    (mapcar #'intern tags)))

(defun elfeed-search-clear-filter ()
  "Reset the search filter.
Use the default value of the variable `elfeed-search-filter'."
  (interactive nil elfeed-search-mode)
  (setf elfeed-search-filter (default-value 'elfeed-search-filter))
  (elfeed-search-update :force))

(defun elfeed-search-set-filter (new-filter)
  "Set a new search filter for the `elfeed-search' buffer.

When NEW-FILTER is nil, reset the filter to the default value.

When given a prefix argument, the current filter is not displayed
in the minibuffer when prompting for a new filter.

Any component beginning with a + or - is treated as a tag.  If +
the tag must be present on the entry.  If - the tag must *not* be
present on the entry.  Examples: \"+unread\" or \"+unread -comic\".

Any component beginning with an @ is an age limit or an age
range.  If a limit, no posts older than this are allowed.  If a
range, posts dates have to be inbetween the specified date
range.  Examples:
- \"@3days\"
- \"@1year\"
- \"@3-days-ago\"
- \"@1-year-old\"
- \"@2019-06-24\"
- \"@2019-06-24--2019-06-24\"
- \"@5-days-ago--1-day-ago\"

Any component beginning with a # is an entry count maximum.  The
number following # determines the maxiumum number of entries
to be shown (descending by date).  Examples: \"#20\" or \"#100\".

Any component beginning with a = is a regular expression matching
the entry's feed (title or URL). Only entries belonging to a feed
that match at least one of the = expressions will be shown.

Every other space-seperated element is treated like a regular
expression, matching against entry link, title, and feed title."
  (interactive
   (let ((elfeed-search-filter-active :non-interactive))
     (list (elfeed-search--prompt
            (if current-prefix-arg "" elfeed-search-filter))))
    elfeed-search-mode)
  (with-current-buffer (elfeed-search-buffer)
    (setf elfeed-search-filter
          (or new-filter (default-value 'elfeed-search-filter)))
    (elfeed-search-update :force)))

(defun elfeed-search--update-list ()
  "Update `elfeed-search-entries' list."
  (let* ((filter (elfeed-search-parse-filter elfeed-search-filter))
         (head (list nil))
         (tail head)
         (count 0))
    (if elfeed-search-compile-filter
        ;; Force lexical bindings regardless of the current
        ;; buffer-local value. Lexical scope uses the faster
        ;; stack-ref opcode instead of the traditional varref opcode.
        (let ((lexical-binding t)
              (func (byte-compile (elfeed-search-compile-filter filter))))
          (elfeed-db-visit (entry feed)
            (when (funcall func entry feed count)
              (setf (cdr tail) (list entry)
                    tail (cdr tail)
                    count (1+ count)))))
      (elfeed-db-visit (entry feed)
        (when (elfeed-search-filter filter entry feed count)
          (setf (cdr tail) (list entry)
                tail (cdr tail)
                count (1+ count)))))
    ;; Determine the final list order
    (let ((entries (cdr head)))
      (when elfeed-search-sort-function
        (setf entries (sort entries elfeed-search-sort-function)))
      (when (eq elfeed-search-sort-order 'ascending)
        (setf entries (nreverse entries)))
      (setf elfeed-search-entries
            entries))))

(defun elfeed--save-position ()
  "Save entry, line and column."
  (list (elfeed-search-selected :ignore-region)
        (line-number-at-pos)
        (current-column)))

(defun elfeed--restore-position (pos)
  "Restore entry, line and column from saved POS."
  (pcase-let* ((`(,entry ,line ,column) pos)
               (pos (cl-position entry elfeed-search-entries)))
    (elfeed-goto-line (if pos (1+ pos) line))
    (move-to-column column)))

(defmacro elfeed-save-excursion (&rest body)
  "Like `save-mark-and-excursion' around BODY.
But keep entry, line and column instead of only point."
  (declare (indent defun))
  (cl-with-gensyms (point-pos mark-pos)
    `(let* ((,point-pos (elfeed--save-position))
            (,mark-pos (cons (when-let* ((m (marker-position (mark-marker))))
                               (save-excursion
                                 (goto-char m)
                                 (elfeed--save-position)))
                             mark-active)))
     (unwind-protect
         ,@body
       (elfeed--restore-position ,point-pos)
       (when-let* ((m (car ,mark-pos)))
         (setcar ,mark-pos (save-excursion
                             (elfeed--restore-position m)
                             (copy-marker (point)))))
       (save-mark-and-excursion--restore ,mark-pos)))))

(defun elfeed-search-update (&optional force)
  "Update the `elfeed-search' buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed.
Otherwise debounce by `elfeed-search-update-delay' and only redraw when
there are changes.  When called interactively FORCE is t, and the
command behaves just like `revert-buffer'."
  (declare (completion ignore)) ;; Press "g" or M-x revert-buffer
  (interactive (list t))
  (when elfeed-search--update-timer
    (cancel-timer elfeed-search--update-timer)
    (setq elfeed-search--update-timer nil))
  (when-let* ((buffer (get-buffer "*elfeed-search*")))
    (if force
        (elfeed-search--update-immediately buffer :force)
      (setf elfeed-search--update-timer
            (run-at-time elfeed-search-update-delay nil
                         #'elfeed-search--update-immediately
                         buffer)))))

(defun elfeed-search--print-entry (entry)
  "Print ENTRY line and attach `elfeed-entry' text property."
  (let ((beg (point)))
    (funcall elfeed-search-print-entry-function entry)
    (put-text-property beg (point) 'elfeed-entry entry)))

(defun elfeed-search--update-immediately (buffer &optional method)
  "Immediately update the `elfeed-search' BUFFER.
METHOD can be nil, :force to force a full entry update and redraw or
:preserve to preserve the entries and redraw.  Do not use this function
directly.  Instead use `elfeed-search-update'."
  (when (and (buffer-live-p buffer)
             (or (eq method :force)
                 (eq method :preserve)
                 (and (not elfeed-search-filter-active)
                      (< elfeed-search-last-update (elfeed-db-last-update)))))
    ;; Run inside window such that save excursion moves the window point.
    (with-selected-window (or (get-buffer-window buffer) (selected-window))
      ;; If no window is found, we still have to execute in the buffer.
      (with-current-buffer buffer
        (elfeed-save-excursion
          (let ((inhibit-read-only t)
                (standard-output (current-buffer)))
            (erase-buffer)
            (unless (eq method :preserve)
              (elfeed-search--update-list))
            (dolist (entry elfeed-search-entries)
              (elfeed-search--print-entry entry)
              (insert "\n"))
            (setf elfeed-search-last-update (float-time))))
        (setq list-buffers-directory elfeed-search-filter)
        ;; Highlighting gets lost due to debouncing.
        (hl-line-highlight)
        (run-hooks 'elfeed-search-update-hook))))
  ;; Always force a header line update
  (force-mode-line-update))

(defun elfeed-search--update-force (&rest _)
  "Call `elfeed-search-update' with argument :force.
The function is used as hook.  Instead of this function, you usually
want to use `elfeed-search-update'."
  (elfeed-search-update :force))

(defun elfeed-search--update-debounce (&rest _)
  "Call `elfeed-search-update' with debouncing.
The function is used as hook.  Instead of this function, you usually
want to use `elfeed-search-update'."
  (elfeed-search-update))

(defun elfeed-search--resize (_win)
  "Resize search window.
The function is used as hook."
  (when elfeed-search--resize-timer
    (cancel-timer elfeed-search--resize-timer)
    (setq elfeed-search--resize-timer nil))
  (setf elfeed-search--resize-timer
        (run-at-time elfeed-search-update-delay nil
                     #'elfeed-search--update-immediately
                     (elfeed-search-buffer) :preserve)))

(defun elfeed-search-fetch (prefix)
  "Update all feeds via `elfeed-update', or only visible feeds with PREFIX.
Given a prefix, this function becomes `elfeed-search-fetch-visible'."
  (interactive "P" elfeed-search-mode)
  (if prefix
      (elfeed-search-fetch-visible)
    (elfeed-update)))

(defun elfeed-search-fetch-visible ()
  "Update any feed with an entry currently displayed in the search buffer."
  (interactive nil elfeed-search-mode)
  (cl-loop with seen = (make-hash-table :test 'equal)
           for entry in elfeed-search-entries
           for feed = (elfeed-entry-feed entry)
           for url = (elfeed-feed-url feed)
           when (not (gethash url seen))
           do (elfeed-update-feed (setf (gethash url seen) url))))

(defun elfeed-search-update-line (&optional n)
  "Redraw line N, defaulting to the current line."
  (let ((inhibit-read-only t))
    (save-excursion
      (when n (elfeed-goto-line n))
      (when-let* ((entry (elfeed-search-selected :ignore-region)))
        (elfeed-kill-line)
        (elfeed-search--print-entry entry)))))

(defun elfeed-search-update-entry (entry)
  "Redraw ENTRY in the `elfeed-search' buffer."
  (when-let* ((n (cl-position entry elfeed-search-entries)))
    (elfeed-search-update-line (1+ n))))

(defun elfeed-search-selected (&optional ignore-region)
  "Return a list of the currently selected feeds.

If IGNORE-REGION is non-nil, only return the entry under point."
  (if (and (not ignore-region) (use-region-p))
      (let ((start (region-beginning))
            (end   (region-end)))
        (save-excursion
          (goto-char start)
          (goto-char (pos-bol))
          (cl-loop while (< (point) end)
                   when (prog1 (get-text-property (point) 'elfeed-entry)
                          (forward-line 1))
                   collect it into selected
                   finally return selected)))
    (when-let* ((entry (get-text-property (pos-bol) 'elfeed-entry)))
      (if ignore-region entry (list entry)))))

(defun elfeed-search-browse-url (&optional secondary)
  "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument SECONDARY, visit the current entry in
the browser defined by `browse-url-secondary-browser-function'."
  (interactive "P" elfeed-search-mode)
  (let ((entries (elfeed-search-selected)))
    (when (elfeed--confirm-browse-url-p (length entries))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (elfeed-browse-url it secondary))
      ;; `browse-url' could have switched to another buffer if eww or another
      ;; internal browser is used, but the remainder of the functions needs to
      ;; run in the elfeed buffer.
      (with-current-buffer (elfeed-search-buffer)
        (mapc #'elfeed-search-update-entry entries)
        (elfeed-search--after-action 'browse)))))

(defun elfeed-search-browse-url-secondary ()
  "Visit the current entry in your browser using the secondary browser."
  (interactive nil elfeed-search-mode)
  (elfeed-search-browse-url t))

(defun elfeed-search-yank ()
  "Copy the selected feed items to clipboard and `kill-ring'."
  (interactive nil elfeed-search-mode)
  (let* ((entries (elfeed-search-selected))
         (links (mapcar #'elfeed-entry-link entries))
         (links-str (string-join links " ")))
    (when entries
      (elfeed-untag entries 'unread)
      (kill-new links-str)
      (gui-set-selection elfeed-search-clipboard-type links-str)
      (message "Copied: %s" links-str)
      (mapc #'elfeed-search-update-entry entries)
      (elfeed-search--after-action 'yank))))

(defun elfeed-search-tag-all (&rest tags)
  "Apply TAGS to all selected entries."
  (interactive (elfeed-search--prompt-tags "Tag: ") elfeed-search-mode)
  (let ((entries (elfeed-search-selected)))
    (apply #'elfeed-tag entries tags)
    (mapc #'elfeed-search-update-entry entries)
    (elfeed-search--after-action 'tag)))

(defun elfeed-search-untag-all (&rest tags)
  "Remove TAGS from all selected entries."
  (interactive
   (elfeed-search--prompt-tags "Untag: " (elfeed-search-selected))
   elfeed-search-mode)
  (let ((entries (elfeed-search-selected)))
    (apply #'elfeed-untag entries tags)
    (mapc #'elfeed-search-update-entry entries)
    (elfeed-search--after-action 'tag)))

(defun elfeed-search-toggle-all (&rest tags)
  "Toggle TAGS on all selected entries."
  (interactive (elfeed-search--prompt-tags "Toggle: ") elfeed-search-mode)
  (let ((entries (elfeed-search-selected)))
    (dolist (tag tags)
      (let (entries-tag entries-untag)
        (cl-loop for entry in entries
                 when (elfeed-tagged-p tag entry)
                 do (push entry entries-untag)
                 else do (push entry entries-tag))
        (elfeed-tag entries-tag tag)
        (elfeed-untag entries-untag tag)))
    (mapc #'elfeed-search-update-entry entries)
    (elfeed-search--after-action 'tag)))

(defun elfeed-search-show-entry (entry)
  "Display the currently selected ENTRY in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region))
               elfeed-search-mode)
  (when (elfeed-entry-p entry)
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (elfeed-search--after-action 'show)
    ;; Update hl-line overlay. This does not happen automatically, since
    ;; `elfeed-show-entry' switches to another buffer.
    (hl-line-highlight)
    ;; elfeed-show.el is required by elfeed.el at runtime.
    (declare-function elfeed-show-entry "elfeed-show")
    (elfeed-show-entry entry)))

(defun elfeed-search--add-filter (str)
  "Add STR to variable `elfeed-search-filter'."
  (let ((filter (split-string elfeed-search-filter)))
    (unless (member str filter)
      (elfeed-search-set-filter (string-join (append filter (list str)) " ")))))

(defun elfeed-search--remove-filter (str)
  "Remove STR from variable `elfeed-search-filter'."
  (let ((filter (split-string elfeed-search-filter)))
    (when (member str filter)
      (elfeed-search-set-filter (string-join (delete str filter) " ")))))

(defun elfeed-search--feed-filter (feed)
  "Create filter string which matches FEED."
  (concat "=" (string-replace
               "\\." "."
               (regexp-quote (elfeed-feed-id feed)))))

(defun elfeed-search--tag-filter (tags)
  "Create filter string which matches a single or a list TAGS."
  (mapconcat (lambda (x) (format "+%s" x))
             (ensure-list tags) " "))

(defun elfeed-search-header-click (event)
  "Handle click EVENT on the header line of the search buffer."
  (declare (completion ignore))
  (interactive "@e")
  (when-let* ((pos (event-end event))
              (str (posn-string pos)))
    (let (obj)
      (cond
       ((setq obj (get-text-property (cdr str) 'elfeed-header-filter (car str)))
        (elfeed-search--remove-filter obj))
       ((setq obj (get-text-property (cdr str) 'elfeed-header-button (car str)))
        (call-interactively obj))))))

(defun elfeed-search-click (event)
  "Handle click EVENT in search buffer."
  (declare (completion ignore))
  (interactive "@e")
  (when-let* ((pos (event-end event))
              (pos (posn-point pos)))
    (let (obj)
      (cond
       ((setq obj (get-text-property pos 'elfeed-date))
        (elfeed-search--add-filter (concat "@" (elfeed-search-format-date obj))))
       ((setq obj (get-text-property pos 'elfeed-tag))
        (elfeed-search--add-filter (elfeed-search--tag-filter obj)))
       ((setq obj (get-text-property pos 'elfeed-feed))
        (elfeed-search--add-filter (elfeed-search--feed-filter obj)))
       ((setq obj (and (get-text-property pos 'elfeed-entry-title)
                       (get-text-property pos 'elfeed-entry)))
        (elfeed-search-show-entry obj))))))

(defun elfeed-search-set-entry-title (&optional title)
  "Manually set TITLE for the entry under point.
Sets the :title key of the entry's metadata.  See `elfeed-meta'."
  (interactive nil elfeed-search-mode)
  (let ((entry (or (elfeed-search-selected :ignore-region)
                   (user-error "No entry selected!"))))
    (unless title
      (setq title (read-from-minibuffer "Entry title: "
                                        (elfeed-meta--title entry))))
    (setf (elfeed-meta entry :title) title)
    (elfeed-search-update-entry entry)))

(defun elfeed-search-set-feed-title (&optional title)
  "Manually set TITLE for the feed belonging to the entry under point.
Sets the :title key of the feed's metadata.  See `elfeed-meta'."
  (interactive nil elfeed-search-mode)
  (let ((feed (elfeed-entry-feed
               (or (elfeed-search-selected :ignore-region)
                   (user-error "No entry selected!")))))
    (unless title
      (setq title (read-from-minibuffer "Feed title: "
                                        (elfeed-meta--title feed))))
    (setf (elfeed-meta feed :title) title)
    (elfeed-search-update :force)))

;; Live Filters

(defvar elfeed-search-filter-syntax-table
  (let ((table (make-syntax-table)))
    (prog1 table
      (modify-syntax-entry ?+ "w" table)
      (modify-syntax-entry ?- "w" table)
      (modify-syntax-entry ?= "w" table)
      (modify-syntax-entry ?@ "w" table)))
  "Syntax table active when editing the filter in the minibuffer.")

(defun elfeed-search--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when elfeed-search-filter-active
    (set-syntax-table elfeed-search-filter-syntax-table)
    (when (eq :live elfeed-search-filter-active)
      (add-hook 'post-command-hook 'elfeed-search--live-update nil :local))))

(add-hook 'minibuffer-setup-hook 'elfeed-search--minibuffer-setup)

(defun elfeed-search--live-update ()
  "Update the `elfeed-search' buffer based on the contents of the minibuffer."
  (when (eq :live elfeed-search-filter-active)
    (let ((current-filter (minibuffer-contents-no-properties)))
      (with-current-buffer (elfeed-search-buffer)
        (let* ((window (get-buffer-window))
               (height (window-total-height window))
               (limiter (if window (format "#%d " height) "#1 "))
               (elfeed-search-filter (concat limiter current-filter)))
          (elfeed-search-update :force)
          (setf elfeed-search-filter-overflowing
                (length= elfeed-search-entries height)))))))

(defun elfeed-search-live-filter ()
  "Filter the `elfeed-search' buffer as the filter is written."
  (interactive nil elfeed-search-mode)
  (unwind-protect
      (let ((elfeed-search-filter-active :live))
        (setq elfeed-search-filter
              (elfeed-search--prompt elfeed-search-filter)))
    (elfeed-search-update :force)))

;; Bookmarks

;;;###autoload
(defun elfeed-search-bookmark-handler (record)
  "Jump to an `elfeed-search' bookmark RECORD."
  (elfeed-search)
  (elfeed-search-set-filter (bookmark-prop-get record 'location)))
(put 'elfeed-search-bookmark-handler 'bookmark-handler-type "Elfeed Search")

(defun elfeed-search-bookmark-make-record ()
  "Return a bookmark record for the current `elfeed-search' buffer."
  (let* ((filter (elfeed-search-parse-filter elfeed-search-filter))
         (tags (plist-get filter :must-have)))
    `(,(format "elfeed %s" elfeed-search-filter)
      (location . ,elfeed-search-filter)
      (tags ,@(mapcar #'symbol-name tags))
      (handler . elfeed-search-bookmark-handler))))

;; Desktop Save

(defun elfeed-search-desktop-save (_desktop-dirname)
  "Save the state of the current `elfeed-search' buffer.
The state may be restored as part of a saved desktop.  Also save the
state of the db for when `desktop-auto-save-timeout' is enabled."
  (elfeed-db-save)
  elfeed-search-filter)

;;;###autoload
(defun elfeed-search-desktop-restore (_file-name _buffer-name search-filter)
  "Restore the SEARCH-FILTER of an `elfeed-search' buffer on desktop restore."
  (elfeed-search)
  (elfeed-search-set-filter search-filter)
  (current-buffer))

;;;###autoload
(add-to-list 'desktop-buffer-mode-handlers
             '(elfeed-search-mode . elfeed-search-desktop-restore))

;; Keep old names to avoid breakage.
(define-obsolete-function-alias 'elfeed-search-quit-window
  #'quit-window "3.4.2")
(define-obsolete-function-alias 'elfeed-search-update--force
  #'revert-buffer "3.4.2")

(provide 'elfeed-search)
;;; elfeed-search.el ends here
