;;; elfeed-search.el --- list feed entries -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>

;;; Commentary:

;; The `elfeed-search' buffer which lists feed entries and supports live
;; filtering.  Open the search buffer via the command `elfeed-search'.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'hl-line)
(require 'wid-edit) ; widget-inactive face

(require 'bookmark)
(bookmark-maybe-load-default-file)

(require 'elfeed)

(defvar-local elfeed-search-entries ()
  "List of the entries currently on display.")

(defvar elfeed-search-filter-history nil
  "Filter history for `completing-read'.")

(defvar elfeed-search--update-timer nil
  "Timer to debounce search buffer updates.")

(defvar elfeed-search--resize-timer nil
  "Timer to debounce search window resizing.")

(define-obsolete-variable-alias 'elfeed-search-last-update
  'elfeed-search--last-update "4.0.0")
(defvar elfeed-search--last-update 0
  "The last time the buffer was redrawn in epoch seconds.")

(defvar elfeed-search--last-width 0
  "The last window width.")

(defvar elfeed-search--marked nil
  "List of marked entries.")

(defvar elfeed-search-update-hook (list #'elfeed-search-add-separators)
  "List of functions to run immediately following a search buffer update.
The functions may modify the search buffer or add overlays, for example
`elfeed-search-add-separators'.")

(defcustom elfeed-search-update-delay 0.2
  "Delay search buffer updates to avoid redundant redraws.
The delay is in seconds."
  :group 'elfeed
  :type 'number)

(defcustom elfeed-search-filter "@6months +unread"
  "Query string filtering shown entries."
  :group 'elfeed
  :type 'string
  :local t)

(defcustom elfeed-search-completion t
  "Enable tag and search filter completion."
  :group 'elfeed
  :type 'boolean)

(define-obsolete-variable-alias 'elfeed-sort-order
  'elfeed-search-sort-order "4.0.0")

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
live filter editing is exited.

The variable can also be set to a list of functions (or nil for the
default function) such that you can cycle between the function via the
command `elfeed-search-cycle-order'."
  :group 'elfeed
  :type `(choice
          (const :tag "Group by feed" ,#'elfeed-search-group-by-feed)
          (function :tag "Custom function")
          (const :tag "Default sorting" nil)
          (repeat :tag "List of functions" function)))

(defun elfeed-search--sort-function ()
  "Get sort function."
  (when-let* ((sort elfeed-search-sort-function))
    (if (functionp sort) sort (car sort))))

(defun elfeed-search-group-by-feed (a b)
  "Group entries A and B by feed."
  (unless (equal (elfeed-entry-feed-id a) (elfeed-entry-feed-id b))
    (string-collate-lessp (elfeed-meta--title (elfeed-entry-feed a))
                          (elfeed-meta--title (elfeed-entry-feed b))
                          nil t)))

(defcustom elfeed-search-remain-on-entry nil
  "When non-nil, keep point at entry after performing a command.

When nil, always move to next entry after a command.  The variable can
be set to a list of symbols show, browse, tag, mark and yank, such that
point does not move after the listed operations.  Example:

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

(defcustom elfeed-search-separator-date-format "%b %Y"
  "The `format-time-string' format for date separators."
  :group 'elfeed
  :type '(choice (const nil) string))

(defcustom elfeed-search-compile-filter t
  "If non-nil, compile search filters into bytecode on the fly."
  :group 'elfeed
  :type 'boolean)

(defvar elfeed-search-header-function #'elfeed-search--header
  "Function that returns the string to be used for the header line.")

(defvar elfeed-search-print-entry-function #'elfeed-search-print-entry--default
  "Function to print entries into the *elfeed-search* buffer.")

(define-obsolete-variable-alias 'elfeed-search-filter-active
  'elfeed-search--filter-active "4.0.0")
(defvar elfeed-search--filter-active nil
  "When non-nil, Elfeed is currently reading a filter from the minibuffer.
When live editing the filter, it is bound to :live.")

(define-obsolete-variable-alias 'elfeed-search-filter-overflowing
  'elfeed-search--filter-overflowing "4.0.0")
(defvar elfeed-search--filter-overflowing nil
  "When non-nil, the current live filter overflows the window.")

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
  "=" #'elfeed-search-feed-filter
  "@" #'elfeed-search-date-filter
  "<elfeed-date>" #'elfeed-search-date-filter
  "<elfeed-entry>" #'elfeed-search-show-entry
  "<elfeed-feed>" #'elfeed-search-feed-filter
  "<elfeed-tag>" #'elfeed-search-tag-filter
  "<header-line> <mouse-1>" #'elfeed-search-header-button
  "o" #'elfeed-search-cycle-order
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
  "m" #'elfeed-search-mark
  "M" #'elfeed-search-unmark
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
    ["Tag as unread" elfeed-search-tag-all-unread]
    ["Tag as read" elfeed-search-untag-all-unread]
    "--"
    ["Live filter" elfeed-search-live-filter]
    ("Filter"
     ["Current day only" elfeed-search-date-filter]
     ["Current feed only" elfeed-search-feed-filter]
     ["Set filter" elfeed-search-set-filter]
     ["Clear filter" elfeed-search-clear-filter])
    ["Cycle sort order" elfeed-search-cycle-order]
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
     ["Unjam queue" elfeed-unjam]
     ["Show log" elfeed-log-show])
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
             (or (eq action 'mark) (not elfeed-search--marked))
             (not (elfeed-search--remain-on-entry-p action)))
    (forward-line)))

(defun elfeed-search--header-button (command &optional text help)
  "Create header line button for COMMAND with optional TEXT and HELP."
  (propertize (or text (symbol-name command))
              'elfeed-header-button command
              'help-echo (or help (format "Run %s" command))
              'mouse-face 'highlight))

(defun elfeed-search--intro-header ()
  "Return the header shown to new users."
  (concat "Database empty. Use "
          (elfeed-search--header-button #'elfeed-add-feed) ", or "
          (elfeed-search--header-button #'elfeed-load-opml) ", or "
          (elfeed-search--header-button #'elfeed-update) "."))

(defun elfeed-search--count-unread ()
  "Count the number of entries and feeds being currently displayed."
  (cl-loop with feeds = (make-hash-table :test #'equal)
           for entry in elfeed-search-entries
           count entry into entry-count
           count (elfeed-tagged-p 'unread entry) into unread-count
           do (puthash (elfeed-feed-url (elfeed-entry-feed entry)) t feeds)
           finally return
           (elfeed-search--header-button
            #'elfeed-search-fetch-visible
            (format
             (propertize "%d/%d:%d" 'face 'elfeed-search-unread-count-face)
             unread-count entry-count
             (hash-table-count feeds)))))

(defun elfeed-search--log-button ()
  "Button to show the Elfeed log."
  (when (> elfeed-log-error-count 0)
    (concat (elfeed-search--header-button
             #'elfeed-log-show
             (format (propertize "(%d)" 'face 'error)
                     elfeed-log-error-count))
            " ")))

(defun elfeed-search--header (&optional hide-filter)
  "Computes the string to be used as the Elfeed header.
Hide filter and unread counter if HIDE-FILTER is non-nil."
  (cond
   ((not elfeed-db) "Database not loaded.")
   ((zerop (elfeed-db-last-update))
    (elfeed-search--intro-header))
   ((let ((total (elfeed-queue-count-total)))
      (when (> total 0)
        (let ((active (elfeed-queue-count-active)))
          (concat
           (elfeed-search--log-button)
           (format "%d jobs pending, %d active…"
                   (- total active) active))))))
   ((let* ((update (elfeed-db-last-update))
           (delta (- (float-time) update))
           (update (if (> delta elfeed-search-last-update-relative)
                       (concat "Updated "
                               (elfeed-add-properties
                                (format-time-string "%Y-%m-%d %H:%M"
                                                    (seconds-to-time update))
                                'face 'elfeed-search-last-update-face))
                     (format "Updated %s ago"
                             (elfeed-add-properties
                              (if (< delta 60)
                                  "< 1 minute"
                                (compat-call seconds-to-string delta t))
                              'face 'elfeed-search-last-update-face))))
           (unread (cond
                    (hide-filter nil)
                    ((and elfeed-search--filter-active
                          elfeed-search--filter-overflowing)
                     (propertize "?/?:?" 'face 'elfeed-search-unread-count-face))
                    (t (elfeed-search--count-unread))))
           (filter (when (and (not hide-filter)
                              (not elfeed-search--filter-active)
                              (string-match-p "[^ ]" elfeed-search-filter))
                     (elfeed-add-properties
                      (mapconcat
                       (lambda (x)
                         (elfeed-search--header-button
                          (lambda ()
                            (interactive)
                            (elfeed-search--toggle-filter x))
                          x (format "Remove filter %s" x)))
                       (split-string elfeed-search-filter) " ")
                      'face 'elfeed-search-filter-face))))
      (concat
       (elfeed-search--log-button)
       (elfeed-search--header-button #'elfeed-update update)
       (and unread ", ") unread (and filter ", ") filter)))))

(defun elfeed-search--header-line-format (fun)
  "Generate `header-line-format' from FUN."
  ;; Provide format string via symbol value slot so that it will
  ;; not be %-construct interpolated. The symbol is uninterned
  ;; so that it's not *really* a global variable.
  (setq header-line-format
        (let ((sym (make-symbol (symbol-name fun))))
          (put sym 'risky-local-variable t)
          `(:eval
            (prog1 ',sym
              (set ',sym (funcall ,fun)))))))

(define-derived-mode elfeed-search-mode special-mode "elfeed-search"
  "Major mode for listing elfeed feed entries."
  :syntax-table nil :abbrev-table nil :interactive nil
  (setq-local truncate-lines t
              mode-line-modified nil
              mode-line-mule-info nil
              mode-line-remote nil
              desktop-save-buffer #'elfeed-search-desktop-save
              bookmark-make-record-function #'elfeed-search-bookmark-make-record
              revert-buffer-function #'elfeed-search--update-force
              default-directory (elfeed-default-directory)
              hl-line-sticky-flag t)
  (elfeed-search--header-line-format 'elfeed-search-header-function)
  (buffer-disable-undo)
  (hl-line-mode)
  (add-hook 'elfeed-update-hook #'elfeed-search--update-debounce)
  (add-hook 'elfeed-update-init-hook #'elfeed-search--update-force)
  (add-hook 'window-size-change-functions #'elfeed-search--resize nil 'local)
  (elfeed-db--save-on-quit)
  (elfeed-search-update :force))

;;;###autoload
(defun elfeed-search (&optional new-filter)
  "Enter `elfeed-search' buffer, optionally with a NEW-FILTER."
  (interactive)
  (switch-to-buffer (elfeed-search-buffer))
  (unless (eq major-mode 'elfeed-search-mode)
    (elfeed-search-mode))
  (when new-filter
    ;; Scroll to top when resetting the buffer
    (goto-char (point-min))
    (set-window-start nil (point-min))
    (elfeed-search-set-filter new-filter)))

(defun elfeed-search-buffer ()
  "Create and return search buffer."
  (get-buffer-create "*elfeed-search*"))

(defun elfeed-search-format-date (date)
  "Format DATE for printing in `elfeed-search-mode'.
The customization `elfeed-search-date-format' sets the formatting."
  (cl-destructuring-bind (format width align) elfeed-search-date-format
    (elfeed-format-column (format-time-string format (seconds-to-time date))
                          width align)))

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

(defface elfeed-search-marked-face
  '((t :inherit (lazy-highlight bold)))
  "Face for marked entries."
  :group 'elfeed)

(put 'elfeed-search-marked-overlay 'face 'elfeed-search-marked-face)

(defface elfeed-search-separator-face
  '((t :inherit (bold elfeed-search-date-face) :underline t :extend t))
  "Face for marked entries."
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

(defcustom elfeed-search-last-update-relative (* 60 60 24 14)
  "Maximum relative last update time in seconds.
Set to `most-positive-fixnum' to always use a relative time, or 0 to
never show a relative time."
  :type 'boolean
  :group 'elfeed)

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
                 'mouse-face 'highlight 'elfeed-tag s
                 'follow-link [elfeed-tag]))
   tags ","))

(defun elfeed-search-format-column--date (entry)
  "Format the date information for ENTRY."
  (let ((date (elfeed-search-format-date (elfeed-entry-date entry)))
        (date-width (cadr elfeed-search-date-format)))
    (cons
     (elfeed-add-properties date
                            'face 'elfeed-search-date-face
                            'mouse-face 'highlight
                            'follow-link [elfeed-date])
     date-width)))

(defun elfeed-search-format-column--title (entry)
  "Format the title information for ENTRY."
  (let* ((tags (elfeed-entry-tags entry))
         (title (elfeed-meta--title entry))
         (title (if (or (not title) (equal title ""))
                    (elfeed-entry-link entry)
                  title))
         (title-faces (elfeed-search--faces tags))
         (window (get-buffer-window))
         (title-width (elfeed-clamp
                       elfeed-search-title-min-width
                       (- (if window (window-width window) (frame-width))
                          10 elfeed-search-trailing-width)
                       elfeed-search-title-max-width))
         (title-column (elfeed-format-column title title-width :left)))
    (cons
     (elfeed-add-properties title-column
                            'face title-faces 'kbd-help title
                            'mouse-face 'highlight
                            'follow-link [elfeed-entry])
     title-width)))

(defun elfeed-search-format-column--feed (entry)
  "Format the feed information for ENTRY."
  (if-let* ((feed (elfeed-entry-feed entry))
            (feed-title (and feed (elfeed-meta--title feed))))
      (propertize feed-title 'face 'elfeed-search-feed-face
                  'mouse-face 'highlight
                  'follow-link [elfeed-feed])))

(defun elfeed-search-format-column--tags (entry)
  "Format the tags information for ENTRY."
  (if-let ((tags (elfeed-entry-tags entry)))
      (concat "(" (elfeed-search--format-tags tags) ")")))

(defun elfeed-search-print-entry--default (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-column--date entry))
         (date-str (car date))
         (date-width (cdr date))
         (title (elfeed-search-format-column--title entry))
         (title-str (car title))
         (title-width (cdr title))
         (feed (elfeed-search-format-column--feed entry))
         (tags (elfeed-search-format-column--tags entry)))
    (insert
     (concat
      date-str
      (propertize " " 'display `(space :align-to ,(1+ date-width)))
      title-str
      (when feed
        (concat
         (propertize " " 'display `( space :align-to
                                     ,(+ 2 date-width title-width)))
         feed))
      (when tags (concat " " tags))))))

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
         ;; Static candidates (tags and history).
         (unless (equal input "")
           (let ((all
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
                       elfeed-search-filter-history))))))
             (if (string-match-p "\\`[=~!#@]" input)
                 ;; Keep only candidates matching the special prefix character.
                 (let (completion-regexp-list completion-ignore-case)
                   (all-completions (substring input 0 1) all))
               all))))))))

(defun elfeed-search--prompt (current &optional live)
  "Prompt for a new filter starting from CURRENT, optionally with LIVE update."
  (unless (or (equal "" current) (string-suffix-p " " current))
    (setq current (concat current " ")))
  (let ((elfeed-search--filter-active (if live :live :non-interactive)))
    (minibuffer-with-setup-hook
        (lambda ()
          (set-syntax-table elfeed-search-filter-syntax-table)
          (when live
            (add-hook 'post-command-hook #'elfeed-search--live-update nil 'local)))
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
         "Filter: " current nil nil 'elfeed-search-filter-history)))))

(defun elfeed-search--prompt-tags (prompt &optional entry-or-entries-list)
  "Prompt for tags in the minibuffer.
PROMPT is the prompt string and ENTRY-OR-ENTRIES-LIST an entry or a list
of entries to take the tags from."
  (let* ((all-tags
          (or (if entry-or-entries-list
                  (delete-dups
                   (copy-sequence
                    (apply #'append
                           (mapcar #'elfeed-entry-tags
                                   (ensure-list entry-or-entries-list)))))
                (elfeed-db-get-all-tags))
              (user-error "No tags found")))
         (all-tags (mapcar #'symbol-name all-tags))
         (initial (when entry-or-entries-list
                    (if (length= all-tags 1)
                        (car all-tags)
                      (let ((tag (get-text-property (point) 'elfeed-tag)))
                        (and tag (symbolp tag) (symbol-name tag))))))
         (require-match (not (not entry-or-entries-list)))
         (tags (if elfeed-search-completion
                   (completing-read-multiple
                    prompt
                    (completion-table-with-metadata
                     all-tags
                     '((category . elfeed-tag)))
                    nil require-match initial)
                 (split-string (read-from-minibuffer prompt initial)
                               "[ \t]*,[ \t]*" t))))
    (unless tags
      (user-error "No tags given!"))
    (mapcar #'intern tags)))

(defun elfeed-search-clear-filter ()
  "Reset the search filter.
Use the default value of the variable `elfeed-search-filter'."
  (interactive nil elfeed-search-mode)
  (elfeed-search-set-filter))

(defun elfeed-search-set-filter (&optional new-filter)
  "Set a new search filter for the `elfeed-search' buffer.

When NEW-FILTER is nil, reset the filter to the default value.

When given a prefix argument, the current filter is not displayed
in the minibuffer when prompting for a new filter.

Any component beginning with a + or - is treated as a tag.  If +
the tag must be present on the entry.  If - the tag must *not* be
present on the entry.  Examples: \"+unread\" or \"+unread -comic\".

Any component beginning with an @ is an age limit or an age
range.  If a limit, no posts older than this are allowed.  If a
range, posts dates have to be in-between the specified date
range.  Examples:
- \"@3days\"
- \"@1year\"
- \"@3-days-ago\"
- \"@1-year-old\"
- \"@2019-06-24\"
- \"@2019-06-24--2019-06-24\"
- \"@5-days-ago--1-day-ago\"

Any component beginning with a # is an entry count maximum.  The
number following # determines the maximum number of entries
to be shown (descending by date).  Examples: \"#20\" or \"#100\".

Any component beginning with a = is a regular expression matching
the entry's feed (title or URL). Only entries belonging to a feed
that match at least one of the = expressions will be shown.

Every other space-separated element is treated like a regular
expression, matching against entry link, title, and feed title."
  (interactive
   (list (elfeed-search--prompt
          (if current-prefix-arg "" elfeed-search-filter)))
   elfeed-search-mode)
  (with-current-buffer (elfeed-search-buffer)
    (setf elfeed-search-filter
          (or new-filter (default-value 'elfeed-search-filter)))
    (elfeed-search-update :force)))

(defun elfeed-search-entries (filter &optional sort ascending)
  "Search database with FILTER string and return a list of matching entries.
The entries are sorted by the entry comparison function SORT.  The result
is reversed if ASCENDING is non-nil."
  (let* ((filter (elfeed-search-parse-filter filter))
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
      (when sort (setq entries (sort entries sort)))
      (when ascending (setq entries (nreverse entries)))
      entries)))

(defun elfeed-search--update-list ()
  "Update the variable `elfeed-search-entries' and `elfeed-search--last-update'."
  (setq elfeed-search-entries (elfeed-search-entries
                               elfeed-search-filter
                               (elfeed-search--sort-function)
                               (eq elfeed-search-sort-order 'ascending)))
  (cl-callf2 cl-delete-if-not
      (lambda (x) (memq x elfeed-search-entries))
      elfeed-search--marked)
  (setq elfeed-search--last-update (float-time)
        list-buffers-directory elfeed-search-filter))

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
:resize to preserve the entries and redraw.  Do not use this function
directly.  Instead use `elfeed-search-update'."
  (when (and (buffer-live-p buffer)
             (or (eq method :force)
                 (eq method :resize)
                 (and (not elfeed-search--filter-active)
                      (< elfeed-search--last-update (elfeed-db-last-update)))))
    ;; Run inside window such that save excursion moves the window point.
    (with-selected-window (or (get-buffer-window buffer) (selected-window))
      ;; If no window is found, we still have to execute in the buffer.
      (with-current-buffer buffer
        (elfeed-with-position elfeed-entry
          (let ((inhibit-read-only t)
                (standard-output (current-buffer)))
            (erase-buffer)
            (remove-overlays nil nil 'category 'elfeed-search-marked-overlay)
            (unless (eq method :resize)
              (elfeed-search--update-list))
            (dolist (entry elfeed-search-entries)
              (elfeed-search--print-entry entry)
              (insert ?\n))
            (mapc #'elfeed-search--make-marked-overlay elfeed-search--marked)))
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

(defun elfeed-search--resize (win)
  "Resize search window WIN.
The function is used as hook."
  (when (/= (window-width win) elfeed-search--last-width)
    (when elfeed-search--resize-timer
      (cancel-timer elfeed-search--resize-timer)
      (setq elfeed-search--resize-timer nil))
    (setf elfeed-search--last-width (window-width win)
          elfeed-search--resize-timer
          (run-at-time elfeed-search-update-delay nil
                       #'elfeed-search--update-immediately
                       (elfeed-search-buffer) :resize))))

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
  (when (> (elfeed-queue-count-total) 0)
    (user-error "Update already running"))
  (elfeed-log 'info "Elfeed visible update: %s"
              (format-time-string "%B %e %Y %H:%M:%S %Z"))
  (run-hooks 'elfeed-update-init-hook)
  (cl-loop with seen = (make-hash-table :test #'equal)
           for entry in elfeed-search-entries
           for url = (elfeed-feed-url (elfeed-entry-feed entry))
           unless (gethash url seen)
           do (elfeed--update-feed (setf (gethash url seen) url))))

(defun elfeed-search--update-line (&optional n)
  "Redraw line N, defaulting to the current line."
  (let ((inhibit-read-only t))
    (save-excursion
      (when n (elfeed-goto-line n))
      (when-let* ((entry (elfeed-search-selected :ignore-region)))
        (elfeed-search--remove-marked-overlay entry)
        (delete-region (pos-bol) (pos-eol))
        (elfeed-search--print-entry entry)
        (when (memq entry elfeed-search--marked)
          (elfeed-search--make-marked-overlay entry))))))

(defun elfeed-search-update-entry (&rest entries)
  "Redraw ENTRIES in the `elfeed-search' buffer."
  (if (length> entries 100)
      (elfeed-search-update :force)
    (cl-loop for entry in entries
             for n = (cl-position entry elfeed-search-entries)
             when n do (elfeed-search--update-line (1+ n)))))

(defun elfeed-search--remove-marked-overlay (entry)
  "Remove mark overlay from ENTRY."
  (when-let* ((n (cl-position entry elfeed-search-entries)))
    (save-excursion
      (elfeed-goto-line (1+ n))
      (remove-overlays (pos-bol) (pos-eol)
                       'category 'elfeed-search-marked-overlay))))

(defun elfeed-search--make-marked-overlay (entry)
  "Add mark overlay over ENTRY."
  (when-let* ((n (cl-position entry elfeed-search-entries)))
    (save-excursion
      (elfeed-goto-line (1+ n))
      (overlay-put (make-overlay (pos-bol) (pos-eol))
                   'category 'elfeed-search-marked-overlay))))

(defun elfeed-search-mark (&rest entries)
  "Mark ENTRIES at point."
  (interactive (elfeed-search-selected :ignore-marked)
               elfeed-search-mode)
  (dolist (entry entries)
    (cl-pushnew entry elfeed-search--marked)
    (elfeed-search--make-marked-overlay entry))
  (elfeed-search--after-action 'mark))

(defun elfeed-search-unmark (&rest entries)
  "Unmark ENTRIES at point.
With prefix argument, unmark all currently marked entries."
  (interactive (if current-prefix-arg
                   elfeed-search--marked
                 (elfeed-search-selected :ignore-marked))
               elfeed-search-mode)
  (dolist (entry entries)
    (cl-callf2 delq entry elfeed-search--marked)
    (elfeed-search--remove-marked-overlay entry))
  (unless current-prefix-arg
    (elfeed-search--after-action 'mark)))

(defun elfeed-search-selected (&optional ignore-region)
  "Return a list of the currently selected feeds.

If IGNORE-REGION is :ignore-marked, do not return marked entries.
Otherwise if IGNORE-REGION is non-nil, only return the entry under
point."
  (let* ((ignore-marked (eq ignore-region :ignore-marked))
         (ignore-region (and (not ignore-marked) ignore-region)))
    (cond
     ((and (not ignore-region) (not ignore-marked) elfeed-search--marked))
     ((and (not ignore-region) (use-region-p))
      (let ((start (region-beginning))
            (end   (region-end)))
        (save-excursion
          (goto-char start)
          (goto-char (pos-bol))
          (cl-loop while (< (point) end)
                   when (prog1 (get-text-property (point) 'elfeed-entry)
                          (forward-line))
                   collect it into selected
                   finally return selected))))
     ((when-let* ((entry (get-text-property (pos-bol) 'elfeed-entry)))
        (if ignore-region entry (list entry)))))))

(defun elfeed-search-browse-url (&optional secondary)
  "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument SECONDARY, visit the current entry in
the browser defined by `browse-url-secondary-browser-function'."
  (interactive "P" elfeed-search-mode)
  (let ((entries (elfeed-search-selected)))
    (when (elfeed--confirm-browse-url-p (length entries))
      (cl-loop for entry in entries
               when (elfeed-entry-link entry)
               do (progn
                    (elfeed-untag entry 'unread)
                    (elfeed-browse-url it secondary)))
      ;; `browse-url' could have switched to another buffer if eww or another
      ;; internal browser is used, but the remainder of the functions needs to
      ;; run in the elfeed buffer.
      (with-current-buffer (elfeed-search-buffer)
        (apply #'elfeed-search-update-entry entries)
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
      (kill-new links-str)
      (gui-set-selection elfeed-search-clipboard-type links-str)
      (message "Copied: %s" links-str)
      (elfeed-search--after-action 'yank))))

(defun elfeed-search--tag (fun)
  "Call FUN with the entries to tag and update the entries afterwards."
  (let* ((all current-prefix-arg)
         (entries (if all elfeed-search-entries (elfeed-search-selected))))
    (apply #'elfeed-search-update-entry (funcall fun entries))
    (unless all
      (elfeed-search--after-action 'tag))))

(defun elfeed-search-tag-all (&rest tags)
  "Apply TAGS to all selected entries."
  (interactive (elfeed-search--prompt-tags "Tag: ") elfeed-search-mode)
  (elfeed-search--tag
   (lambda (entries)
     (apply #'elfeed-tag entries tags))))

(defun elfeed-search-untag-all (&rest tags)
  "Remove TAGS from all selected entries."
  (interactive (elfeed-search--prompt-tags
                "Untag: "
                (if current-prefix-arg
                    elfeed-search-entries
                  (elfeed-search-selected)))
   elfeed-search-mode)
  (elfeed-search--tag
   (lambda (entries)
     (apply #'elfeed-untag entries tags))))

(defun elfeed-search-toggle-all (&rest tags)
  "Toggle TAGS on all selected entries."
  (interactive (elfeed-search--prompt-tags "Toggle: ") elfeed-search-mode)
  (elfeed-search--tag
   (lambda (entries)
     (dolist (tag tags entries)
       (let (entries-tag entries-untag)
         (cl-loop for entry in entries
                  when (elfeed-tagged-p tag entry)
                  do (push entry entries-untag)
                  else do (push entry entries-tag))
         (elfeed-tag entries-tag tag)
         (elfeed-untag entries-untag tag))))))

(defun elfeed-search-show-entry (entry &optional preview)
  "Display the currently selected ENTRY in a buffer.
If the prefix argument PREVIEW is non-nil, do not mark the entry as
read.  If the feed specifies an alternative `:show-entry' function,
e.g., `browse-url', this function will be called with the entry link as
argument."
  (interactive (list (or (elfeed-search-selected :ignore-region)
                         (user-error "No entry selected!"))
                     current-prefix-arg)
               elfeed-search-mode)
  (when (and (not preview) (elfeed-untag entry 'unread))
    (elfeed-search-update-entry entry))
  (elfeed-search--after-action 'show)
  ;; Update hl-line overlay. This does not happen automatically, since
  ;; `elfeed-show-entry' switches to another buffer.
  (hl-line-highlight)
  (push-mark nil 'nomsg)
  (if-let* ((feed (elfeed-entry-feed entry))
            (show (elfeed-meta feed :show-entry))
            (link (elfeed-entry-link entry)))
      (funcall show link)
    ;; elfeed-show.el is required by elfeed.el at runtime.
    (declare-function elfeed-show-entry "elfeed-show")
    (elfeed-show-entry entry)))

(defun elfeed-search--toggle-filter (str)
  "Toggle STR filter in variable `elfeed-search-filter'."
  (let ((filter (split-string elfeed-search-filter)))
    (elfeed-search-set-filter
     (string-join (if (member str filter)
                      (delete str filter)
                    (append filter (list str)))
                  " "))))

(defun elfeed-search--date-filter (date)
  "Create filter string which matches a DATE."
  (format-time-string "@%Y-%m-%d" (seconds-to-time date)))

(defun elfeed-search--tag-filter (tag-or-tags-list)
  "Create filter string which matches a TAG-OR-TAGS-LIST."
  (mapconcat (lambda (x) (format "+%s" x))
             (ensure-list tag-or-tags-list) " "))

(defun elfeed-search--feed-filter (feed)
  "Create filter string which matches FEED."
  (concat "=" (string-replace "\\." "." (regexp-quote (elfeed-feed-id feed)))))

(defun elfeed-search-date-filter ()
  "Toggle date filter from date at point."
  (interactive nil elfeed-search-mode)
  (when-let* ((entry (get-text-property (pos-bol) 'elfeed-entry)))
    (elfeed-search--toggle-filter (elfeed-search--date-filter
                                   (elfeed-entry-date entry)))))

(defun elfeed-search-tag-filter ()
  "Toggle tag filter from tag at point."
  (interactive nil elfeed-search-mode)
  (when-let* ((tag (get-text-property (point) 'elfeed-tag)))
    (elfeed-search--toggle-filter (elfeed-search--tag-filter tag))))

(defun elfeed-search-feed-filter ()
  "Toggle feed filter from feed at point."
  (interactive nil elfeed-search-mode)
  (when-let* ((entry (get-text-property (pos-bol) 'elfeed-entry)))
    (elfeed-search--toggle-filter (elfeed-search--feed-filter
                                   (elfeed-entry-feed entry)))))

(defun elfeed-search-header-button ()
  "Handle click on the header line of the search buffer."
  (declare (completion ignore))
  (interactive "@")
  (when-let* (((mouse-event-p last-input-event))
              (pos (event-end last-input-event))
              (str (posn-string pos))
              (button (get-text-property
                       (cdr str) 'elfeed-header-button (car str))))
    (call-interactively button)))

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
    (modify-syntax-entry ?+ "w" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?~ "w" table)
    (modify-syntax-entry ?= "w" table)
    (modify-syntax-entry ?@ "w" table)
    (modify-syntax-entry ?# "w" table)
    table)
  "Syntax table active when editing the filter in the minibuffer.")

(defun elfeed-search--live-update ()
  "Update the `elfeed-search' buffer based on the contents of the minibuffer."
  (when (eq :live elfeed-search--filter-active)
    (let ((current-filter (minibuffer-contents-no-properties)))
      (with-current-buffer (elfeed-search-buffer)
        (let* ((window (get-buffer-window))
               (height (window-total-height window))
               (limiter (if window (format "#%d " height) "#1 "))
               (elfeed-search-filter (concat limiter current-filter)))
          (elfeed-search-update :force)
          (setf elfeed-search--filter-overflowing
                (length= elfeed-search-entries height)))))))

(defun elfeed-search-live-filter ()
  "Filter the `elfeed-search' buffer as the filter is written."
  (interactive nil elfeed-search-mode)
  (unwind-protect
      (setq elfeed-search-filter
            (elfeed-search--prompt elfeed-search-filter :live))
    (elfeed-search-update :force)))

(defun elfeed-search-new-live ()
  "Quit the current window, search again in the `elfeed-search' buffer."
  (interactive nil elfeed-show-mode elfeed-tree-mode)
  (quit-window)
  (elfeed-search)
  (elfeed-search-live-filter))

(defun elfeed-search-cycle-order ()
  "Cycle between different sort functions."
  (interactive nil elfeed-search-mode)
  (setq-local elfeed-search-sort-function
              (pcase elfeed-search-sort-function
                (`nil (list #'elfeed-search-group-by-feed nil))
                ((pred functionp) (list nil elfeed-search-sort-function))
                (_ (cons (car (last elfeed-search-sort-function))
                         (butlast elfeed-search-sort-function)))))
  (elfeed-search-update :force))

;; Separators in search display

;; Default separator title function
(put nil 'elfeed-search-separator
     (lambda (entry)
       (when elfeed-search-separator-date-format
         (format-time-string elfeed-search-separator-date-format
                             (seconds-to-time (elfeed-entry-date entry))))))

;; Separator title function for `elfeed-search-group-by-feed'
(put #'elfeed-search-group-by-feed 'elfeed-search-separator
     (lambda (entry)
       (elfeed-meta--title (elfeed-entry-feed entry))))

(defun elfeed-search--separator-title (entry)
  "Format separator title for ENTRY."
  (let ((sort (elfeed-search--sort-function)))
    (funcall (or (and (symbolp sort) (get sort 'elfeed-search-separator))
                 (get nil 'elfeed-search-separator))
             entry)))

(defun elfeed-search-add-separators ()
  "Add separators to the search buffer."
  (let ((last nil) (ov nil) (count 0))
    (remove-overlays (point-min) (point-max)
                     'category 'elfeed-search-separator)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((entry (get-text-property (point) 'elfeed-entry))
                    (title (elfeed-search--separator-title entry))
                    ((not (equal title last))))
          (incf count)
          (setq ov (make-overlay (pos-bol) (pos-bol)))
          (overlay-put ov 'category 'elfeed-search-separator)
          (overlay-put ov 'before-string
                       (concat (and last "\n")
                               (propertize (concat title "\n")
                                           'face 'elfeed-search-separator-face)))
          (setq last title))
        (forward-line)))
    ;; Delete unnecessary separator again if there is only a single one.
    (when (= count 1) (delete-overlay ov))))

;; Bookmarks

;;;###autoload
(defun elfeed-search-bookmark-handler (record)
  "Jump to an `elfeed-search' bookmark RECORD."
  (elfeed-search (bookmark-prop-get record 'location)))
(put 'elfeed-search-bookmark-handler 'bookmark-handler-type "Elfeed")

(defun elfeed-search-bookmark-make-record ()
  "Return a bookmark record for the current `elfeed-search' buffer."
  `(,(format "elfeed %s" elfeed-search-filter)
    (location . ,elfeed-search-filter)
    (tags ,@(plist-get (elfeed-search-parse-filter elfeed-search-filter) :must-have))
    (handler . ,#'elfeed-search-bookmark-handler)))

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
  (elfeed-search search-filter)
  (current-buffer))

;;;###autoload
(add-to-list 'desktop-buffer-mode-handlers
             '(elfeed-search-mode . elfeed-search-desktop-restore))

;; Keep old names to avoid breakage.
(define-obsolete-function-alias 'elfeed-search-quit-window
  #'quit-window "4.0.0")
(define-obsolete-function-alias 'elfeed-search-update--force
  #'revert-buffer "4.0.0")
(define-obsolete-function-alias 'elfeed-search-update-line
  #'elfeed-search--update-line "4.0.0")

(provide 'elfeed-search)
;;; elfeed-search.el ends here
