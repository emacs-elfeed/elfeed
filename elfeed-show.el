;;; elfeed-show.el --- display feed entries -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>

;;; Commentary:

;; Code to display feed entries.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'shr)
(require 'eww)

(require 'elfeed)
(require 'elfeed-search)

(defface elfeed-show-entry-header-face
  '((t :inherit font-lock-keyword-face))
  "Face for showing headers in the elfeed-entry buffer."
  :group 'elfeed)

(defface elfeed-show-entry-title-face
  '((t :weight bold :inherit font-lock-string-face))
  "Face for showing the title name in the elfeed-entry buffer."
  :group 'elfeed)

(defface elfeed-show-entry-author-face
  '((t :weight bold :inherit font-lock-string-face))
  "Face for showing the author name in the elfeed-entry buffer."
  :group 'elfeed)

(defface elfeed-show-entry-date-face
  '((t :inherit font-lock-string-face))
  "Face for showing the date in the elfeed-entry buffer."
  :group 'elfeed)

(defface elfeed-show-entry-feed-face
  '((t :inherit font-lock-string-face))
  "Face for showing the feed name in the elfeed-entry buffer."
  :group 'elfeed)

(defface elfeed-show-entry-tags-face
  '((t :inherit font-lock-string-face))
  "Face for showing the tag names in the elfeed-entry buffer."
  :group 'elfeed)

(defcustom elfeed-show-truncate-long-urls t
  "When non-nil, use an ellipsis to shorten very long displayed URLs."
  :group 'elfeed
  :type 'boolean)

(defcustom elfeed-show-entry-author t
  "When non-nil, show the entry's author (if it's in the entry's metadata)."
  :group 'elfeed
  :type 'boolean)

(defvar elfeed-show-entry nil
  "The entry being displayed in this buffer.")

(defcustom elfeed-show-entry-switch #'switch-to-buffer
  "Function used to display the feed entry buffer."
  :group 'elfeed
  :type '(choice (function-item switch-to-buffer)
                 (function-item pop-to-buffer)
                 function))

(defcustom elfeed-show-entry-delete #'ignore
  "Function called when quitting from the elfeed-entry buffer.
Called without arguments."
  :group 'elfeed
  :type '(choice function))

(defcustom elfeed-show-entry-retrieve-content-link 180
  "Possible matches for entries which contents should be using eww retrieved.
Entries have to contain link property.
Match can be either the maximum length of content in characters,
a matching regular expression, a boolean or a list of possible matches."
  :group 'elfeed
  :type '(choice
          regexp
          integer
          boolean
          (repeat (choice regexp
                          integer))))

(defcustom elfeed-show-entry-retrieve-content-link-readability eww-readable-urls
  "Elfeed's variant of URL's where EWW's readability functions are applied.
Depends on `eww-readable'.  Defaults to `eww-readable-urls' if available."
  :group 'elfeed
  :link '(variable-link eww-readable-urls)
  :require #'eww-readable
  :type '(repeat (choice (string :tag "Readable URL")
                         (cons :tag "URL and Readability"
                               (string :tag "URL")
                               (radio (const :tag "Readable" t)
                                      (const :tag "Non-readable" nil))))))

(defvar-local elfeed-show-entry-readable nil
  "If true entry was made readable.")

(unless (fboundp #'eww-default-readable-p)
  (defun elfeed-default-readable-p (url)
    "Return non-nil if URL should be displayed in readable mode by default.
This consults the entries in
`elfeed-show-entry-retrieve-content-link-readability' (which see)."
    (catch 'found
      (let (result)
        (dolist (regexp eww-readable-urls)
          (if (consp regexp)
              (setq result (cdr regexp)
                    regexp (car regexp))
            (setq result t))
          (when (string-match regexp url)
            (throw 'found result))))))
  (defalias 'eww-default-readable-p 'elfeed-default-readable-p)
  (defalias 'eww-readable-urls nil))

;; Emacs < 31 compatibility
(with-no-warnings
  (if (fboundp 'eww-readable-dom)
      (defalias 'elfeed-readable-dom #'eww-readable-dom)
    (defun elfeed-readable-dom (dom)
      "Return a readable version of DOM."
      (eww-score-readability dom)
      (eww-highest-readability dom))))

(defvar elfeed-show-refresh-function #'elfeed-show-refresh--mail-style
  "Function called to refresh the `*elfeed-entry*' buffer.")

(defvar-keymap elfeed-show-mode-map
  :doc "Keymap for `elfeed-show-mode'."
  :parent special-mode-map
  "d" #'elfeed-show-save-enclosure
  "R" #'elfeed-show-refresh-togge-readable
  "n" #'elfeed-show-next
  "p" #'elfeed-show-prev
  "s" #'elfeed-show-new-live-search
  "b" #'elfeed-show-visit
  "B" #'elfeed-show-visit-secondary
  "y" #'elfeed-show-yank
  "u" #'elfeed-show-tag-unread
  "+" #'elfeed-show-tag
  "-" #'elfeed-show-untag
  "TAB" #'elfeed-show-next-link
  "M-TAB" #'shr-previous-link
  "<backtab>" #'shr-previous-link
  "c" #'elfeed-kill-link-url-at-point
  "<mouse-2>" #'shr-browse-url
  "A" #'elfeed-show-add-enclosure-to-playlist
  "P" #'elfeed-show-play-enclosure)

(easy-menu-define elfeed-show-mode-menu elfeed-show-mode-map
  "Menu for `elfeed-show-mode'."
  '("Elfeed Entry"
    ["Browse entry" elfeed-show-visit]
    ["Browse secondary" elfeed-show-visit-secondary]
    ["Copy URL" elfeed-show-yank]
    "--"
    ["Add tag" elfeed-show-tag]
    ["Remove tag" elfeed-show-untag]
    ["Mark as unread" elfeed-show-tag-unread]
    "--"
    ["Next entry" elfeed-show-next]
    ["Previous entry" elfeed-show-prev]
    "--"
    ["New live filter" elfeed-show-live-filter]
    "--"
    ("Enclosure"
     ["Save enclosure" elfeed-show-save-enclosure]
     ["Play enclosure" elfeed-show-play-enclosure]
     ["Add enclosure to playlist" elfeed-show-add-enclosure-to-playlist])
    "--"
    ["Revert buffer" revert-buffer]
    ["Quit window" quit-window]
    "--"
    ["Customize" (customize-group 'elfeed)]))

(define-derived-mode elfeed-show-mode special-mode "elfeed-show"
  "Mode for displaying Elfeed feed entries."
  :syntax-table nil :abbrev-table nil :interactive nil
  (buffer-disable-undo)
  (make-local-variable 'elfeed-show-entry)
  (setq-local bookmark-make-record-function
              #'elfeed-show-bookmark-make-record
              revert-buffer-function #'elfeed-show-refresh
              default-directory (elfeed-default-directory)))

(defun elfeed-show-tag-unread ()
  "Mark the current entry unread."
  (interactive nil elfeed-show-mode)
  (elfeed-show-tag 'unread))

(defun elfeeed--html-if-doctype (_headers response-buffer)
  "Return \"text/html\" if RESPONSE-BUFFER has an HTML doctype declaration.
HEADERS is unused."
  ;; https://html.spec.whatwg.org/multipage/syntax.html#the-doctype
  (with-current-buffer response-buffer
    (let ((case-fold-search t))
      (save-excursion
        (goto-char (point-min))
        ;; Match basic "<!doctype html>" and also legacy variants as
        ;; specified in link above -- being purposely lax about it.
        (when (search-forward "<!doctype html" nil t)
          "text/html")))))

(defalias 'elfeed--guess-cotent-type-headers
  (if (fboundp #'eww--guess-content-type)
      #'eww--guess-content-type))



(defvar-local elfeed--render-html-tick 0
  "Insert counter for the current buffer.
This counter helps protecting against inserting outdated images.")
(put 'elfeed--render-html-tick 'permanent-local t)

(defun elfeed-render (status url &optional point buffer encode explicit-content-type)
  (cl-letf* ((headers (eww-parse-headers))
	     (content-type (or
                        explicit-content-type
                        (mail-header-parse-content-type
                         (if (zerop (length (cdr (assoc "content-type" headers))))
                             (elfeed--guess-cotent-type-headers headers (current-buffer))
                           (cdr (assoc "content-type" headers))))))
	     (charset  (intern
		            (downcase
		             (or (cdr (assq 'charset (cdr content-type)))
			             (eww-detect-charset (eww-html-p (car content-type)))
			             "utf-8"))))
	     (data-buffer (current-buffer))
	     (shr-target-id (url-target (url-generic-parse-url url)))
         (tick (incf elfeed--render-html-tick))
         (orig (symbol-function 'url-queue-retrieve))
         ((symbol-function 'url-queue-retrieve)
          (lambda (url cb &rest args)
            (let ((cb (if (eq cb #'shr-image-fetched)
                          (lambda (status buffer &rest args)
                            (when (and (buffer-live-p buffer)
                                       (= tick
                                              (buffer-local-value
                                               'elfeed--render-html-tick buffer)))
                              (apply #'shr-image-fetched status buffer args)))
                        cb)))
              (apply orig url cb args))))
	     (last-coding-system-used))
    ;; Reset point after `eww-parse-headers' when CONTENT-TYPE was explicit
    ;; we assume explicit content type means a document without headers
    (when explicit-content-type
        (goto-char (point-min)))
    (let ((redirect (plist-get status :redirect)))
      (when redirect
        (setq url redirect)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Make buffer listings more informative.
        (setq list-buffers-directory url)
        ;; Let the URL library have a handle to the current URL for
        ;; referer purposes.
        (setq url-current-lastloc (url-generic-parse-url url)))
      (unwind-protect
	      (progn
	        (cond
             ((and eww-use-external-browser-for-content-type
                   (string-match-p eww-use-external-browser-for-content-type
                                   (car content-type)))
              (erase-buffer)
              (insert "<title>Unsupported content type</title>")
              (insert (format "<h1>Content-type %s is unsupported</h1>"
                              (car content-type)))
              (insert (format "<a href=%S>Direct link to the document</a>"
                              url))
              (goto-char (point-min))
              (elfeed-display-html (or encode charset)
                                   url nil point buffer))
	         ((equal (car content-type) "application/pdf")
	          (eww-display-pdf))
	         ((string-match-p "\\`image/" (car content-type))
	          (eww-display-image buffer))
             ((eww-html-p (car content-type))
              (elfeed-display-html (or encode charset)
                                   url nil point buffer))
	         (t
	          (elfeed-display-raw buffer (or encode charset 'utf-8))))
	        (with-current-buffer buffer
	          (plist-put eww-data :url url)
	          (and last-coding-system-used
		           (set-buffer-file-coding-system last-coding-system-used))
              (unless shr-fill-text
                (visual-line-mode)
                (visual-wrap-prefix-mode))
	          (run-hooks 'eww-after-render-hook)
              ;; Enable undo again so that undo works in text input
              ;; boxes.
              (setq buffer-undo-list nil)))
        (kill-buffer data-buffer)))
    (unless (buffer-live-p buffer)
      (kill-buffer data-buffer))))

(defun elfeed-display-html (charset url &optional document point buffer)
  (let ((source (buffer-substring (point) (point-max))))
    (with-current-buffer buffer
      (plist-put document :source source)))
  (unless document
    (let ((dom (eww--parse-html-region (point) (point-max) charset))
          (eww-readable-urls elfeed-show-entry-retrieve-content-link-readability))
      (when (with-current-buffer buffer
              (or (and elfeed-show-entry-readable
                       (not (eww-default-readable-p url)))
                  (and (eww-default-readable-p url)
                       (not elfeed-show-entry-readable))))
        (setq dom (elfeed-readable-dom dom))
        (setq elfeed-show-entry-readable t))
      (setq document (eww-document-base url dom))))
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (elfeed-display-document document point buffer))))

(defun elfeed-display-raw (buffer &optional encode)
  (let ((data (buffer-substring (point) (point-max))))
    (unless (buffer-live-p buffer)
      (error "Buffer %s doesn't exist" buffer))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(insert data)
	(condition-case nil
	    (decode-coding-region (point-min) (1+ (length data)) encode)
	  (coding-system-error nil)))
      (goto-char (point-min)))))

(cl-defun elfeed-insert-link (url &optional (content url))
  "Insert a clickable hyperlink to URL titled CONTENT."
  (when (and elfeed-show-truncate-long-urls
             (integerp shr-width)
             (> (length content) (- shr-width 8)))
    (let ((len (- (/ shr-width 2) 10)))
      (setq content (format "%s[...]%s"
                            (substring content 0 len)
                            (substring content (- len))))))
  (shr-tag-a `(a ((href . ,url)) ,content)))

(defun elfeed-compute-base (url)
  "Return the base URL for URL, useful for relative paths."
  (let ((obj (url-generic-parse-url url)))
    (setf (url-filename obj) nil)
    (setf (url-target obj) nil)
    (url-recreate-url obj)))

(defun elfeed--show-format-author (author)
  "Format AUTHOR plist for the header."
  (cl-destructuring-bind (&key name uri email &allow-other-keys)
      author
    (cond ((and name uri email)
           (format "%s <%s> (%s)" name email uri))
          ((and name email)
           (format "%s <%s>" name email))
          ((and name uri)
           (format "%s (%s)" name uri))
          (name name)
          (email email)
          (uri uri)
          ("[unknown]"))))

(defun elfeed-show-refresh--mail-style ()
  "Update the buffer to match the selected entry, using a mail-style."
  (let* ((inhibit-read-only t)
         (title (elfeed-entry-title elfeed-show-entry))
         (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (authors (elfeed-meta elfeed-show-entry :authors))
         (link (elfeed-entry-link elfeed-show-entry))
         (tags (elfeed-entry-tags elfeed-show-entry))
         (tagsstr (mapconcat #'symbol-name tags ", "))
         (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
         (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
         (type (elfeed-entry-content-type elfeed-show-entry))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (feed-title (elfeed-feed-title feed))
         (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
    (setq list-buffers-directory title)
    (erase-buffer)
    (insert (format (propertize "Title: %s\n" 'face 'elfeed-show-entry-header-face)
                    (propertize title 'face 'elfeed-show-entry-title-face)))
    (when elfeed-show-entry-author
      (dolist (author authors)
        (let ((formatted (elfeed--show-format-author author)))
          (insert
           (format (propertize "Author: %s\n" 'face 'elfeed-show-entry-header-face)
                   (propertize formatted 'face 'elfeed-show-entry-author-face))))))
    (insert (format (propertize "Date: %s\n" 'face 'elfeed-show-entry-header-face)
                    (propertize nicedate 'face 'elfeed-show-entry-date-face)))
    (insert (format (propertize "Feed: %s\n" 'face 'elfeed-show-entry-header-face)
                    (propertize feed-title 'face 'elfeed-show-entry-feed-face)))
    (when tags
      (insert (format (propertize "Tags: %s\n" 'face 'elfeed-show-entry-header-face)
                      (propertize tagsstr 'face 'elfeed-show-entry-tags-face))))
    (insert (propertize "Link: " 'face 'elfeed-show-entry-header-face))
    (elfeed-insert-link link link)
    (insert "\n")
    (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
             do (insert (propertize "Enclosure: " 'face 'elfeed-show-entry-header-face))
             do (elfeed-insert-link (car enclosure))
             do (insert "\n"))
    (insert "\n")
    (cond ((and elfeed-show-entry-retrieve-content-link
                (or
                 ;; regexp
                 (and (stringp elfeed-show-entry-retrieve-content-link)
                      (string-match elfeed-show-entry-retrieve-content-link
                                    link))
                 ;; number of characters
                 (and (integerp elfeed-show-entry-retrieve-content-link)
                      (or (not content)
                          (<= (length content) elfeed-show-entry-retrieve-content-link)))
                 ;; list of either above
                 (and (listp elfeed-show-entry-retrieve-content-link)
                      (let ((retrieve-conditions elfeed-show-entry-retrieve-content-link)
                            condition
                            (result t))
                        ;; Loop through until no conditions are left or found a result
                        (while (and result
                                    (setq condition (pop retrieve-conditions)))
                          (when (or (and
                                     ;; regexp
                                     (stringp condition)
                                     (string-match condition
                                                   link))
                                    ;; number of characters
                                    (and (integerp condition)
                                         (or (not content)
                                             (<= (length content)
                                                 condition))))
                            ;; Make while condition fail a result has been found
                            (setq result nil)))
                        ;; Invert result
                        (not result)))
                 ;; a true boolean (by this point it can only be true)
                 (booleanp elfeed-show-entry-retrieve-content-link)
                 nil))
           (eww-retrieve link #'elfeed-render (list link nil (current-buffer))))
          (content
           ;; FIXME: Elfeed doesn't really differentiate between content and description.
           ;; Some entries might claim to have content but they don't.
           ;; Calling `eww-retrieve' on those entries without content should be enough but yeah..
           (let ((content-buffer (current-buffer))
                 (elfeed-show-entry-retrieve-content-link-readability nil)
                 (content-type `(,(cond ((eq type 'html)
                                         "text/html")
                                        (t "text/plain"))
                                 (char-set . nil))))
             (with-temp-buffer
               (insert content)
               (elfeed-render nil link nil content-buffer nil content-type))))
          (t
           (insert (propertize "(empty)\n" 'face 'italic))
           (goto-char (point-min))))))

(defun elfeed-display-document (document &optional point buffer)
  (unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2"))
  (setq buffer (or buffer (current-buffer)))
  (unless (buffer-live-p buffer)
    (error "Buffer %s doesn't exist" buffer))
  ;; There should be a better way to abort loading images
  ;; asynchronously.
  (setq url-queue nil)
  (let ((url (when (eq (car document) 'base)
               (alist-get 'href (cadr document)))))
    (unless url
      (error "Document is missing base URL"))
    (with-current-buffer buffer
      (setq bidi-paragraph-direction nil)
      (let ((inhibit-read-only t)
	        (inhibit-modification-hooks t)
            ;; Possibly set by the caller, e.g., `eww-render' which
            ;; preserves the old URL #target before chasing redirects.
            (shr-target-id (or shr-target-id
                               (url-target (url-generic-parse-url url)))))
        (with-delayed-message (2 "Rendering HTML...")
	      (shr-insert-document document))
	    (cond
	     (point
	      (goto-char point))
	     (shr-target-id
	      (goto-char (point-min))
          (let ((match (text-property-search-forward
                        'shr-target-id shr-target-id #'member)))
            (when match
              (goto-char (prop-match-beginning match)))))
	     (t
	      (goto-char (point-min))
	      ;; Don't leave point inside forms, because the normal eww
	      ;; commands aren't available there.
	      (while (and (not (eobp))
		              (get-text-property (point) 'eww-form))
	        (forward-line 1)))))
      (eww-size-text-inputs))))

(defun elfeed-show-refresh (&rest _)
  "Update the buffer to match the selected entry.
Used as `revert-buffer-function'."
  (declare (completion ignore)) ;; Press "g" or M-x revert-buffer
  (interactive)
  (funcall elfeed-show-refresh-function))


(defun elfeed-show-refresh-togge-readable ()
    "Toggle display of only the main \"readable\" parts of the current web page.
This command uses heuristics to find the parts of the web page that
contain the main textual portion, leaving out navigation menus and the
like.

If called interactively, toggle the display of the readable parts.  If
the prefix argument is positive, display the readable parts, and if it
is zero or negative, display the full page.

If called from Lisp, toggle the display of the readable parts if ARG is
`toggle'.  Display the readable parts if ARG is nil, omitted, or is a
positive number.  Display the full page if ARG is a negative number."
    (interactive "" elfeed-show-mode)
    (setq elfeed-show-entry-readable (not elfeed-show-entry-readable))
    (call-interactively elfeed-show-refresh-function))

(defcustom elfeed-show-unique-buffers nil
  "When non-nil, every entry buffer gets a unique name.
This allows for displaying multiple show buffers at the same
time."
  :group 'elfeed
  :type 'boolean)

(defun elfeed-show--buffer-name (entry)
  "Return the appropriate buffer name for ENTRY.
The result depends on the value of `elfeed-show-unique-buffers'."
  (if elfeed-show-unique-buffers
      (format "*elfeed-entry-<%s %s>*"
              (elfeed-entry-title entry)
              (format-time-string "%F" (elfeed-entry-date entry)))
    "*elfeed-entry*"))

(defun elfeed-show-entry (entry)
  "Display ENTRY in the current buffer."
  (let ((buff (get-buffer-create (elfeed-show--buffer-name entry))))
    (with-current-buffer buff
      (elfeed-show-mode)
      (setq elfeed-show-entry entry)
      (elfeed-show-refresh))
    (funcall elfeed-show-entry-switch buff)))

(defun elfeed-show-next (&optional n)
  "Show the Nth next item in the `elfeed-search' buffer."
  (interactive "p" elfeed-show-mode)
  (funcall elfeed-show-entry-delete)
  (with-selected-window (or (get-buffer-window (elfeed-search-buffer))
                            (selected-window))
    (with-current-buffer (elfeed-search-buffer)
      (forward-line
       (- (or n 1) (if (elfeed-search--remain-on-entry-p 'show) 0 1)))
      (call-interactively #'elfeed-search-show-entry))))

(defun elfeed-show-prev (&optional n)
  "Show the Nth previous item in the `elfeed-search' buffer."
  (interactive "p" elfeed-show-mode)
  (elfeed-show-next (- (or n 1))))

(defun elfeed-show-new-live-search ()
  "Quit the current window, search again in the `elfeed-search' buffer."
  (interactive nil elfeed-show-mode)
  (quit-window)
  (elfeed-search)
  (elfeed-search-live-filter))

(defun elfeed-show-visit (&optional secondary)
  "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument SECONDARY, visit the current entry in
the browser defined by `browse-url-secondary-browser-function'."
  (interactive "P" elfeed-show-mode)
  (when-let* ((link (elfeed-entry-link elfeed-show-entry))
              ((elfeed--confirm-browse-url-p)))
    (message "Sent to browser: %s" link)
    (elfeed-browse-url link secondary)))

(defun elfeed-show-visit-secondary ()
  "Visit the current entry in your browser using the secondary browser."
  (interactive nil elfeed-show-mode)
  (elfeed-show-visit t))

(defun elfeed-show-yank ()
  "Copy the current entry link URL to the clipboard."
  (interactive nil elfeed-show-mode)
  (when-let* ((link (elfeed-entry-link elfeed-show-entry)))
    (kill-new link)
    (gui-set-selection 'PRIMARY link)
    (message "Yanked: %s" link)))

(defun elfeed-show-tag (&rest tags)
  "Add TAGS to the displayed entry."
  (interactive (elfeed-search--prompt-tags "Tag: ") elfeed-show-mode)
  (let ((entry elfeed-show-entry))
    (apply #'elfeed-tag entry tags)
    (with-current-buffer (elfeed-search-buffer)
      (elfeed-search-update-entry entry))
    (elfeed-show-refresh)))

(defun elfeed-show-untag (&rest tags)
  "Remove TAGS from the displayed entry."
  (interactive
   (elfeed-search--prompt-tags "Untag: " elfeed-show-entry)
   elfeed-show-mode)
  (let ((entry elfeed-show-entry))
    (apply #'elfeed-untag entry tags)
    (with-current-buffer (elfeed-search-buffer)
      (elfeed-search-update-entry entry))
    (elfeed-show-refresh)))

;; Enclosures:

(defcustom elfeed-enclosure-default-dir "~/"
  "Default directory for saving enclosures.
This can be either a string (a file system path), or a function
that takes a filename and the mime-type as arguments, and returns
the enclosure dir."
  :type 'directory
  :group 'elfeed
  :safe 'stringp)

(defcustom elfeed-save-multiple-enclosures-without-asking nil
  "If non-nil, saving multiple enclosures asks once for a directory.
All attachments are saved in the chosen directory."
  :type 'boolean
  :group 'elfeed)

(defvar elfeed-show-enclosure-filename-function
  #'elfeed-show-enclosure-filename-remote
  "Function called to generate the filename for an enclosure.")

(defun elfeed--download-enclosure (url path)
  "Download asynchronously the enclosure from URL to PATH."
  (if elfeed-use-curl
      (make-process
       :name "elfeed-curl"
       :command
       (list elfeed-curl-program-name
             "--disable" "--fail" "--location" "--silent"
             "--user-agent" elfeed-user-agent "-o" (expand-file-name path)
             url)
       :sentinel
       (lambda (_proc status)
         (if (string-prefix-p "finished" status)
             (message "Enclosure download %s finished (%s)" path url)
           (message "Enclosure download %s failed (%s)" path url))))
    (url-copy-file url path t))
  nil)

(defun elfeed--get-enclosure-num (prompt entry &optional multi)
  "Ask the user with PROMPT for an enclosure number for ENTRY.
The number is [1..n] for enclosures \[0..(n-1)] in the entry.  If MULTI
is nil, return the number for the enclosure; otherwise (MULTI is
non-nil), accept ranges of enclosure numbers, as per
`elfeed-split-ranges-to-numbers', and return the corresponding string."
  (let* ((count (length (elfeed-entry-enclosures entry)))
         def)
    (when (zerop count)
      (error "No enclosures to this entry"))
    (if (not multi)
        (if (= count 1)
            (read-number (format "%s: " prompt) 1)
          (read-number (format "%s (1-%d): " prompt count)))
      (setq def (if (= count 1) "1" (format "1-%d" count)))
      (read-string (format "%s (default %s): " prompt def)
                   nil nil def))))

(defun elfeed--request-enclosure-path (fname path)
  "Ask the user where to save FNAME (default is PATH/FNAME)."
  (let ((fpath (expand-file-name
                (read-file-name "Save as: " path nil nil fname) path)))
    (if (file-directory-p fpath)
        (expand-file-name fname fpath)
      fpath)))

(defun elfeed--request-enclosures-dir (path)
  "Ask the user where to save multiple enclosures (default is PATH)."
  (let ((fpath (expand-file-name
                (read-directory-name
                 (format "Save in directory: ") path nil nil nil) path)))
    (if (file-directory-p fpath)
        fpath)))

(defun elfeed-show-enclosure-filename-remote (_entry url-enclosure)
  "Returns the remote filename as local filename for URL-ENCLOSURE."
  (file-name-nondirectory
   (url-unhex-string
    (car (url-path-and-query (url-generic-parse-url
                              url-enclosure))))))

(defun elfeed-show-save-enclosure-single (&optional entry enclosure-index)
  "Save enclosure number ENCLOSURE-INDEX from ENTRY.
If ENTRY is nil use the variable `elfeed-show-entry'.
If ENCLOSURE-INDEX is nil ask for the enclosure number."
  (interactive nil elfeed-show-mode)
  (let* ((path (expand-file-name elfeed-enclosure-default-dir))
         (entry (or entry elfeed-show-entry))
         (enclosure-index (or enclosure-index
                              (elfeed--get-enclosure-num
                               "Enclosure to save" entry)))
         (url-enclosure (car (elt (elfeed-entry-enclosures entry)
                                  (- enclosure-index 1))))
         (fname
          (funcall elfeed-show-enclosure-filename-function
                   entry url-enclosure))
         (retry t)
         (fpath))
    (while retry
      (setf fpath (elfeed--request-enclosure-path fname path)
            retry (and (file-exists-p fpath)
                       (not (y-or-n-p (format "Overwrite '%s'?" fpath))))))
    (elfeed--download-enclosure url-enclosure fpath)))

(defun elfeed-show-save-enclosure-multi (&optional entry)
  "Offer to save multiple entry enclosures from ENTRY.
ENTRY defaults to the current entry.
Default is to save all enclosures, [1..n], where n is the number of
enclosures.  You can type multiple values separated by space, e.g.
  1 3-6 8
will save enclosures 1,3,4,5,6 and 8.

Furthermore, there is a shortcut \"a\" which so means all
enclosures, but as this is the default, you may not need it."
  (interactive nil elfeed-show-mode)
  (let* ((entry (or entry elfeed-show-entry))
         (attachstr (elfeed--get-enclosure-num
                     "Enclosure number range (or 'a' for 'all')" entry t))
         (count (length (elfeed-entry-enclosures entry)))
         (attachnums (elfeed-split-ranges-to-numbers attachstr count))
         (path (expand-file-name elfeed-enclosure-default-dir))
         (fpath))
    (if elfeed-save-multiple-enclosures-without-asking
        (let ((attachdir (elfeed--request-enclosures-dir path)))
          (dolist (enclosure-index attachnums)
            (let* ((url-enclosure
                    (aref (elfeed-entry-enclosures entry) enclosure-index))
                   (fname
                    (funcall elfeed-show-enclosure-filename-function
                             entry url-enclosure))
                   (retry t))
              (while retry
                (setf fpath (expand-file-name (concat attachdir fname) path)
                      retry
                      (and (file-exists-p fpath)
                           (not (y-or-n-p (format "Overwrite '%s'?" fpath))))))
              (elfeed--download-enclosure url-enclosure fpath))))
      (dolist (enclosure-index attachnums)
        (elfeed-show-save-enclosure-single entry enclosure-index)))))

(defun elfeed-show-save-enclosure (&optional multi)
  "Offer to save enclosure(s).
If MULTI (prefix-argument) is nil, save a single one, otherwise,
offer to save a range of enclosures."
  (interactive "P" elfeed-show-mode)
  (if multi
      (elfeed-show-save-enclosure-multi)
    (elfeed-show-save-enclosure-single)))

(defun elfeed--enclosure-maybe-prompt-index (entry)
  "Prompt for an enclosure if there are multiple in ENTRY."
  (if (= 1 (length (elfeed-entry-enclosures entry)))
      1
    (elfeed--get-enclosure-num "Enclosure to play" entry)))

(defun elfeed-show-play-enclosure (enclosure-index)
  "Play enclosure number ENCLOSURE-INDEX from current entry using EMMS.
Prompts for ENCLOSURE-INDEX when called interactively."
  (interactive (list (elfeed--enclosure-maybe-prompt-index elfeed-show-entry))
               elfeed-show-mode)
  (elfeed-show-add-enclosure-to-playlist enclosure-index)
  (eval '(with-current-emms-playlist
          (save-excursion
            (emms-playlist-last)
            (emms-playlist-mode-play-current-track)))))

(defun elfeed-show-add-enclosure-to-playlist (enclosure-index)
  "Add enclosure number ENCLOSURE-INDEX to current EMMS playlist.
Prompts for ENCLOSURE-INDEX when called interactively."
  (interactive (list (elfeed--enclosure-maybe-prompt-index elfeed-show-entry))
               elfeed-show-mode)
  (require 'emms)
  (declare-function emms-add-url "emms")
  (emms-add-url (car (elt (elfeed-entry-enclosures elfeed-show-entry)
                          (- enclosure-index 1)))))

(defun elfeed-show-next-link ()
  "Skip to the next link, exclusive of the Link header."
  (interactive nil elfeed-show-mode)
  (let ((properties (text-properties-at (line-beginning-position))))
    (when (memq 'elfeed-show-entry-header-face properties)
      (forward-paragraph))
    (shr-next-link)))

(defun elfeed-kill-link-url-at-point ()
  "Get link URL at point and store in `kill-ring'."
  (interactive nil elfeed-show-mode)
  (let ((url (or (elfeed-get-link-at-point)
                 (thing-at-point-url-at-point))))
    (if url
        (progn (kill-new url) (message "%s" url))
      (call-interactively #'shr-copy-url))))

;; Bookmarks

;;;###autoload
(defun elfeed-show-bookmark-handler (record)
  "Show the bookmarked entry saved in the `RECORD'."
  (let* ((id (bookmark-prop-get record 'id))
         (entry (elfeed-db-get-entry id))
         (position (bookmark-get-position record)))
    (elfeed-show-entry entry)
    (goto-char position)))
(put 'elfeed-show-bookmark-handler 'bookmark-handler-type "Elfeed Entry")

(defun elfeed-show-bookmark-make-record ()
  "Save the current position and the entry into a bookmark."
  (let ((id (elfeed-entry-id elfeed-show-entry))
        (position (point))
        (title (elfeed-entry-title elfeed-show-entry)))
    `(,(format "elfeed entry \"%s\"" title)
      (id . ,id)
      (location . ,title)
      (position . ,position)
      (handler . elfeed-show-bookmark-handler))))

(provide 'elfeed-show)
;;; elfeed-show.el ends here
