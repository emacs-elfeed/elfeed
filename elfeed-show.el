;;; elfeed-show.el --- display feed entries -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>

;;; Commentary:

;; Code to display feed entries.

;;; Code:

(eval-when-compile (require 'subr-x))

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

(defvar-local elfeed-show-entry nil
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

(defcustom elfeed-show-entry-date-format "%a, %e %b %Y %T %Z"
  "The ‘format-time-string’ format for date in the elfeed-entry buffer."
  :group 'elfeed
  :type 'string)

(defvar elfeed-show-refresh-function #'elfeed-show-refresh--mail-style
  "Function called to refresh the `*elfeed-entry*' buffer.")

(defvar elfeed-show-refresh-hook (list #'elfeed-show-auto-fetch-link)
 "Functions in this list are called after the show buffer has been refreshed.")

(defvar-keymap elfeed-show-mode-map
  :doc "Keymap for `elfeed-show-mode'."
  :parent special-mode-map
  "f" #'elfeed-show-fetch-link
  "d" #'elfeed-show-save-enclosure
  "n" #'elfeed-show-next
  "p" #'elfeed-show-prev
  "s" #'elfeed-search-new-live
  "b" #'elfeed-show-visit
  "B" #'elfeed-show-visit-secondary
  "y" #'elfeed-show-yank
  "u" #'elfeed-show-tag-unread
  "+" #'elfeed-show-tag
  "-" #'elfeed-show-untag
  "m" #'elfeed-show-compose-mail
  "TAB" #'elfeed-show-next-link
  "M-TAB" #'shr-previous-link
  "<backtab>" #'shr-previous-link
  "c" #'elfeed-kill-link-url-at-point
  "<mouse-2>" #'shr-browse-url
  "A" #'elfeed-show-add-enclosure-to-playlist
  "P" #'elfeed-show-play-enclosure
  "SPC" #'elfeed-show-scroll-up-or-next
  "S-SPC" #'elfeed-show-scroll-down-or-prev
  "DEL" #'elfeed-show-scroll-down-or-prev)

(easy-menu-define elfeed-show-mode-menu elfeed-show-mode-map
  "Menu for `elfeed-show-mode'."
  '("Elfeed Entry"
    ["Browse entry" elfeed-show-visit]
    ["Browse secondary" elfeed-show-visit-secondary]
    ["Copy URL" elfeed-show-yank]
    ["Fetch link" elfeed-show-fetch-link]
    "--"
    ["Add tag" elfeed-show-tag]
    ["Remove tag" elfeed-show-untag]
    ["Tag as unread" elfeed-show-tag-unread]
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
  (setq-local bookmark-make-record-function #'elfeed-show-bookmark-make-record
              revert-buffer-function #'elfeed-show-refresh
              mode-line-modified nil
              mode-line-mule-info nil
              mode-line-remote nil
              elfeed-db--kill-on-unload t
              default-directory (elfeed-default-directory)))

(defun elfeed-show-tag-unread ()
  "Tag the current entry as unread."
  (interactive nil elfeed-show-mode)
  (elfeed-show-tag 'unread))

(defun elfeed-show--format-author (author)
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
         (nicedate (format-time-string elfeed-show-entry-date-format date))
         (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
         (type (elfeed-entry-content-type elfeed-show-entry))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (feed-title (elfeed-meta--title feed))
         (base (and feed (elfeed-compute-base
                          (or (elfeed-meta elfeed-show-entry :base-url)
                              (elfeed-feed-url feed))))))
    (setq list-buffers-directory title)
    (erase-buffer)
    (insert (format
             (propertize "Title: %s\n" 'face 'elfeed-show-entry-header-face)
             (if (or (not title) (equal title ""))
                 (propertize "(Untitled)" 'face '(elfeed-show-entry-title-face italic))
               (propertize title 'face 'elfeed-show-entry-title-face))))
    (when elfeed-show-entry-author
      (dolist (author authors)
        (insert
         (format (propertize "Author: %s\n" 'face 'elfeed-show-entry-header-face)
                 (propertize (elfeed-show--format-author author)
                             'face 'elfeed-show-entry-author-face)))))
    (insert (format (propertize "Date: %s\nFeed: %s\n" 'face 'elfeed-show-entry-header-face)
                    (propertize nicedate 'face 'elfeed-show-entry-date-face)
                    (propertize feed-title 'face 'elfeed-show-entry-feed-face)))
    (when tags
      (insert (format (propertize "Tags: %s\n" 'face 'elfeed-show-entry-header-face)
                      (propertize tagsstr 'face 'elfeed-show-entry-tags-face))))
    (insert (propertize "Link: " 'face 'elfeed-show-entry-header-face))
    (elfeed-insert-link link nil elfeed-show-truncate-long-urls)
    (insert "\n")
    (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
             do (insert (propertize "Enclosure: " 'face 'elfeed-show-entry-header-face))
             do (elfeed-insert-link (car enclosure) nil elfeed-show-truncate-long-urls)
             do (insert "\n"))
    (insert "\n")
    (if (and content (not (string-blank-p content)))
        (if (eq type 'html)
            (elfeed-insert-html content base)
          (insert content))
      (insert (propertize "(empty)\n" 'face 'italic 'elfeed-link-content t)))
    (goto-char (point-min))))

(defun elfeed-show-refresh (&rest _)
  "Update the buffer to match the selected entry.
Used as `revert-buffer-function'."
  (declare (completion ignore)) ;; Press "g" or M-x revert-buffer
  (interactive)
  (funcall elfeed-show-refresh-function)
  (run-hooks 'elfeed-show-refresh-hook))

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
  (let ((buffer (get-buffer-create (elfeed-show--buffer-name entry))))
    (with-current-buffer buffer
      (elfeed-show-mode)
      (setq elfeed-show-entry entry)
      (elfeed-show-refresh))
    (funcall elfeed-show-entry-switch buffer)
    ;; Scroll to top after reset
    (when-let* ((win (get-buffer-window buffer)))
      (set-window-start win (point-min)))))

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

(defun elfeed-show-scroll-up-or-next ()
  "Scroll-up the current entry or go to the next entry."
  (interactive nil elfeed-show-mode)
  (condition-case nil
      (scroll-up-command)
    (error (elfeed-show-next))))

(defun elfeed-show-scroll-down-or-prev ()
  "Scroll-down the current entry or go to the previous entry."
  (interactive nil elfeed-show-mode)
  (condition-case nil
      (scroll-down-command)
    (error
     (elfeed-show-prev)
     (with-no-warnings (end-of-buffer)))))

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
  (when (apply #'elfeed-tag elfeed-show-entry tags)
    (let ((entry elfeed-show-entry))
      (with-current-buffer (elfeed-search-buffer)
        (elfeed-search-update-entry entry)))
    (elfeed-show-refresh)))

(defun elfeed-show-untag (&rest tags)
  "Remove TAGS from the displayed entry."
  (interactive
   (elfeed-search--prompt-tags "Untag: " elfeed-show-entry)
   elfeed-show-mode)
  (when (apply #'elfeed-untag elfeed-show-entry tags)
    (let ((entry elfeed-show-entry))
      (with-current-buffer (elfeed-search-buffer)
        (elfeed-search-update-entry entry)))
    (elfeed-show-refresh)))

;; Enclosures:

(defcustom elfeed-enclosure-default-dir "~/"
  "Default directory for saving enclosures.
This can be either a string (a file system path), or a function
that takes a filename and the mime-type as arguments, and returns
the enclosure dir."
  :type 'directory
  :group 'elfeed)

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

(defun elfeed-show-compose-mail ()
  "Compose mail from elfeed-entry buffer."
  (interactive nil elfeed-show-mode)
  (declare-function message-goto-body "message")
  (declare-function message-goto-to "message")
  (let ((show-buffer (current-buffer))
        (title (elfeed-entry-title elfeed-show-entry))
        (link (elfeed-entry-link elfeed-show-entry)))
    (compose-mail nil title)
    (message-goto-body)
    (insert (format "You may find this interesting:\n%s\n\n" link))
    (let ((beg (point)))
      (insert-buffer-substring show-buffer)
      (fill-region (point) (point-max))
      (comment-region beg (point)))
    (message-goto-to)))

;; Link fetching

(defvar elfeed-show--fetch nil
  "Active fetch processes.")

(cl-defun elfeed-show--fetch (cb &key url key headers force)
  "Fetch link content, store result in the entry metadata and call CB.
The callback CB is called with the URL and content string on success in
the show buffer.  URL defaults to the entry link.  The content is stored
under KEY, defaulting to :link-content in the entry metadata.  HEADERS
are optional HTTP headers.  If FORCE is non-nil do not use cached
content."
  (unless elfeed-use-curl
    (error "elfeed-show--fetch requires curl"))
  (setq url (or url
                (when-let* ((entry elfeed-show-entry))
                  (elfeed-entry-link entry))
                (error "No link to fetch"))
        key (or key :link-content))
  (ignore-errors
    (kill-process (alist-get key elfeed-show--fetch))
    (setf (alist-get key elfeed-show--fetch) nil))
  (if-let* ((content (and (not force) (elfeed-deref (elfeed-meta elfeed-show-entry key)))))
      (funcall cb url content)
    (setf (alist-get key elfeed-show--fetch)
          (elfeed-curl-retrieve
           url
           (let ((buffer (current-buffer))
                 (entry elfeed-show-entry))
             (lambda (status)
               (setf (alist-get key elfeed-show--fetch) nil)
               (unless (elfeed-status-error-p status)
                 (let ((content (buffer-string)))
                   (setf (elfeed-meta entry key) (elfeed-ref content))
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (when (eq entry elfeed-show-entry)
                         (funcall cb url content))))))))
           :headers headers))))

(defun elfeed-show-fetch-link (&optional force)
  "Fetch link content and insert it into the show buffer.
If the prefix argument FORCE is non-nil, force refetching.  The fetched
content is stored in the entry metadata under the key :link-content."
  (interactive "P" elfeed-show-mode)
  (elfeed-show--fetch
   (lambda (url content)
     (let ((inhibit-read-only t))
       (save-excursion
         (when-let* ((pos (text-property-any (point-min) (point-max)
                                             'elfeed-link-content t)))
           (delete-region pos (point-max)))
         (goto-char (point-max))
         (insert (propertize "\n" 'elfeed-link-content t))
         (elfeed-insert-html content (elfeed-compute-base url)))))
   :force force))

(defun elfeed-show-auto-fetch-link ()
  "Automatically fetch link content if the feed is marked with `:fetch-link'."
  (when (elfeed-meta (elfeed-entry-feed elfeed-show-entry) :fetch-link)
    (elfeed-show-fetch-link)))

;; Bookmarks

;;;###autoload
(defun elfeed-show-bookmark-handler (record)
  "Show the bookmarked entry saved in the `RECORD'."
  (let ((elfeed-show-entry-switch #'switch-to-buffer))
    (elfeed-show-entry (elfeed-db-get-entry (bookmark-prop-get record 'id))))
  (goto-char (bookmark-get-position record)))
(put 'elfeed-show-bookmark-handler 'bookmark-handler-type "Elfeed")

(defun elfeed-show-bookmark-make-record ()
  "Save the current position and the entry into a bookmark."
  (let ((title (elfeed-meta--title elfeed-show-entry)))
    `(,(format "elfeed entry \"%s\"" title)
      (id . ,(elfeed-entry-id elfeed-show-entry))
      (location . ,title)
      (position . ,(point))
      (handler . ,#'elfeed-show-bookmark-handler))))

(define-obsolete-function-alias 'elfeed-show-new-live-search
  #'elfeed-search-new-live "4.0.0")

(provide 'elfeed-show)
;;; elfeed-show.el ends here
