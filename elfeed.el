;;; elfeed.el --- An Atom/RSS feed reader -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; Maintainer: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>, Ihor Radchenko <yantar92@posteo.net>, Daniel Mendler <mail@daniel-mendler.de>
;; URL: https://github.com/emacs-elfeed/elfeed
;; Version: 4.1.0
;; Package-Requires: ((emacs "29.1") (compat "31"))
;; Keywords: network, comm, hypermedia

;;; Commentary:

;; Elfeed is a web feed client for Emacs, which supports Atom, RSS
;; and JSON feeds.
;;
;; After installation of Elfeed from a package archive, invoke M-x
;; elfeed to open the Elfeed search buffer.  The list of feeds can be
;; configured in your user configuration.
;;
;;     (setq elfeed-feeds
;;       '(("https://nullprogram.com/feed/" blog emacs)
;;         "https://sachachua.com/blog/category/emacs-news/feed/" ;; no autotagging
;;         ("https://nedroid.com/feed/" webcomic)))
;;
;; For the start a few basic commands suffice.  Inside the search
;; buffer press G to refresh the feeds, s to live filter the entries,
;; or c to reset the search filter.  Press RET on an entry to show it
;; in a separate buffer.
;;
;; See the README for the full documentation.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'xml-query)
(require 'url-queue)

(require 'elfeed-db)
(require 'elfeed-lib)
(require 'elfeed-log)
(require 'elfeed-curl)

(defgroup elfeed ()
  "An Emacs web feed reader."
  :link '(info-link :tag "Info Manual" "(elfeed)")
  :link '(url-link :tag "Website" "https://github.com/emacs-elfeed/elfeed")
  :link '(url-link :tag "Wiki" "https://github.com/emacs-elfeed/elfeed/wiki")
  :link '(emacs-library-link :tag "Library Source" "elfeed.el")
  :group 'web
  :group 'comm)

(defconst elfeed-version "4.0.1"
  "The Elfeed version, used for example by `elfeed-user-agent'.")

(defcustom elfeed-entry-point 'elfeed-search
  "Entry point for the command `elfeed'.
You can set this either to the search or the tree command, or an
alternative custom command if you use another custom frontend."
  :type '(choice (const :tag "Search buffer" elfeed-search)
                 (const :tag "Tree buffer" elfeed-tree)
                 (command :tag "Custom command")))

(defcustom elfeed-feed-functions
  (list #'elfeed-get-link-at-point
        #'thing-at-point-url-at-point
        #'elfeed-clipboard-get)
  "List of functions to use to get possible feeds for `elfeed-add-feed'.
Each function should accept no arguments, and return a string or nil."
  :type 'hook
  :options (list #'elfeed-get-link-at-point
                 #'thing-at-point-url-at-point
                 #'elfeed-clipboard-get))

(defcustom elfeed-use-curl
  (if (executable-find elfeed-curl-program-name)
      t
    (warn "Elfeed: curl is not available, falling back to `url-retrieve'")
    nil)
  "If non-nil, fetch feeds using curl instead of `url-retrieve'."
  :type 'boolean)

(defcustom elfeed-use-libxml nil
  "Use faster libxml2 for feed parsing.
This setting is experimental, and disabled for now.  It may lead to
subtle differences to the usual xml.el parser, which renders certain
feeds unreadable.  Enabling may yield a performance boost."
  :type 'boolean)

(defcustom elfeed-user-agent (format "Emacs Elfeed %s" elfeed-version)
  "User agent string to use for Elfeed (requires `elfeed-use-curl')."
  :type 'string)

(defcustom elfeed-initial-tags '(unread)
  "Initial tags for new entries."
  :type '(repeat symbol))

(defcustom elfeed-confirm-browse-url 2
  "Confirm `browse-url' when opening this many or more URLs."
  :type '(choice (const :tag "No confirmation" nil)
                 (const :tag "Always confirm" t)
                 natnum))

(defun elfeed--confirm-browse-url-p (&optional count)
  "Confirm browsing COUNT URLs, with COUNT defaulting to 1."
  (let ((confirm elfeed-confirm-browse-url)
        (count (or count 1)))
    (or (not confirm)
        (and (numberp confirm) (< count confirm))
        (y-or-n-p (if (= count 1)
                      "Browse URL? "
                    (format "Browse %d URLs? " count))))))

(defcustom elfeed-default-directory nil
  "Default directory for all Elfeed buffers."
  :type '(choice (const :tag "current" nil)
                 (const :tag "user home" "~/")
                 directory))

(defun elfeed-default-directory ()
  "Return default directory to be used by Elfeed buffers."
  (if (and elfeed-default-directory
           (file-exists-p elfeed-default-directory))
      (file-name-as-directory (expand-file-name elfeed-default-directory))
    default-directory))

(defcustom elfeed-last-update-relative (* 60 60 24 14)
  "Maximum relative last update time in seconds in the header line.
Set to `most-positive-fixnum' to always use a relative time, or 0 to
never show a relative time."
  :type 'boolean)

;; Completion predicate for commands which should be available in all Elfeed
;; mode buffers.
;;;###autoload
(progn
  (defun elfeed--mode-p (_sym buf)
    (let ((mode (buffer-local-value 'major-mode buf)))
      (and (symbolp mode) (string-prefix-p "elfeed-" (symbol-name mode))))))

;; Fetching:

(define-obsolete-variable-alias 'elfeed-http-error-hooks 'elfeed-http-error-hook "4.0.0")
(define-obsolete-variable-alias 'elfeed-parse-error-hooks 'elfeed-parse-error-hook "4.0.0")
(define-obsolete-variable-alias 'elfeed-update-hooks 'elfeed-update-hook "4.0.0")
(define-obsolete-variable-alias 'elfeed-update-init-hooks 'elfeed-update-init-hook "4.0.0")
(define-obsolete-variable-alias 'elfeed-tag-hooks 'elfeed-tag-hook "4.0.0")
(define-obsolete-variable-alias 'elfeed-untag-hooks 'elfeed-untag-hook "4.0.0")

(defvar elfeed-http-error-hook ()
  "Hooks to run when an http connection error occurs.
It is called with 2 arguments.  The first argument is the url of
the failing feed.  The second argument is the http status code.")

(defvar elfeed-parse-error-hook ()
  "Hooks to run when an error occurs during the parsing of a feed.
It is called with 2 arguments.  The first argument is the url of
the failing feed.  The second argument is the error message .")

(defvar elfeed-transform-html-functions nil
  "List of functions to transform HTML before insertion.
Each function takes a DOM and returns the modified DOM.  For example the
DOM can be cleaned up with a readability filter.")

(defvar elfeed-fetch-functions (list #'elfeed-fetch-url)
  "Abnormal hooks to run when fetching feeds.
It is called with 2 arguments: the URL of the feed, and a callback
function.  The hook must return non-nil if it handles the URL and the
other hooks that come after in the list are not called.  When fetching
completed, the callback function must be called with a single result
argument.  Result can either be the keyword :error in case of error, the
keyword :success in case of success, the keyword :parse to parse feed
XML or JSON in the current buffer at point and add the resulting entries
to the database.")

(defvar elfeed-update-hook ()
  "Hooks to run any time a feed update has completed a request.
It is called with 1 argument: the URL of the feed that was just
updated.  The hook is called even when no new entries were
found.")

(defvar elfeed-update-init-hook ()
  "Hooks called when one or more feed updates have begun.
Receivers may want to, say, update a display to indicate that
updates are pending.")

(defvar elfeed-tag-hook ()
  "Hooks called when one or more entries add tags.
It is called with 2 arguments.  The first argument is the entry
list.  The second argument is the tag list.")

(defvar elfeed-untag-hook ()
  "Hooks called when one or more entries remove tags.
It is called with 2 arguments.  The first argument is the entry
list.  The second argument is the tag list.")

(defun elfeed-queue-count-active ()
  "Return the number of items in process."
  (if elfeed-use-curl
      elfeed-curl-queue-active
    (cl-count-if #'url-queue-buffer url-queue)))

(defun elfeed-queue-count-total ()
  "Return the number of items in process."
  (if elfeed-use-curl
      (+ (length elfeed-curl-queue) elfeed-curl-queue-active)
    (length url-queue)))

(defun elfeed-set-max-connections (n)
  "Limit the maximum number of concurrent connections to N."
  (if elfeed-use-curl
      (setf elfeed-curl-max-connections n)
    (setf url-queue-parallel-processes n)))

(defun elfeed-get-max-connections ()
  "Get the maximum number of concurrent connections."
  (if elfeed-use-curl
      elfeed-curl-max-connections
    url-queue-parallel-processes))

(defun elfeed-set-timeout (seconds)
  "Limit the time for fetching a feed to SECONDS."
  (if elfeed-use-curl
      (setf elfeed-curl-timeout seconds)
    (setf url-queue-timeout seconds)))

(defun elfeed-get-timeout ()
  "Get the time limit for fetching feeds in SECONDS."
  (if elfeed-use-curl
      elfeed-curl-timeout
    url-queue-timeout))

(defun elfeed-unjam ()
  "Manually clear the connection pool when connections fail to timeout.
This is a workaround for issues in `url-queue-retrieve'."
  (declare (completion elfeed--mode-p))
  (interactive)
  (setq elfeed-log-error-count 0)
  (elfeed-log 'info "Unjamming queue")
  (if elfeed-use-curl
      (setf elfeed-curl-queue nil
            elfeed-curl-queue-active 0)
    (when-let* ((fails (mapcar #'url-queue-url url-queue)))
      (elfeed-log 'warn "Aborted feeds: %s" (string-join fails " ")))
    (setf url-queue nil))
  (run-hooks 'elfeed-update-init-hook))

;; Parsing:

(defun elfeed-feed-type (content)
  "Obtain the feed type (:atom, :rss, :rss1.0) or nil for unknown from CONTENT."
  (let ((top (xml-query-strip-ns (caar content))))
    (cadr (assoc top '((feed :atom)
                       (rss :rss)
                       (RDF :rss1.0))))))

(defun elfeed-generate-id (&optional content)
  "Generate an ID based on CONTENT or from the current time."
  (concat "urn:sha1:" (sha1 (format "%s" (or content (float-time))))))

(defun elfeed--atom-content (entry)
  "Get content string from ENTRY.
If there is no content tag, use summary instead."
  (let ((content-type (or (xml-query* (content :type) entry)
                          (xml-query* (summary :type) entry))))
    (if (equal content-type "xhtml")
        (with-temp-buffer
          (let ((xhtml (cddr (or (xml-query* (content) entry)
                                 (xml-query* (summary) entry)))))
            (dolist (element xhtml)
              (if (stringp element)
                  (insert element)
                (elfeed-xml-unparse element))))
          (buffer-string))
      (when-let* ((all-content
                   (or (xml-query-all* (content *) entry)
                       (xml-query-all* (summary *) entry))))
        (apply #'concat all-content)))))

(defvar elfeed-new-entry-parse-hook '()
  "Hook to be called after parsing a new entry.

Take three arguments: the feed TYPE, the XML structure for the
entry, and the Elfeed ENTRY object.  Return value is ignored, and
is called for side-effects on the ENTRY object.")

(defsubst elfeed--fixup-protocol (protocol url)
  "Prepend PROTOCOL to URL if it is protocol-relative.
If PROTOCOL is nil, returns URL."
  (if (and protocol url (string-match-p "\\`//[^/]" url))
      (concat protocol ":" url)
    url))

(defun elfeed--json-authors-to-plist (obj)
  "Parse list of authors from json OBJ into list of plists."
  (cl-loop for author in (or (alist-get 'authors obj)
                             (delq nil (list (alist-get 'author obj))))
           for name = (alist-get 'name author)
           for url = (alist-get 'url author)
           collect `(,@(and name (list :name name))
                     ,@(and url (list :uri url)))))

(defun elfeed-entries-from-json (url json)
  "Turn parsed JSON Feed content into a list of elfeed-entry structs.
URL identifies the feed and JSON is the parsed content."
  (let* ((feed-id url)
         (protocol (url-type (url-generic-parse-url url)))
         (namespace (elfeed-url-to-namespace url))
         (feed (elfeed-db-get-feed feed-id))
         (title (elfeed-cleanup (or (alist-get 'title json) "")))
         (authors (elfeed--json-authors-to-plist json))
         (autotags (elfeed-feed-autotags url)))
    (setf (elfeed-feed-url feed) url
          (elfeed-feed-title feed) title
          (elfeed-feed-author feed) authors)
    (cl-loop for item in (alist-get 'items json) collect
             (let* ((title (or (alist-get 'title item) ""))
                    (link (elfeed--fixup-protocol
                           protocol
                           (or (alist-get 'url item)
                               (alist-get 'external_url item))))
                    (date (or (alist-get 'date_published item)
                              (alist-get 'date_modified item)))
                    (authors (elfeed--json-authors-to-plist item))
                    (categories (alist-get 'tags item))
                    (content-html (alist-get 'content_html item))
                    (content (or content-html
                                 (alist-get 'content_text item)
                                 (alist-get 'summary item)))
                    (content-type (and content-html 'html))
                    (id (or (alist-get 'id item)
                            link
                            (elfeed-generate-id content)))
                    (full-id (cons namespace (elfeed-cleanup id)))
                    (original (elfeed-db-get-entry full-id))
                    (original-date (and original (elfeed-entry-date original)))
                    (tags (elfeed-normalize-tags autotags elfeed-initial-tags))
                    (enclosures
                     (cl-loop for attachment in (alist-get 'attachments item)
                              for url = (alist-get 'url attachment)
                              for type = (alist-get 'mime_type attachment)
                              for size = (alist-get 'size_in_bytes attachment)
                              when url collect
                              (list url type (and size (format "%s" size)))))
                    (db-entry
                     (elfeed-entry--create
                      :title (elfeed-cleanup title)
                      :id full-id
                      :feed-id feed-id
                      :link (elfeed-cleanup link)
                      :tags tags
                      :date (elfeed-new-date-for-entry original-date date)
                      :content content
                      :content-type content-type
                      :enclosures enclosures
                      :meta `(,@(when authors
                                  (list :authors authors))
                              ,@(when categories
                                  (list :categories categories))))))
               (run-hook-with-args 'elfeed-new-entry-parse-hook
                                   :json item db-entry)
               db-entry))))

(defsubst elfeed--atom-authors-to-plist (authors)
  "Parse list of AUTHORS as XML tags into list of plists."
  (let ((result ()))
    (dolist (author authors)
      (let ((plist ())
            (name (xml-query* (name *) author))
            (uri (xml-query* (uri *) author))
            (email (xml-query* (email *) author)))
        (when email
          (setf plist (list :email (elfeed-cleanup email))))
        (when uri
          (setf plist (nconc (list :uri (elfeed-cleanup uri)) plist)))
        (when name
          (setf plist (nconc (list :name (elfeed-cleanup name)) plist)))
        (push plist result)))
    (nreverse result)))

(defsubst elfeed--creators-to-plist (creators)
  "Convert Dublin Core list of CREATORS into an authors plist."
  (cl-loop for creator in creators
           collect (list :name creator)))

(defun elfeed--atom-cleanup-title (title type)
  "Cleanup TITLE string.
If TYPE is html, handle html entities."
  (elfeed-cleanup
   (if (and type (string-search "html" type))
       (xml-substitute-special title)
     title)))

(defun elfeed-entries-from-atom (url xml)
  "Turn parsed Atom content into a list of elfeed-entry structs.
URL identifies the feed and XML is the parsed content."
  (let* ((feed-id url)
         (protocol (url-type (url-generic-parse-url url)))
         (namespace (elfeed-url-to-namespace url))
         (feed (elfeed-db-get-feed feed-id))
         (title (elfeed--atom-cleanup-title
                 (xml-query* (feed title *) xml)
                 (xml-query* (feed title :type) xml)))
         (authors (xml-query-all* (feed author) xml))
         (xml-base (or (xml-query* (feed :base) xml) url))
         (autotags (elfeed-feed-autotags url)))
    (setf (elfeed-feed-url feed) url
          (elfeed-feed-title feed) title
          (elfeed-feed-author feed) (elfeed--atom-authors-to-plist authors))
    (cl-loop for entry in (xml-query-all* (feed entry) xml) collect
             (let* ((title (elfeed--atom-cleanup-title
                            (or (xml-query* (title *) entry) "")
                            (xml-query* (title :type) entry)))
                    (xml-base (elfeed-update-location
                               xml-base (xml-query* (:base) (list entry))))
                    (anylink (xml-query* (link :href) entry))
                    (altlink (xml-query* (link [rel "alternate"] :href) entry))
                    (link (elfeed--fixup-protocol
                           protocol
                           (elfeed-update-location xml-base
                                                   (or altlink anylink))))
                    (date (or (xml-query* (published *) entry)
                              (xml-query* (updated *) entry)
                              (xml-query* (date *) entry)
                              (xml-query* (modified *) entry) ; Atom 0.3
                              (xml-query* (issued *) entry))) ; Atom 0.3
                    (authors (nconc (elfeed--atom-authors-to-plist
                                     (xml-query-all* (author) entry))
                                    ;; Dublin Core
                                    (elfeed--creators-to-plist
                                     (xml-query-all* (creator *) entry))))
                    (categories (xml-query-all* (category :term) entry))
                    (content (elfeed--atom-content entry))
                    (id (or (xml-query* (id *) entry) link
                            (elfeed-generate-id content)))
                    (type (or (xml-query* (content :type) entry)
                              (xml-query* (summary :type) entry)
                              ""))
                    (tags (elfeed-normalize-tags autotags elfeed-initial-tags))
                    (content-type (if (string-match-p "html" type) 'html nil))
                    (etags (xml-query-all* (link [rel "enclosure"]) entry))
                    (enclosures
                     (cl-loop for enclosure in etags
                              for wrap = (list enclosure)
                              for href = (xml-query* (:href) wrap)
                              for type = (xml-query* (:type) wrap)
                              for length = (xml-query* (:length) wrap)
                              collect (list href type length)))
                    (db-entry (elfeed-entry--create
                               :title title
                               :feed-id feed-id
                               :id (cons namespace (elfeed-cleanup id))
                               :link (elfeed-cleanup link)
                               :tags tags
                               :date (or (elfeed-float-time date) (float-time))
                               :content content
                               :enclosures enclosures
                               :content-type content-type
                               :meta `(,@(unless (equal xml-base url)
                                           (list :base-url xml-base))
                                       ,@(when authors
                                           (list :authors authors))
                                       ,@(when categories
                                           (list :categories categories))))))
               (run-hook-with-args 'elfeed-new-entry-parse-hook
                                   :atom entry db-entry)
               db-entry))))

(defsubst elfeed--rss-author-to-plist (author)
  "Parse an RSS AUTHOR element into an authors plist."
  (when author
    (let ((clean (elfeed-cleanup author)))
      (if (string-match "\\`\\(.*\\) (\\([^)]+\\))\\'" clean)
          (list (list :name (match-string 2 clean)
                      :email (match-string 1 clean)))
        (list (list :email clean))))))

(defun elfeed-entries-from-rss (url xml)
  "Turn parsed RSS content into a list of elfeed-entry structs.
URL identifies the feed and XML is the parsed content."
  (let* ((feed-id url)
         (protocol (url-type (url-generic-parse-url url)))
         (namespace (elfeed-url-to-namespace url))
         (feed (elfeed-db-get-feed feed-id))
         (title (elfeed-cleanup (xml-query* (rss channel title *) xml)))
         (autotags (elfeed-feed-autotags url)))
    (setf (elfeed-feed-url feed) url
          (elfeed-feed-title feed) title)
    (cl-loop for item in (xml-query-all* (rss channel item) xml) collect
             (let* ((title (or (xml-query* (title *) item) ""))
                    (guid (xml-query* (guid *) item))
                    (link (or (xml-query* (link *) item) guid))
                    (link (elfeed--fixup-protocol
                           protocol
                           (elfeed-update-location
                            url (and link (string-trim link)))))
                    (date (or (xml-query* (pubDate *) item)
                              (xml-query* (date *) item)))
                    (authors (nconc (elfeed--rss-author-to-plist
                                     (xml-query* (author *) item))
                                    ;; Dublin Core
                                    (elfeed--creators-to-plist
                                     (xml-query-all* (creator *) item))))
                    (categories (xml-query-all* (category *) item))
                    (content (or (xml-query-all* (encoded *) item)
                                 (xml-query-all* (description *) item)))
                    (description (apply #'concat content))
                    (id (or guid link (elfeed-generate-id description)))
                    (full-id (cons namespace (elfeed-cleanup id)))
                    (original (elfeed-db-get-entry full-id))
                    (original-date (and original (elfeed-entry-date original)))
                    (tags (elfeed-normalize-tags autotags elfeed-initial-tags))
                    (etags (xml-query-all* (enclosure) item))
                    (enclosures
                     (cl-loop for enclosure in etags
                              for wrap = (list enclosure)
                              for url = (xml-query* (:url) wrap)
                              for type = (xml-query* (:type) wrap)
                              for length = (xml-query* (:length) wrap)
                              collect (list url type length)))
                    (db-entry (elfeed-entry--create
                               :title (elfeed-cleanup title)
                               :id full-id
                               :feed-id feed-id
                               :link (elfeed-cleanup link)
                               :tags tags
                               :date (elfeed-new-date-for-entry
                                      original-date date)
                               :enclosures enclosures
                               :content description
                               :content-type 'html
                               :meta `(,@(when authors
                                           (list :authors authors))
                                       ,@(when categories
                                           (list :categories categories))))))
               (run-hook-with-args 'elfeed-new-entry-parse-hook
                                   :rss item db-entry)
               db-entry))))

(defun elfeed-entries-from-rss1.0 (url xml)
  "Turn parsed RSS 1.0 content into a list of elfeed-entry structs.
URL identifies the feed and XML is the parsed content."
  (let* ((feed-id url)
         (namespace (elfeed-url-to-namespace url))
         (feed (elfeed-db-get-feed feed-id))
         (title (elfeed-cleanup (xml-query* (RDF channel title *) xml)))
         (autotags (elfeed-feed-autotags url)))
    (setf (elfeed-feed-url feed) url
          (elfeed-feed-title feed) title)
    (cl-loop for item in (xml-query-all* (RDF item) xml) collect
             (let* ((title (or (xml-query* (title *) item) ""))
                    (link (xml-query* (link *) item))
                    (date (or (xml-query* (pubDate *) item)
                              (xml-query* (date *) item)))
                    (description
                     (apply #'concat (xml-query-all* (description *) item)))
                    (id (or link (elfeed-generate-id description)))
                    (full-id (cons namespace (elfeed-cleanup id)))
                    (original (elfeed-db-get-entry full-id))
                    (original-date (and original (elfeed-entry-date original)))
                    (tags (elfeed-normalize-tags autotags elfeed-initial-tags))
                    (db-entry (elfeed-entry--create
                               :title (elfeed-cleanup title)
                               :id full-id
                               :feed-id feed-id
                               :link (elfeed-cleanup link)
                               :tags tags
                               :date (elfeed-new-date-for-entry
                                      original-date date)
                               :content description
                               :content-type 'html)))
               (run-hook-with-args 'elfeed-new-entry-parse-hook
                                   :rss1.0 item db-entry)
               db-entry))))

(defun elfeed--plist-skip (plist)
  "Skip over PLIST and return rest of the list."
  (while (keywordp (car plist))
    (setq plist (cddr plist)))
  plist)

(defun elfeed--feed-valid-p (feed)
  "Valid FEED entry in `elfeed-feeds'."
  (cl-typecase feed
    (list (let ((autotags (elfeed--plist-skip (cdr feed))))
            (and (stringp (car feed))
                 (all (lambda (x)
                        (and x (symbolp x) (not (keywordp x))))
                      autotags)
                 (evenp (- (length (cdr feed)) (length autotags))))))
    (string t)))

(defun elfeed-feed-list (&optional all)
  "Return a flat list version of `elfeed-feeds'.
Only a list of strings will be returned.  If ALL is nil, include only
feeds without the :no-update property."
  ;; Validate elfeed-feeds and fail early rather than asynchronously later.
  (cl-loop for feed in elfeed-feeds
           for url = (if (elfeed--feed-valid-p feed)
                         (cl-typecase feed (list (car feed)) (string feed))
                       (error "elfeed-feeds malformed, bad entry: %S" feed))
           if (or all (not (plist-get (cdr (assoc url elfeed-feeds)) :no-update)))
           collect url))

(defun elfeed-feed-autotags (url-or-feed)
  "Return tags to automatically apply to all entries from URL-OR-FEED."
  (let ((url (if (elfeed-feed-p url-or-feed)
                 (or (elfeed-feed-url url-or-feed)
                     (elfeed-feed-id url-or-feed))
               url-or-feed)))
    (elfeed--plist-skip (cdr (assoc url elfeed-feeds)))))

(defun elfeed-apply-hooks-now ()
  "Apply `elfeed-new-entry-hook' to all entries in the database."
  (declare (completion elfeed--mode-p))
  (interactive)
  (elfeed-db-visit (entry)
    (run-hook-with-args 'elfeed-new-entry-hook entry))
  (message "Hooks applied"))

(defun elfeed-apply-autotags-now ()
  "Apply autotags to existing entries according to `elfeed-feeds'."
  (declare (completion elfeed--mode-p))
  (interactive)
  (elfeed-db-visit (entry feed)
    (apply #'elfeed-tag entry (elfeed-feed-autotags feed)))
  (message "Auto tags applied"))

(defun elfeed-handle-http-error (url status)
  "Handle an http error during retrieval of URL with STATUS code."
  (incf (elfeed-meta (elfeed-db-get-feed url) :failures 0))
  (run-hook-with-args 'elfeed-http-error-hook url status)
  (elfeed-log 'error "%s: %S" url status))

(defun elfeed-handle-parse-error (url error)
  "Handle parse error during parsing of URL with ERROR message."
  (incf (elfeed-meta (elfeed-db-get-feed url) :failures 0))
  (run-hook-with-args 'elfeed-parse-error-hook url error)
  (elfeed-log 'error "%s: %s" url error))

(defun elfeed--prompt-feed ()
  "Prompt for feed URL via `completing-read'."
  (let ((url (completing-read
              "Feed: "
              (completion-table-with-metadata
               (elfeed-feed-list :all)
               `((category . url)
                 (annotation-function
                  . ,(lambda (url)
                       (when-let* ((feed (elfeed-db-get-feed url))
                                   (title (elfeed-meta--title feed)))
                         (format " (%s)" title)))))))))
    (when (equal url "")
      (user-error "No feed selected"))
    url))

(defun elfeed-update-feed (url)
  "Update a specific feed identified by URL.
Run `elfeed-update-init-hook' before."
  (declare (completion elfeed--mode-p))
  (interactive (list (elfeed--prompt-feed)))
  (run-hooks 'elfeed-update-init-hook)
  (elfeed--update-feed url))

(defun elfeed--update-feed-parse (url)
  "Parse buffer for entries from URL and add them to the database."
  (condition-case error
      (let* ((json (or (string-suffix-p ".json" url) (looking-at-p "{")))
             (data (if json
                       (json-parse-buffer :object-type 'alist :array-type 'list
                                          :null-object nil :false-object nil)
                     (elfeed-xml-parse-region (point) (point-max)))))
        (elfeed-db-add
         (cl-case (if json :json (elfeed-feed-type data))
           (:json (elfeed-entries-from-json url data))
           (:atom (elfeed-entries-from-atom url data))
           (:rss (elfeed-entries-from-rss url data))
           (:rss1.0 (elfeed-entries-from-rss1.0 url data))
           (otherwise (error "Unknown feed type")))))
    (error
     (elfeed-handle-parse-error url error))))

(defun elfeed--update-feed (url &optional inhibit-update-hook)
  "Update a specific feed identified by URL.
If INHIBIT-UPDATE-HOOK is non-nil do not run the `elfeed-update-hook'."
  (run-hook-with-args-until-success
   'elfeed-fetch-functions url
   (lambda (result)
     (pcase result
       ((or :error :success) nil)
       (:parse (elfeed--update-feed-parse url))
       (_ (error "Invalid fetch result")))
     (unless inhibit-update-hook
       (run-hook-with-args 'elfeed-update-hook url)))))

(defun elfeed--fetch-url-curl (url cb)
  "See `elfeed-fetch-url' for documentation of URL and CB."
  (let* ((cb (lambda (success)
               (cond
                ((not success)
                 (let ((print-escape-newlines t))
                   (elfeed-handle-http-error url elfeed-curl-error-message)
                   (funcall cb :error)))
                ((eq elfeed-curl-status-code 304) ;; Not modified
                 (funcall cb :success))
                ((let ((feed (elfeed-db-get-feed url)))
                   ;; Update Last-Modified and Etag
                   (setf (elfeed-meta feed :last-modified)
                         (cdr (assoc "last-modified" elfeed-curl-headers))
                         (elfeed-meta feed :etag)
                         (cdr (assoc "etag" elfeed-curl-headers)))
                   (if (equal url elfeed-curl-location)
                       (setf (elfeed-meta feed :canonical-url) nil)
                     (setf (elfeed-meta feed :canonical-url) elfeed-curl-location))
                   (funcall cb :parse))))))
         (feed (elfeed-db-get-feed url))
         (last-modified (elfeed-meta feed :last-modified))
         (etag (elfeed-meta feed :etag))
         (headers `(("User-Agent" . ,elfeed-user-agent))))
    (when etag
      (push `("If-None-Match" . ,etag) headers))
    (when last-modified
      (push `("If-Modified-Since" . ,last-modified) headers))
    (elfeed-curl-enqueue url cb :headers headers)))

(defun elfeed--fetch-url-queue (url cb)
  "See `elfeed-fetch-url' for documentation of URL and CB."
  (let ((cb (lambda (status)
              (if (eq (car status) :error)
                  (let ((print-escape-newlines t))
                    (elfeed-handle-http-error url status)
                    (funcall cb :error))
                (goto-char (point-min))
                (elfeed-move-to-first-empty-line)
                (set-buffer-multibyte t)
                (funcall cb :parse))
              (kill-buffer))))
    (url-queue-retrieve url cb nil t t)))

(defun elfeed-fetch-url (url cb)
  "Fetch feed from URL and call CB either with the keyword :error or :parse.
In order to modify feed content, you can use a custom fetch function.

    (defun custom-fetcher (url cb)
      (elfeed-fetch-url url
        (lambda (result)
          (when (eq result :parse)
            ;; ...manipulate buffer...
            (funcall cb :parse)))))"
  (if elfeed-use-curl
      (elfeed--fetch-url-curl url cb)
    (elfeed--fetch-url-queue url cb)))

(defun elfeed-candidate-feeds ()
  "Return a list of possible feeds from `elfeed-feed-functions'."
  (let (res)
    (run-hook-wrapped
     'elfeed-feed-functions
     (lambda (fun)
       (let* ((val (elfeed-cleanup (funcall fun))))
         (when (and (not (zerop (length val)))
                    (elfeed-looks-like-url-p val))
           (cl-pushnew val res :test #'equal)))
       nil))
    (nreverse res)))

(cl-defun elfeed-add-feed (url &key save)
  "Manually add a feed at URL to the database.
If SAVE is non-nil the new value of ‘elfeed-feeds’ is saved.  When
called interactively, SAVE is set to t."
  (declare (completion elfeed--mode-p))
  (interactive
   (list
    (let* ((feeds (elfeed-candidate-feeds))
           (prompt (if feeds (concat "URL (default " (car feeds)  "): ")
                     "URL: "))
           (input (read-from-minibuffer prompt nil nil nil nil feeds))
           (result (elfeed-cleanup input)))
      (cond ((not (zerop (length result))) result)
            (feeds (car feeds))
            ((user-error "No feed to add"))))
    :save t))
  (cl-pushnew url elfeed-feeds :test #'equal)
  (when save
    (customize-save-variable 'elfeed-feeds elfeed-feeds))
  (elfeed-update-feed url))

(defun elfeed-delete-feed (url)
  "Delete feed identified by URL from the database."
  (declare (completion elfeed--mode-p))
  (interactive (list (elfeed--prompt-feed)))
  (let (entries)
    (elfeed-db-visit (entry feed)
      (when (equal (elfeed-feed-id feed) url)
        (push entry entries)))
    (when (y-or-n-p (format "Really delete %d entries of feed %s? "
                            (length entries) url))
      (run-hooks 'elfeed-update-init-hook)
      (elfeed-db-delete entries)
      (elfeed-db--gc-empty-feeds)
      (setq elfeed-feeds (assoc-delete-all
                          url (delete url (copy-sequence elfeed-feeds))))
      (message "Deleted feed %s. Adjust `elfeed-feeds' in your configuration!" url)
      (run-hook-with-args 'elfeed-update-hook url))))

;;;###autoload
(defun elfeed-update ()
  "Update all the feeds in `elfeed-feeds'."
  (declare (completion elfeed--mode-p))
  (interactive)
  (if (> (elfeed-queue-count-total) 0)
      (user-error "Update already running")
    (setq elfeed-log-error-count 0)
    (elfeed-log 'info "Update all feeds: %s"
                (format-time-string "%B %e %Y %H:%M:%S %Z"))
    (run-hooks 'elfeed-update-init-hook)
    (mapc #'elfeed--update-feed (elfeed-shuffle (elfeed-feed-list)))))

;;;###autoload
(defun elfeed-update-background ()
  "Update all the feeds in `elfeed-feeds' without running update hooks.
This function can be called from a timer in the background, since it
does not disturb any visible Elfeed windows.  No new update is started
if another update is already running."
  (when (= (elfeed-queue-count-total) 0)
    (elfeed-log 'info "Update all feeds in background: %s"
                (format-time-string "%B %e %Y %H:%M:%S %Z"))
    (dolist (feed (elfeed-shuffle (elfeed-feed-list)))
      (elfeed--update-feed feed t))))

;;;###autoload
(defun elfeed ()
  "Enter elfeed via `elfeed-entry-point'."
  (interactive)
  (call-interactively elfeed-entry-point))

;; New entry filtering

(cl-defun elfeed-make-tagger
    (&key feed-title feed-url entry-title entry-link categories
          after before add remove callback)
  "Create a function that adds or removes tags on matching entries.

FEED-TITLE, FEED-URL, ENTRY-TITLE, ENTRY-LINK and CATEGORIES are regular
expressions or an expression (not <regex>), which indicates a negative
match.  AFTER and BEFORE are relative times (see `elfeed-time-duration').
Entries must match all provided expressions.  If an entry matches, add
tags ADD and remove tags REMOVE.  Call CALLBACK for each entry.

Examples,

  (elfeed-make-tagger :feed-url \"youtube\\\\.com\"
                      :add \\='(video youtube))

  (elfeed-make-tagger :before \"1 week ago\"
                      :remove \\='unread)

  (elfeed-make-tagger :feed-url \"example\\\\.com\"
                      :entry-title \\='(not \"something interesting\")
                      :add \\='junk)

The returned function should be added to `elfeed-new-entry-hook'."
  (let ((after-time  (and after  (elfeed-time-duration after)))
        (before-time (and before (elfeed-time-duration before))))
    (when (and add (symbolp add)) (setf add (list add)))
    (when (and remove (symbolp remove)) (setf remove (list remove)))
    (lambda (entry)
      (let ((feed (elfeed-entry-feed entry))
            (date (elfeed-entry-date entry))
            (case-fold-search t))
        (cl-labels ((match (r s)
                    (pcase r
                      (`nil t)
                      (`(not ,r) (not (match r s)))
                      (_ (if (stringp s)
                             (string-match-p r s)
                           (any (lambda (s) (string-match-p r s)) s))))))
          (when (and
                 (match feed-title  (elfeed-feed-title  feed))
                 (match feed-url    (elfeed-feed-url    feed))
                 (match entry-title (elfeed-entry-title entry))
                 (match entry-link  (elfeed-entry-link  entry))
                 (match categories  (elfeed-meta entry :categories))
                 (or (not after-time)  (> date (- (float-time) after-time)))
                 (or (not before-time) (< date (- (float-time) before-time))))
            (when add
              (apply #'elfeed-tag entry add))
            (when remove
              (apply #'elfeed-untag entry remove))
            (when callback
              (funcall callback entry))
            entry))))))

;; OPML

(defun elfeed--parse-opml (xml)
  "Parse XML (from `xml-parse-region') into `elfeed-feeds' list."
  (cl-loop for (_tag attr . content) in (cl-remove-if-not #'listp xml)
           when (assoc 'xmlUrl attr) collect (cdr it)
           else append (elfeed--parse-opml content)))

(defun elfeed-load-opml (file)
  "Load feeds from FILE in OPML format into `elfeed-feeds'.
When called interactively, the changes to `elfeed-feeds' are
saved to your customization file."
  (declare (completion elfeed--mode-p))
  (interactive "fOPML file: ")
  (let* ((xml (elfeed-xml-parse-file file))
         (feeds (elfeed--parse-opml xml))
         (full (append feeds elfeed-feeds)))
    (prog1 (setf elfeed-feeds (delete-dups (copy-sequence full)))
      (when (called-interactively-p 'any)
        (customize-save-variable 'elfeed-feeds elfeed-feeds)
        (elfeed-log 'notice "%d feeds loaded from %s" (length feeds) file)))))

(defun elfeed-export-opml (file)
  "Export the current feed listing to OPML-formatted FILE."
  (declare (completion elfeed--mode-p))
  (interactive "FOutput OPML file: ")
  (with-temp-file file
    (let ((standard-output (current-buffer)))
      (princ "<?xml version=\"1.0\"?>\n")
      (xml-print
       `((opml ((version . "1.0"))
               (head () (title () "Elfeed Export"))
               (body ()
                     ,@(cl-loop for url in (elfeed-feed-list)
                                for feed = (elfeed-db-get-feed url)
                                for title = (or (elfeed-feed-title feed) "")
                                collect `(outline ((xmlUrl . ,url)
                                                   (title . ,title)))))))))))

(defun elfeed--header-button-click ()
  "Handle click on the header line of the search buffer."
  (declare (completion ignore))
  (interactive "@")
  (when-let* (((mouse-event-p last-input-event))
              (pos (event-end last-input-event))
              (str (posn-string pos))
              (button (get-text-property
                       (cdr str) 'elfeed--header-button (car str))))
    (call-interactively button)))

(defvar-keymap elfeed--header-button-map
  :doc "Keymap attached as a text property to header line buttons."
  "<header-line> <down-mouse-1>" #'ignore
  "<header-line> <mouse-1>" #'elfeed--header-button-click)

(defun elfeed--header-button (command &optional text help)
  "Create header line button for COMMAND with optional TEXT and HELP."
  (propertize (or text (symbol-name command))
              'elfeed--header-button command
              'keymap elfeed--header-button-map
              'help-echo (or help (format "Run `%s'" command))
              'mouse-face 'highlight))

(defun elfeed--header-log-button ()
  "Button to show the Elfeed log."
  (when (> elfeed-log-error-count 0)
    (concat (elfeed--header-button
             #'elfeed-log-show
             (format (propertize "(%d)" 'face 'error)
                     elfeed-log-error-count))
            " ")))

(defun elfeed--header-jobs ()
  "Header showing active jobs."
  (cond
   ((not elfeed-db) "Database not loaded.")
   ((zerop (elfeed-db-last-update))
    (concat "Database empty. Use "
            (elfeed--header-button #'elfeed-add-feed) ", or "
            (elfeed--header-button #'elfeed-load-opml) ", or "
            (elfeed--header-button #'elfeed-update) "."))
   ((let ((total (elfeed-queue-count-total)))
      (when (> total 0)
        (let ((active (elfeed-queue-count-active)))
          (concat
           (elfeed--header-log-button)
           (format "%d jobs pending, %d active…"
                   (- total active) active))))))))

(defun elfeed--header-update (buffer-update)
  "Header showing the last database update time.
Furthermore show an extra button if the current buffer is outdated
relative to the database according to BUFFER-UPDATE."
  (let* ((db-update (elfeed-db-last-update))
         (delta (- (float-time) db-update))
         (updated (if (> delta elfeed-last-update-relative)
                      (concat "Updated "
                              (elfeed-add-properties
                               (format-time-string "%Y-%m-%d %H:%M"
                                                   (seconds-to-time db-update))
                               'face 'elfeed-search-last-update-face))
                    (format "Updated %s ago"
                            (elfeed-add-properties
                             (if (< delta 60)
                                 "< 1 minute"
                               (compat-call seconds-to-string delta t))
                             'face 'elfeed-search-last-update-face)))))
    (concat
     (elfeed--header-log-button)
     (elfeed--header-button #'elfeed-update updated)
     (when (> db-update buffer-update)
       (concat " " (elfeed--header-button #'revert-buffer
                                          (propertize "(*)" 'face 'error)
                                          "Buffer is outdated. Run `revert-buffer'."))))))

(defun elfeed--header-line-format (fun)
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

(defun elfeed-is-status-error (status use-curl)
  "Check if STATUS is an error, USE-CURL specifies the backend (obsolete)."
  (declare (obsolete nil "4.0.0"))
  (if use-curl
      (not status)
    (eq (car status) :error)))

(defmacro elfeed-with-fetch (url &rest body)
  "Fetch feed URL and call BODY (obsolete).
Use `elfeed-fetch-url' instead which updates the Etag and modification
date, in addition to checking the tag and date.  The new
`elfeed-fetch-url' completely disentangles the `elfeed-curl-enqueue' and
`url-queue-queue' code."
  (declare (obsolete #'elfeed-fetch-url "4.0.0"))
  `(let ((cb (let ((use-curl elfeed-use-curl))
               (ignore use-curl)
               (lambda (status) ,@body))))
     (if elfeed-use-curl
         (let* ((feed (elfeed-db-get-feed url))
                (last-modified (elfeed-meta feed :last-modified))
                (etag (elfeed-meta feed :etag))
                (headers `(("User-Agent" . ,elfeed-user-agent))))
           (when etag
             (push `("If-None-Match" . ,etag) headers))
           (when last-modified
             (push `("If-Modified-Since" . ,last-modified) headers))
           (elfeed-curl-enqueue ,url cb :headers headers))
       (url-queue-retrieve ,url cb () t t))))

(provide 'elfeed)

(unless nil
  ;; run-time only, so don't load when compiling other files
  (require 'elfeed-show)
  (require 'elfeed-search)
  (require 'elfeed-tree))

;;; elfeed.el ends here
