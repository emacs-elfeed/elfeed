;;; elfeed-db.el --- database and model for elfeed -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>

;;; Commentary:

;; Elfeed is aware of two type of things: feeds and entries.  All dates
;; are stored as floating point epoch seconds.

;; Feeds are keyed by their user-provided feed URL, which acts as the
;; feed identity regardless of any other stated identity.  Feeds have a
;; list of entries.

;; Entries are keyed in order of preference by id (Atom), guid (RSS),
;; or link.  To avoid circular references, entries refer to their
;; parent feeds by URL.

;; Feed content is stored in a content-addressable loose-file
;; database, very similar to an unpacked Git object database.  Entries
;; have references to items in this database (elfeed-ref), keeping the
;; actual entry struct memory footprint small.  Most importantly, this
;; keeps the core index small so that it can quickly be written as a
;; whole to the filesystem.  The wire format is just the s-expression
;; print form of the top-level hash table.

;; The database can be compacted into a small number of compressed
;; files with the interactive function `elfeed-db-compact'.  This could
;; be used as a kill-emacs hook.

;; An AVL tree containing all database entries ordered by date is
;; maintained as part of the database.  We almost always want to look
;; at entries ordered by date and this step accomplished that very
;; efficiently with the AVL tree.  This is the reasoning behind the
;; `elfeed-db-visit' interface.

;; Unfortunately there's a nasty bug (bug#15190) in the reader that
;; makes hash tables and `print-circle' incompatible.  It's been fixed
;; in trunk, but many users will likely be stuck with this bug for the
;; next few years.  This means the database format can't exploit
;; circular references.

;; Entry and feed objects can have arbitrary metadata attached,
;; automatically stored in the database.  The setf-able `elfeed-meta'
;; function is used to access these.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'avl-tree)
(require 'cl-lib)

(require 'elfeed-lib)

(defcustom elfeed-db-directory
  (locate-user-emacs-file "elfeed" "~/.elfeed")
  "Directory where elfeed will store its database.
You may want to customize this setting in your user configuration.
Elfeed uses a subdirectory of your Emacs configuration by default, e.g.,
~/.config/emacs/elfeed, in order to keep the user home clean.  But if an
old ~/.elfeed directory is present, it will be used instead."
  :group 'elfeed
  :type 'directory)

(defvar elfeed-new-entry-hook ()
  "Functions in this list are called with the new entry as its argument.
This is a chance to add custom tags to new entries.")

(defvar elfeed-db-update-hook ()
  "Functions which are called without arguments when :last-update time is updated.")

(defvar elfeed-db-unload-hook ()
  "Hook to run immediately after `elfeed-db-unload'.")

;; Data model:

(cl-defstruct (elfeed-feed (:constructor elfeed-feed--create)
                           (:copier nil))
  "A web feed, contains elfeed-entry structs."
  id url title author meta)

(cl-defstruct (elfeed-entry (:constructor elfeed-entry--create)
                            (:copier nil))
  "A single entry from a feed, normalized towards Atom."
  id title link date content content-type enclosures tags feed-id meta)

(cl-defstruct (elfeed-db-vtbl (:constructor elfeed-db-vtbl-create))
  "A vtable for database methods.
All of these methods when through the elfeed-db-* functions will call
`elfeed-db-ensure' unless specified otherwise."
  (get-feed nil :documentation "Get/create the feed for ID.")
  (get-entry nil :documentation "Get the entry for ID.")
  (set-update-time-1 nil :documentation "Update the database last-update time.")
  (add nil :documentation "Add ENTRIES to the database.")
  (delete nil :documentation "Remove ENTRIES from database.")
  (tag nil :documentation "Modify the database to add TAGS to entries in ENTRY-OR-ENTRY-LIST.")
  (untag nil :documentation "Modify the database to remove TAGS to entries in ENTRY-OR-ENTRY-LIST.")
  (last-update nil :documentation "Return the last database update time in (`float-time') seconds.")
  (for-each nil :documentation "Run FUNCTION over every entry id from newest to oldest.")
  (feed-entries nil :documentation "Return a list of all entries for a particular feed.
The FEED-OR-ID may be a feed struct or a feed ID (url).")
  (get-all-tags nil :documentation "Return a list of all tags currently in the database.")
  (save nil :documentation "Write the database index to the filesystem.")
  (load nil :documentation "Load the database index from the filesystem.
This function does not call `elfeed-db-ensure'.")
  (loaded-p nil :documentation "Predicate for whether the database has been loaded.
This function does not call `elfeed-db-ensure'.")
  (unload-1 nil :documentation "Unload the database so that it can be operated on externally.
This function does not call `elfeed-db-ensure.'")
  (size nil :documentation "Return a count of the number of entries in the database.")
  (gc-empty-feeds nil :documentation "Remove feeds with no entries from the database."))

(defconst elfeed-db-vtbl-classic (elfeed-db-vtbl-create
                                  :get-feed #'elfeed-db-classic-get-feed
                                  :get-entry #'elfeed-db-classic-get-entry
                                  :set-update-time-1 #'elfeed-db-classic-set-update-time-1
                                  :add #'elfeed-db-classic-add
                                  :delete #'elfeed-db-classic-delete
                                  :tag #'elfeed-db-classic-tag
                                  :untag #'elfeed-db-classic-untag
                                  :last-update #'elfeed-db-classic-last-update
                                  :for-each #'elfeed-db-classic-for-each
                                  :save #'elfeed-db-classic-save
                                  :load #'elfeed-db-classic-load
                                  :loaded-p #'elfeed-db-classic-loaded-p
                                  :unload-1 #'elfeed-db-classic-unload-1
                                  :size #'elfeed-db-classic-size
                                  :gc-empty-feeds #'elfeed-db-classic-gc-empty-feeds)
  "Database vtable using the classic implementation.")

(defconst elfeed-db-vtbl-sqlite (elfeed-db-vtbl-create
                                 :get-feed #'elfeed-db-sqlite-get-feed
                                 :get-entry #'elfeed-db-sqlite-get-entry
                                 :set-update-time-1 #'elfeed-db-sqlite-set-update-time-1
                                 :add #'elfeed-db-sqlite-add
                                 :delete #'elfeed-db-sqlite-delete
                                 :tag #'elfeed-db-sqlite-tag
                                 :untag #'elfeed-db-sqlite-untag
                                 :last-update #'elfeed-db-sqlite-last-update
                                 :for-each #'elfeed-db-sqlite-for-each
                                 :feed-entries #'elfeed-db-sqlite-feed-entries
                                 :get-all-tags #'elfeed-db-sqlite-get-all-tags
                                 :save #'elfeed-db-sqlite-save
                                 :load #'elfeed-db-sqlite-load
                                 :loaded-p #'elfeed-db-sqlite-loaded-p
                                 :unload-1 #'elfeed-db-sqlite-unload-1
                                 :size #'elfeed-db-sqlite-size
                                 :gc-empty-feeds #'elfeed-db-sqlite-gc-empty-feeds)
  "Database vtable using the sqlite implementation.")

(defvar elfeed-db-vtbl elfeed-db-vtbl-classic
  "The vtable for database operations.")

(defun elfeed-entry-merge (a b)
  "Merge B into A, preserving A's tags.
Return non-nil if an actual update occurred, not counting content."
  (setf (elfeed-entry-tags b) (elfeed-entry-tags a)
        (elfeed-entry-content a) (elfeed-entry-content b))
  (cl-loop for (key value) on (elfeed-entry-meta b) by #'cddr
           do (setf (elfeed-entry-meta a)
                    (plist-put (elfeed-entry-meta a) key value)))
  (not
   (zerop
    (cl-loop for i from 1 below (1- (length a))
             for part-a = (aref a i)
             for part-b = (aref b i)
             count (not (equal part-a part-b))
             do (setf (aref a i) part-b)))))

(defun elfeed-db-get-feed (id)
  "Get/create the feed for ID."
  (elfeed-db-ensure)
  (funcall (elfeed-db-vtbl-get-feed elfeed-db-vtbl) id))

(defun elfeed-db-get-entry (id)
  "Get the entry for ID."
  (elfeed-db-ensure)
  (funcall (elfeed-db-vtbl-get-entry elfeed-db-vtbl) id))

(defun elfeed-db-compare (a b)
  "Return non-nil if entry A is newer than entry B."
  (let* ((entry-a (elfeed-db-get-entry a))
         (entry-b (elfeed-db-get-entry b))
         (date-a (elfeed-entry-date entry-a))
         (date-b (elfeed-entry-date entry-b)))
    (if (= date-a date-b)
        (string< (prin1-to-string b) (prin1-to-string a))
      (> date-a date-b))))

(defun elfeed-db-set-update-time ()
  "Update the database last-update time."
  (elfeed-db-ensure)
  (funcall (elfeed-db-vtbl-set-update-time-1 elfeed-db-vtbl))
  (run-hooks 'elfeed-db-update-hook))

(defun elfeed-db-add (entries)
  "Add ENTRIES to the database."
  (elfeed-db-ensure)
  (funcall (elfeed-db-vtbl-add elfeed-db-vtbl) entries))

(defun elfeed-db-delete (entries)
  "Delete ENTRIES from database."
  (elfeed-db-ensure)
  (funcall (elfeed-db-vtbl-delete elfeed-db-vtbl) entries))

(defun elfeed-entry-feed (entry)
  "Get the feed struct for ENTRY."
  (elfeed-db-get-feed (elfeed-entry-feed-id entry)))

(defun elfeed-normalize-tags (tags &rest more-tags)
  "Return the normalized tag list for TAGS.
Additional tag lists can be given as MORE-TAGS."
  (delete-consecutive-dups
   (sort (apply #'append tags (nconc more-tags (list ()))) #'string<)))

(defun elfeed-tag-1 (entry &rest tags)
  "Add TAGS to ENTRY."
  (let ((current (elfeed-entry-tags entry)))
    (setf (elfeed-entry-tags entry)
          (elfeed-normalize-tags (append tags current)))))

(defun elfeed-untag-1 (entry &rest tags)
  "Remove TAGS from ENTRY."
  (setf (elfeed-entry-tags entry)
        (cl-loop for tag in (elfeed-entry-tags entry)
                 unless (memq tag tags) collect tag)))

(defun elfeed-tag (entry-or-entry-list &rest tags)
  "Add TAGS to ENTRY-OR-ENTRY-LIST and run `elfeed-tag-hooks'."
  (elfeed-db-ensure)
  (let ((entries (ensure-list entry-or-entry-list)))
    (funcall (elfeed-db-vtbl-tag elfeed-db-vtbl) entries tags)
    (run-hook-with-args 'elfeed-tag-hooks entries tags)
    (cl-loop for entry in entries do (apply #'elfeed-tag-1 entry tags))))

(defun elfeed-untag (entry-or-entry-list &rest tags)
  "Remove TAGS from ENTRY-OR-ENTRY-LIST and run `elfeed-untag-hooks'."
  (elfeed-db-ensure)
  (let ((entries (ensure-list entry-or-entry-list)))
    (funcall (elfeed-db-vtbl-untag elfeed-db-vtbl) entries tags)
    (run-hook-with-args 'elfeed-untag-hooks entries tags)
    (cl-loop for entry in entries do (apply #'elfeed-untag-1 entry tags))))

(defun elfeed-tagged-p (tag entry)
  "Return non-nil if ENTRY is tagged by TAG."
  (memq tag (elfeed-entry-tags entry)))

(defun elfeed-db-last-update ()
  "Return the last database update time in (`float-time') seconds."
  (elfeed-db-ensure)
  (funcall (elfeed-db-vtbl-last-update elfeed-db-vtbl)))

(defun elfeed-db-for-each (function)
  "Call FUNCTION with every entry id from newest to oldest."
  (elfeed-db-ensure)
  (funcall (elfeed-db-vtbl-for-each elfeed-db-vtbl) function))

(defmacro elfeed-db-visit (binds &rest body)
  "Visit each entry in the database from newest to oldest.
Use `elfeed-db-return' to exit early and optionally return data.
BINDS are the bindings for entry and optionally feed around BODY.

  (elfeed-db-visit (entry feed)
    (do-something entry)
    (when (some-date-criteria-p entry)
      (elfeed-db-return)))

  (elfeed-db-visit (entry)
    ...)"
  (declare (indent defun) (debug ((symbolp &optional symbolp) body)))
  `(catch 'elfeed-db-done
     (prog1 nil
       (elfeed-db-ensure)
       (elfeed-db-for-each (lambda (id)
                             (let* ((,(car binds) (elfeed-db-get-entry id))
                                    ,@(and (cdr binds)
                                           (string-match-p "\\`[^_]" (symbol-name (cadr binds)))
                                           `((,@(cdr binds) (elfeed-entry-feed ,(car binds))))))
                               ,@body))))))

;; I prefer if everything lives inside the elfeed-* namespace. Nevertheless keep
;; the old name for backward compatibility, since it is widely used in packages
;; depending on Elfeed.
(defalias 'with-elfeed-db-visit #'elfeed-db-visit)

(defun elfeed-feed-entries (feed-or-id)
  "Return a list of all entries for a particular feed.
The FEED-OR-ID may be a feed struct or a feed ID (url)."
  (if-let* ((feed-entries (elfeed-db-vtbl-feed-entries elfeed-db-vtbl)))
      (funcall feed-entries feed-or-id)
    (let ((feed-id (if (elfeed-feed-p feed-or-id)
                       (elfeed-feed-id feed-or-id)
                     feed-or-id)))
      (let ((entries))
        (elfeed-db-visit (entry feed)
                         (when (equal (elfeed-feed-id feed) feed-id)
                           (push entry entries)))
        (nreverse entries)))))

(defun elfeed-apply-hooks-now ()
  "Apply `elfeed-new-entry-hook' to all entries in the database."
  (interactive)
  (elfeed-db-visit (entry)
    (run-hook-with-args 'elfeed-new-entry-hook entry)))

(defmacro elfeed-db-return (&optional value)
  "Use this to exit early and return VALUE from `elfeed-db-visit'."
  (declare (debug (&optional sexp)))
  `(throw 'elfeed-db-done ,value))

(defun elfeed-db-get-all-tags ()
  "Return a list of all tags currently in the database."
  (elfeed-db-ensure)
  (if-let* ((get-all-tags (elfeed-db-vtbl-get-all-tags elfeed-db-vtbl)))
      (funcall get-all-tags)
    (let ((table (make-hash-table :test 'eq)))
      (elfeed-db-visit (e)
                       (dolist (tag (elfeed-entry-tags e))
                         (setf (gethash tag table) tag)))
      (let ((tags ()))
        (maphash (lambda (k _) (push k tags)) table)
        (sort tags #'string<)))))

;; Saving and Loading:

(defun elfeed-db-save ()
  "Write the database index to the filesystem."
  (elfeed-db-ensure)
  (funcall (elfeed-db-vtbl-save elfeed-db-vtbl)))

(defun elfeed-db-save-safe ()
  "Run `elfeed-db-save' without triggering any errors, for use as a safe hook."
  (ignore-errors (elfeed-db-save)))

(defun elfeed-db-load ()
  "Load the database index from the filesystem."
  (funcall (elfeed-db-vtbl-load elfeed-db-vtbl)))

(defun elfeed-db-loaded-p ()
  "Load the database index from the filesystem."
  (funcall (elfeed-db-vtbl-loaded-p elfeed-db-vtbl)))

(defun elfeed-db-unload ()
  "Unload the database so that it can be operated on externally.

Runs `elfeed-db-unload-hook' after unloading the database."
  (interactive)
  (elfeed-db-save)
  (funcall (elfeed-db-vtbl-unload-1 elfeed-db-vtbl))
  (run-hooks 'elfeed-db-unload-hook))

(defun elfeed-db-ensure ()
  "Ensure that the database has been loaded."
  (unless (elfeed-db-loaded-p) (elfeed-db-load)))

(defun elfeed-db-size ()
  "Return a count of the number of entries in the database."
  (elfeed-db-ensure)
  (funcall (elfeed-db-vtbl-size elfeed-db-vtbl)))

;; Metadata:

(defun elfeed-meta--plist (thing)
  "Get the metadata plist for THING."
  (cl-typecase thing
    (elfeed-feed  (elfeed-feed-meta  thing))
    (elfeed-entry (elfeed-entry-meta thing))
    (otherwise (error "Don't know how to access metadata on %S" thing))))

(defun elfeed-meta--set-plist (thing plist)
  "Set the metadata plist on THING to PLIST."
  (cl-typecase thing
    (elfeed-feed  (setf (elfeed-feed-meta thing) plist))
    (elfeed-entry (setf (elfeed-entry-meta thing) plist))
    (otherwise (error "Don't know how to access metadata on %S" thing))))

(defun elfeed-db--plist-fixup (plist)
  "Remove nil values from PLIST."
  (cl-loop for (k v) on plist by #'cddr
           when v collect k and collect v))

(defun elfeed-meta (thing key &optional default)
  "Access metadata for THING (entry, feed) under KEY.
Return DEFAULT if unavailable.  During `elfeed-db-gc' and
`elfeed-db-pack', metadata values will be scanned for `elfeed-ref'
objects, such that references in metadata will be kept alive.  Note that
only list data structures will be scanned (e.g., cons, list, alist,
plist).  The data structures must not be cyclic since this is not
supported by the database format."
  (or (plist-get (elfeed-meta--plist thing) key)
      default))

(defun elfeed-meta--put (thing key value)
  "Set metadata to VALUE on THING under KEY."
  (unless (elfeed-readable-p value) (error "New value must be readable"))
  (let ((new-plist (plist-put (elfeed-meta--plist thing) key value)))
    (prog1 value
      (elfeed-meta--set-plist thing (elfeed-db--plist-fixup new-plist)))))

(gv-define-setter elfeed-meta (value thing key &optional _default)
  `(elfeed-meta--put ,thing ,key ,value))

(defun elfeed-meta--title (thing)
  "Return TITLE of THING."
  (cl-typecase thing
    (elfeed-feed  (or (plist-get (elfeed-feed-meta thing) :title)
                      (elfeed-feed-title thing)))
    (elfeed-entry (or (plist-get (elfeed-entry-meta thing) :title)
                      (elfeed-entry-title thing)))
    (otherwise (error "Don't know how to access title on %S" thing))))

;; Filesystem storage:

(defvar elfeed-ref-archive nil
  "Index of archived/packed content.")

(defvar elfeed-ref-cache nil
  "Temporary storage of the full archive content.")

(cl-defstruct (elfeed-ref (:constructor elfeed-ref--create)
                          (:copier nil))
  id)

(defun elfeed-ref--file (ref)
  "Determine the storage filename for REF."
  (let ((id (elfeed-ref-id ref)))
    (file-name-concat (expand-file-name "data" elfeed-db-directory)
                      (substring id 0 2) id)))

(cl-defun elfeed-ref-archive-filename (&optional (suffix ""))
  "Return the base filename of the archive files with optional SUFFIX."
  (concat (expand-file-name "data/archive" elfeed-db-directory) suffix))

(defun elfeed-ref-archive-load ()
  "Load the archived ref index."
  (let ((archive-index (elfeed-ref-archive-filename ".index")))
    (if (file-exists-p archive-index)
        (with-temp-buffer
          (insert-file-contents archive-index)
          (setf elfeed-ref-archive (read (current-buffer))))
      (setf elfeed-ref-archive :empty))))

(defun elfeed-ref-archive-ensure ()
  "Ensure that the archive index is loaded."
  (unless elfeed-ref-archive (elfeed-ref-archive-load)))

(defun elfeed-ref-exists-p (ref)
  "Return non-nil if REF can be dereferenced."
  (elfeed-ref-archive-ensure)
  (or (and (hash-table-p elfeed-ref-archive)
           (not (null (gethash (elfeed-ref-id ref) elfeed-ref-archive))))
      (file-exists-p (elfeed-ref--file ref))))

(defun elfeed-deref (ref)
  "Fetch the content behind the reference REF, or nil if non-existent."
  (elfeed-ref-archive-ensure)
  (if (not (elfeed-ref-p ref))
      ref
    (let ((index (and (hash-table-p elfeed-ref-archive)
                      (gethash (elfeed-ref-id ref) elfeed-ref-archive)))
          (archive-file (elfeed-ref-archive-filename ".gz"))
          (coding-system-for-read 'utf-8))
      (if (and index (file-exists-p archive-file))
          (progn
            (unless elfeed-ref-cache
              (with-temp-buffer
                (insert-file-contents archive-file)
                (setf elfeed-ref-cache (buffer-string)))
              ;; Clear cache on next turn.
              (run-at-time 0 nil (lambda () (setf elfeed-ref-cache nil))))
            (substring elfeed-ref-cache (car index) (cdr index)))
        (let ((file (elfeed-ref--file ref)))
          (when (file-exists-p file)
            (with-temp-buffer
              (insert-file-contents file)
              (buffer-string))))))))

(defun elfeed-ref (content)
  "Create a reference to CONTENT, to be persistently stored."
  (if (elfeed-ref-p content)
      content
    (let* ((id (secure-hash 'sha1 (encode-coding-string content 'utf-8 t)))
           (ref (elfeed-ref--create :id id))
           (file (elfeed-ref--file ref)))
      (prog1 ref
        (unless (elfeed-ref-exists-p ref)
          (mkdir (file-name-directory file) t)
          (let ((coding-system-for-write 'utf-8)
                ;; Content data loss is a tolerable risk.
                ;; Fsync will occur soon on index write anyway.
                (write-region-inhibit-fsync t))
            (with-temp-file file
              (insert content))))))))

(defun elfeed-deref-entry (entry)
  "Move ENTRY's content to filesystem storage and return the entry."
  (let ((content (elfeed-entry-content entry)))
    (when (stringp content)
      (setf (elfeed-entry-content entry) (elfeed-ref content)))
    entry))

(defun elfeed-ref-delete (ref)
  "Remove the content behind REF from the database."
  (ignore-errors
    (delete-file (elfeed-ref--file ref))))

(defun elfeed-db-gc-empty-feeds ()
  "Remove feeds with no entries from the database."
  (elfeed-db-ensure)
  (funcall (elfeed-db-vtbl-gc-empty-feeds elfeed-db-vtbl)))

(defun elfeed-db--scan-1 (cb obj)
  "Scan OBJ for `elfeed-ref' references and call CB for each reference."
  ;; Written out as a loop to scan lists efficiently since Elisp lacks TCO.
  ;; Note that cyclic objects are not supported by the database format.
  (while (cond
          ((elfeed-ref-p obj)
           (funcall cb obj)
           nil)
          ((consp obj)
           (elfeed-db--scan-1 cb (car obj))
           (setf obj (cdr obj))))))

(defun elfeed-db--scan (cb)
  "Scan database for `elfeed-ref' references and call CB for each reference."
  (let ((feeds (make-hash-table :test #'equal)))
    (elfeed-db-visit (entry)
      ;; Search for references in content, entry metadata and feed metadata
      (elfeed-db--scan-1 cb (elfeed-entry-content entry))
      (elfeed-db--scan-1 cb (elfeed-entry-meta entry))
      ;; Only scan feed metadata once. Keep track of feeds in feeds.
      (unless (gethash (elfeed-entry-feed-id entry) feeds)
        (setf (gethash (elfeed-entry-feed-id entry) feeds) t)
        (elfeed-db--scan-1 cb (elfeed-feed-meta (elfeed-entry-feed entry)))))))

(defun elfeed-db-gc (&optional stats-p)
  "Clean up unused content from the content database.
If STATS-P is true, return the space cleared in bytes."
  (elfeed-db-gc-empty-feeds)
  (let* ((data (expand-file-name "data" elfeed-db-directory))
         (dirs (directory-files data t "\\`[0-9a-z]\\{2\\}\\'"))
         (ids (mapcan (lambda (d) (directory-files d nil nil t)) dirs))
         (table (make-hash-table :test #'equal)))
    (dolist (id ids)
      (setf (gethash id table) nil))
    (elfeed-db--scan
     (lambda (ref) (setf (gethash (elfeed-ref-id ref) table) t)))
    (cl-loop for id hash-keys of table using (hash-value used)
             for used-p = (or used (member id '("." "..")))
             when (and (not used-p) stats-p)
             sum (let* ((ref (elfeed-ref--create :id id))
                        (file (elfeed-ref--file ref)))
                   (* 1.0 (nth 7 (file-attributes file))))
             unless used-p
             do (elfeed-ref-delete (elfeed-ref--create :id id))
             finally (cl-loop for dir in dirs
                              when (directory-empty-p dir)
                              do (delete-directory dir)))))

(defun elfeed-db-pack ()
  "Pack all content into a single archive for efficient storage."
  (let* ((content-dest (elfeed-ref-archive-filename ".gz"))
         (content-temp (file-name-with-extension content-dest ".tmp.gz"))
         (index-dest (elfeed-ref-archive-filename ".index"))
         (index-temp (file-name-with-extension index-dest ".tmp.index"))
         (next-archive (make-hash-table :test #'equal))
         (coding-system-for-write 'utf-8)
         (write-region-inhibit-fsync nil)
         (packed ()))
    (make-directory (expand-file-name "data" elfeed-db-directory) t)
    (with-temp-file content-temp
      (elfeed-db--scan
       (lambda (ref)
         (when-let* (((not (gethash (elfeed-ref-id ref) next-archive)))
                     (content (elfeed-deref ref)))
           (push ref packed)
           (let ((start (1- (point))))
             (insert content)
             (setf (gethash (elfeed-ref-id ref) next-archive)
                   (cons start (1- (point)))))))))
    (with-temp-file index-temp
      (let ((standard-output (current-buffer))
            (print-level nil)
            (print-length nil)
            (print-circle nil))
        (prin1 next-archive)))
    ;; Rename two files, non-atomically! Instead use separate temporary data
    ;; directory, see https://github.com/emacs-elfeed/elfeed/pull/569.
    (rename-file content-temp content-dest t)
    (rename-file index-temp index-dest t)
    (setf elfeed-ref-cache nil)
    (setf elfeed-ref-archive next-archive)
    (mapc #'elfeed-ref-delete packed)
    :success))

(defun elfeed-db-compact ()
  "Minimize the Elfeed database storage size on the filesystem.
This requires that `auto-compression-mode' can handle
gzip-compressed files, so the gzip program must be in your PATH."
  (interactive)
  (unless (elfeed-gzip-supported-p)
    (error "Aborting compaction: gzip auto-compression-mode unsupported"))
  (elfeed-db-pack)
  (elfeed-db-gc))

(defun elfeed-db-gc-safe ()
  "Run `elfeed-db-gc' without triggering any errors, for use as a safe hook."
  (ignore-errors (elfeed-db-gc)))

;; Classic database implementation

(define-obsolete-variable-alias 'elfeed-db 'elfeed-db-classic "3.4.3")
(defvar elfeed-db-classic nil
  "The core database for elfeed.")

(define-obsolete-variable-alias 'elfeed-db-feeds 'elfeed-db-classic-feeds "3.4.3")
(defvar elfeed-db-classic-feeds nil
  "Feeds hash table, part of `elfeed-db'.")

(define-obsolete-variable-alias 'elfeed-db-entries 'elfeed-db-classic-entries "3.4.3")
(defvar elfeed-db-classic-entries nil
  "Entries hash table, part of `elfeed-db'.")

(define-obsolete-variable-alias 'elfeed-db-index 'elfeed-db-classic-index "3.4.3")
(defvar elfeed-db-classic-index nil
  "Collection of all entries sorted by date, part of `elfeed-db'.")

(define-obsolete-variable-alias 'elfeed-db-version 'elfeed-db-classic-version "3.4.3")
(defvar elfeed-db-classic-version 4
  "The database version this version of Elfeed expects to use.")

(defun elfeed-db-classic--empty ()
  "Create an empty database object."
  `(:version ,elfeed-db-classic-version
    :feeds ,(make-hash-table :test 'equal)
    :entries ,(make-hash-table :test 'equal)
    ;; Compiler may warn about this (bug#15327):
    :index ,(avl-tree-create #'elfeed-db-compare)))

(defun elfeed-db-classic-get-feed (id)
  "Get/create the feed for ID."
  (with-memoization (gethash id elfeed-db-classic-feeds)
    (elfeed-feed--create :id id)))

(defun elfeed-db-classic-get-entry (id)
  "Get the entry for ID."
  (gethash id elfeed-db-classic-entries))

(defun elfeed-db-classic-set-update-time-1 ()
  "Update the database last-update time."
  (setf elfeed-db-classic (plist-put elfeed-db-classic :last-update (float-time))))

(defun elfeed-db-classic-add (entries)
  "Add ENTRIES to the database."
  (cl-loop for entry in entries
           for id = (elfeed-entry-id entry)
           for original = (gethash id elfeed-db-classic-entries)
           for new-date = (elfeed-entry-date entry)
           for original-date = (and original (elfeed-entry-date original))
           do (elfeed-deref-entry entry)
           when original count
           (if (= new-date original-date)
               (elfeed-entry-merge original entry)
             (avl-tree-delete elfeed-db-classic-index id)
             (prog1 (elfeed-entry-merge original entry)
               (avl-tree-enter elfeed-db-classic-index id)))
           into change-count
           else count
           (setf (gethash id elfeed-db-classic-entries) entry)
           into change-count
           and do
           (progn
             (avl-tree-enter elfeed-db-classic-index id)
             (run-hook-with-args 'elfeed-new-entry-hook entry))
           finally
           (unless (zerop change-count)
             (elfeed-db-set-update-time)))
  :success)

(defun elfeed-db-classic-delete (entries)
  "Delete ENTRIES from database."
  (when entries
    (dolist (entry entries)
      (let ((id (elfeed-entry-id entry)))
        (avl-tree-delete elfeed-db-classic-index id)
        (remhash id elfeed-db-classic-entries)))
    (elfeed-db-set-update-time))
  :success)

;; no-ops because `elfeed-tag-1' already accomplishes the same thing
(defalias 'elfeed-db-classic-tag #'ignore
  "Modify the database to add TAGS to entries in ENTRY-OR-ENTRY-LIST.")
(defalias 'elfeed-db-classic-untag #'ignore
  "Modify the database to remove TAGS to entries in ENTRY-OR-ENTRY-LIST.")

(defun elfeed-db-classic-last-update ()
  "Return the last database update time in (`float-time') seconds."
  (or (plist-get elfeed-db-classic :last-update) 0))

(defun elfeed-db-classic-for-each (function)
  "Call FUNCTION with every entry id from newest to oldest."
  (avl-tree-mapc
   function
   elfeed-db-classic-index))

(defun elfeed-db-classic-save ()
  "Write the database index to the filesystem."
  (elfeed-db-ensure)
  (setf elfeed-db-classic (plist-put elfeed-db-classic :version elfeed-db-classic-version))
  (mkdir elfeed-db-directory t)
  (let* ((coding-system-for-write 'utf-8)
         (dest (expand-file-name "index" elfeed-db-directory))
         (temp (concat dest ".tmp"))
         (write-region-inhibit-fsync nil))
    ;; We write to a temporary file and rename to avoid corrupting the database
    ;; on crash. `file-precious-flag' is insufficient as it only works for
    ;; `save-buffer'.
    (with-temp-file temp
      (let ((standard-output (current-buffer))
            (print-level nil)
            (print-length nil)
            (print-circle nil))
        (princ (format ";;; Elfeed Database Index (version %s)\n\n"
                       elfeed-db-classic-version))
        (prin1 elfeed-db)))
    (rename-file temp dest t)
    :success))

(defun elfeed-db-classic-load ()
  "Load the database index from the filesystem."
  (let ((index (expand-file-name "index" elfeed-db-directory))
        (enable-local-variables nil)) ; don't set local variables from index!
    (if (not (file-exists-p index))
        (setf elfeed-db-classic (elfeed-db-classic--empty))
      ;; Override the default value for major-mode. There is no
      ;; preventing find-file-noselect from starting the default major
      ;; mode while also having it handle buffer conversion. Some
      ;; major modes crash Emacs when enabled in large buffers (e.g.
      ;; org-mode). This includes the Elfeed index, so we must not let
      ;; this happen.
      (cl-letf (((default-value 'major-mode) 'fundamental-mode))
        (with-current-buffer (find-file-noselect index :nowarn)
          (goto-char (point-min))
          (if (eql elfeed-db-classic-version 4)
              ;; May need to skip over dummy database
              (let ((db-1 (read (current-buffer)))
                    (db-2 (ignore-errors (read (current-buffer)))))
                (setf elfeed-db-classic (or db-2 db-1)))
            ;; Just load first database
            (setf elfeed-db-classic (read (current-buffer))))
          (kill-buffer))))
    ;; Perform an upgrade if necessary and possible
    (unless (equal (plist-get elfeed-db-classic :version) elfeed-db-classic-version)
      (setq elfeed-db-classic nil)
      (error "Elfeed database format is outdated.  Please upgrade first using an older version of Elfeed"))
    (setf elfeed-db-classic-feeds (plist-get elfeed-db-classic :feeds)
          elfeed-db-classic-entries (plist-get elfeed-db-classic :entries)
          elfeed-db-classic-index (plist-get elfeed-db-classic :index)
          ;; Internal function use required for security!
          (avl-tree--cmpfun elfeed-db-classic-index) #'elfeed-db-compare)))

(defun elfeed-db-classic-upgrade (_db)
  "Upgrade the database DB from a previous format."
  (error "Upgrade is not supported"))
(define-obsolete-function-alias 'elfeed-db-upgrade 'elfeed-db-classic-upgrade "3.4.3")

(defun elfeed-db-classic-loaded-p ()
  "Predicate for whether the database has been loaded.
This function does not call `elfeed-db-ensure'."
  elfeed-db-classic)

(defun elfeed-db-classic-unload-1 ()
  "Unload the database so that it can be operated on externally."
  (interactive)
  (setf elfeed-db-classic nil
        elfeed-db-classic-feeds nil
        elfeed-db-classic-entries nil
        elfeed-db-classic-index nil))

(defun elfeed-db-classic-size ()
  "Return a count of the number of entries in the database."
  (let ((count-table (hash-table-count elfeed-db-classic-entries))
        (count-tree (avl-tree-size elfeed-db-classic-index)))
    (if (= count-table count-tree)
        count-table
      (error "Elfeed database error: entry count mismatch"))))

(defun elfeed-db-classic-gc-empty-feeds ()
  "Remove feeds with no entries from the database."
  (let ((seen (make-hash-table :test 'equal)))
    (elfeed-db-visit (entry feed)
      (setf (gethash (elfeed-feed-id feed) seen) feed))
    (maphash (lambda (id _)
               (unless (gethash id seen)
                 (remhash id elfeed-db-classic-feeds)))
             elfeed-db-classic-feeds)))

;; Sqlite implementation

(defvar elfeed-db-sqlite nil
  "The sqlite connection.")

(defvar elfeed-db-sqlite-version 1
  "The version of the sqlite database schema.")

(defun elfeed-db-sqlite-get-feed (id)
  (if-let* ((feed-slots (car (sqlite-select elfeed-db-sqlite "SELECT url, title, author, meta FROM feed
WHERE id == $1"
                                            (list (prin1-to-string id)))))
            (feed-parsed (mapcar #'read feed-slots)))
      (cl-destructuring-bind (url title author meta) feed-parsed
        (elfeed-feed--create :id id
                             :url url
                             :title title
                             :author author
                             :meta meta))
    (sqlite-execute elfeed-db-sqlite "INSERT INTO feed(id) VALUES($1)"
                    (list (prin1-to-string id)))
    (elfeed-feed--create :id id)))

(defun elfeed-db-sqlite--get-tags (id)
  (mapcar (lambda (tag) (intern (car tag)))
          (sqlite-select elfeed-db-sqlite "SELECT tag FROM entry_tag
WHERE entry_id == $1"
                         (list (prin1-to-string id)))))

(defun elfeed-db-sqlite--get-enclosures (id)
  (mapcar #'car
          (sqlite-select elfeed-db-sqlite "SELECT enclosure FROM entry_enclosure
WHERE entry_id == $1"
                         (list (prin1-to-string id)))))

(defun elfeed-db-sqlite-get-entry (id)
  (when-let* ((slots (car (sqlite-select elfeed-db-sqlite "SELECT title, link, date, content, content_type, feed_id, meta FROM entry
WHERE id == $1"
                                         (list (prin1-to-string id))))))
    (cl-destructuring-bind (title link date content content-type feed-id meta) slots
      (elfeed-entry--create :id id
                            :title (read title)
                            :link (read link)
                            :date date
                            :content (read content)
                            :content-type (read content-type)
                            :tags (elfeed-db-sqlite--get-tags id)
                            :enclosures (elfeed-db-sqlite--get-enclosures id)
                            :feed-id (read feed-id)
                            :meta (read meta)))))

(defun elfeed-db-sqlite-set-update-time-1 ()
  (sqlite-execute elfeed-db-sqlite "UPDATE metadata SET last_update = $1" (list (float-time))))

(defun elfeed-db-sqlite-add (_entries)
;;   (cl-loop for entry in entries
;;            for id = (elfeed-entry-id entry)
;;            for values = (list (prin1-to-string id)
;;                               (prin1-to-string (elfeed-entry-title entry))
;;                               (prin1-to-string (elfeed-entry-link entry))
;;                               (elfeed-entry-date entry)
;;                               (prin1-to-string (elfeed-entry-content entry))
;;                               (prin1-to-string (elfeed-entry-content-type entry))
;;                               (prin1-to-string (elfeed-entry-feed-id entry))
;;                               (prin1-to-string (elfeed-entry-meta entry)))
;;            do (progn
;;                 (sqlite-select elfeed-db-sqlite
;;                                "INSERT INTO entry(id, title, link, date, content, content_type, feed_id, meta)
;; VALUES($1, $2, $3, $4, $5, $6, $7, $8)
;; ON CONFLICT(id) DO NOTHING"
;;                                values)
;;                 (elfeed-db-sqlite-tag (list entry) (elfeed-entry-tags entry))))
  (error "todo"))

(defun elfeed-db-sqlite-delete (entries)
  (cl-loop for entry in entries
           do (sqlite-select elfeed-db-sqlite
                             "DELETE FROM entry
WHERE id == $1"
                             (list (prin1-to-string (elfeed-entry-id entry))))))

(defun elfeed-db-sqlite-tag (entries tags)
  (dolist (entry entries)
    (dolist (tag tags)
      (sqlite-select elfeed-db-sqlite "INSERT OR REPLACE INTO entry_tag(entry_id, tag) VALUES($1, $2)"
                     (list (prin1-to-string (elfeed-entry-id entry)) (symbol-name tag))))))

(defun elfeed-db-sqlite-untag (entries tags)
  (dolist (entry entries)
    (dolist (tag tags)
      (sqlite-select elfeed-db-sqlite "DELETE FROM entry_tag
WHERE entry_id == $1 AND tag == $2"
                     (list (prin1-to-string (elfeed-entry-id entry)) (symbol-name tag))))))

(defun elfeed-db-sqlite-last-update ()
  (car (sqlite-select elfeed-db-sqlite "SELECT version FROM metadata")))

(defun elfeed-db-sqlite-for-each (function)
  (let ((set (sqlite-select elfeed-db-sqlite
                            "SELECT id FROM entry
ORDER BY date DESC"
                            nil
                            'set)))
    (while-let ((row (sqlite-next set)))
      (funcall function (car row)))))

(defun elfeed-db-sqlite-feed-entries (feed-or-id)
  (let ((feed-id (if (elfeed-feed-p feed-or-id)
                     (elfeed-feed-id feed-or-id)
                   feed-or-id)))
    (mapcar (lambda (row)
              (cl-destructuring-bind (id title link date content content-type meta) row
                  (let ((id (read id)))
                    (elfeed-feed--create :id id
                                         :title (read title)
                                         :link (read link)
                                         :date date
                                         :content (read content)
                                         :content-type (read content-type)
                                         :tags (elfeed-db-sqlite--get-tags id)
                                         :enclosures (elfeed-db-sqlite--get-enclosures id)
                                         :meta (read meta)
                                         :feed-id feed-id))))
            (sqlite-select elfeed-db-sqlite
                           "SELECT FROM id, title, link, date, content, content_type, meta
WHERE feed_id == $1"
                           (list feed-id)))))

(defun elfeed-db-sqlite-get-all-tags ()
  (mapcar (lambda (tag) (intern (car tag)))
          (sqlite-select elfeed-db-sqlite "SELECT DISTINCT tag FROM entry_tag
ORDER BY tag ASC")))

(defun elfeed-db-sqlite-save ()
  (sqlite-commit elfeed-db-sqlite))

(defun elfeed-db-sqlite-load ()
  "Load the sqlite database."
  (unless (sqlite-available-p)
    (error "Emacs was not built with sqlite support"))

  (let* ((path (expand-file-name "index.sqlite" elfeed-db-directory))
         (old (file-exists-p path))
         (sqlite (sqlite-open path)))
    (if old
        (let ((version (caar (sqlite-select sqlite "SELECT version FROM metadata"))))
          (unless (eql version elfeed-db-sqlite-version)
            (error "Elfeed database format is outdated.  Please upgrade first using an older version of Elfeed")))
      (sqlite-execute-batch sqlite
                            "CREATE TABLE IF NOT EXISTS feed(
id TEXT NOT NULL PRIMARY KEY,
url TEXT NOT NULL DEFAULT 'nil',
title TEXT NOT NULL DEFAULT 'nil',
author TEXT NOT NULL DEFAULT 'nil',
meta TEXT NOT NULL DEFAULT 'nil'
);

CREATE TABLE IF NOT EXISTS entry(
id TEXT PRIMARY KEY,
title TEXT NOT NULL DEFAULT 'nil',
link TEXT NOT NULL DEFAULT 'nil',
date DOUBLE NOT NULL DEFAULT 0.0,
content TEXT NOT NULL DEFAULT 'nil',
content_type TEXT NOT NULL DEFAULT 'nil',
feed_id TEXT NOT NULL,
meta TEXT NOT NULL DEFAULT 'nil',

FOREIGN KEY(feed_id) REFERENCES feed(id)
);

CREATE TABLE IF NOT EXISTS entry_enclosure(
entry_id TEXT NOT NULL,
enclosure TEXT NOT NULL,

FOREIGN KEY(entry_id) REFERENCES entry(id)
);

CREATE TABLE IF NOT EXISTS entry_tag(
entry_id INTEGER NOT NULL,
tag TEXT NOT NULL,

FOREIGN KEY(entry_id) REFERENCES entry(id)
);

CREATE TABLE IF NOT EXISTS metadata(
version INTEGER NOT NULL,
last_update DOUBLE,

enforcer INT DEFAULT 0 NOT NULL CHECK(enforcer == 0),
UNIQUE(enforcer)
)")
      (sqlite-execute sqlite "INSERT INTO metadata(version) values($1)" (list elfeed-db-sqlite-version)))
    (setq elfeed-db-sqlite sqlite)))

(defun elfeed-db-sqlite-loaded-p ()
  "Return non-nil when the database is loaded."
  elfeed-db-sqlite)

(defun elfeed-db-sqlite-unload-1 ()
  (sqlite-close elfeed-db-sqlite)
  (setq elfeed-db-sqlite nil))

(defun elfeed-db-sqlite-size ()
  (caar (sqlite-select elfeed-db-sqlite "SELECT COUNT(1) FROM entry")))

(defun elfeed-db-sqlite-gc-empty-feeds ()
  (sqlite-execute elfeed-db-sqlite
                  "DELETE FROM feed
WHERE feed.id NOT IN (SELECT feed_id FROM entry)"))

(unless noninteractive
  (add-hook 'kill-emacs-hook #'elfeed-db-gc-safe :append)
  (add-hook 'kill-emacs-hook #'elfeed-db-save-safe))

(define-obsolete-function-alias 'copy-elfeed-entry
  #'copy-sequence "3.4.2")
(define-obsolete-function-alias 'copy-elfeed-feed
  #'copy-sequence "3.4.2")
(define-obsolete-function-alias 'copy-elfeed-ref
  #'copy-sequence "3.4.2")

(provide 'elfeed-db)
;;; elfeed-db.el ends here
