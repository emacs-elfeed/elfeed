;;; elfeed-tree.el --- Show feeds as a tree structure -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Daniel Mendler <mail@daniel-mendler.de>

;;; Commentary:

;; The `elfeed-tree' buffer gives an overview over all feeds and tags.
;; Open the tree buffer via the command `elfeed-tree'.  The feeds are
;; visualized as a tree using the feed auto tags.  The first auto tag
;; is the root node, the second tag comes below, and so on.  Feeds
;; with the same auto tags are grouped together.
;;
;; For example the configuration
;;
;;     (setq elfeed-feeds
;;       '(("https://yhetil.org/emacs-devel/new.atom" emacs lists devel)
;;         ("https://yhetil.org/emacs-bugs/new.atom" emacs lists bugs)
;;         ("https://old.reddit.com/r/emacs.rss" emacs reddit)))
;;
;; will lead to a tree of the following form.
;;
;;     emacs
;;       ├─● /r/reddit
;;       lists
;;         ├─● emacs-devel
;;         ╰─● emacs-bugs
;;     [all feeds]
;;       ├─● …
;;       ╰─● emacs-devel
;;     [all tags]
;;       ├─● …
;;       ╰─● emacs
;;            ├─● …
;;            ╰─● emacs-devel
;;
;; `outline-minor-mode' is enabled in the tree buffer.  Unfold the
;; tree nodes with TAB or S-TAB, jump to an entry via RET or by
;; clicking.  This feature was inspired by the elfeed-summary package.

;;; Code:

(eval-when-compile (require 'subr-x))

(require 'elfeed)
(require 'elfeed-search)
(require 'outline)

(defgroup elfeed-tree ()
  "Elfeed tree buffer."
  :group 'elfeed)

(defcustom elfeed-tree-filter "@6months"
  "Query string added to filter."
  :type 'string)

(defcustom elfeed-tree-nodes ["├─●" "╰─●" "│  " "   "]
  "Strings used to visualize nodes of the tree."
  :type '(vector string string string string))

(defgroup elfeed-tree-faces ()
  "Elfeed tree buffer faces."
  :group 'elfeed-tree)

(defface elfeed-tree-highlight-unread-face
  '((t :inherit warning))
  "Face used in tree mode to highlight unread entries.")

(defvar elfeed-tree-header-function #'elfeed-tree--header
  "Function that returns the string to be used for the header line.")

(defvar elfeed-tree-update-hook nil
 "Functions in this list are called after the tree buffer has been updated.")

(defvar elfeed-tree--update-timer nil
  "Timer to debounce search buffer updates.")

(defvar elfeed-tree--last-update 0
  "The last time the buffer was redrawn in epoch seconds.")

(defvar-keymap elfeed-tree-mode-map
  :doc "Keymap for `elfeed-tree-mode'."
  :parent special-mode-map
  "RET" #'elfeed-tree-search
  "<elfeed-filter>" #'elfeed-tree-search
  "s" #'elfeed-search-new-live
  "n" #'next-line
  "p" #'previous-line
  "T" #'elfeed-tree-set-title
  "G" #'elfeed-update
  "TAB" #'outline-cycle
  "<backtab>" #'outline-cycle-buffer)

(defun elfeed-tree--header ()
  "Computes the string to be used as the header line."
  (or (elfeed--header-jobs)
      (elfeed--header-update elfeed-tree--last-update)))

(easy-menu-define elfeed-tree-mode-menu elfeed-tree-mode-map
  "Menu for `elfeed-tree-mode'."
  '("Elfeed Tree"
    ["Search for feed or tag" elfeed-tree-search]
    ["Set feed title" elfeed-tree-set-feed-title]
    "--"
    ["Fetch all" elfeed-search-fetch]
    ["Fetch feed" elfeed-update-feed]
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

(defun elfeed-tree-search (filter)
  "Go to search buffer limited to FILTER string at point."
  (interactive (list (or (get-text-property (pos-bol) 'elfeed-filter)
                         (user-error "No filter at point")))
               elfeed-tree-mode)
  (push-mark nil 'nomsg)
  (elfeed-search (concat elfeed-tree-filter
                         (and (not (equal elfeed-tree-filter "")) " ")
                         filter)))

(defun elfeed-tree-set-title (feed title)
  "Set TITLE of FEED at point."
  (interactive
   (let ((feed (or (get-text-property (pos-bol) 'elfeed-feed)
                   (user-error "No feed at point"))))
     (list feed (read-from-minibuffer "Feed title: "
                                      (elfeed-meta--title feed))))
   elfeed-tree-mode)
  (setf (elfeed-meta feed :title) title)
  (elfeed-tree-update :force))

(define-derived-mode elfeed-tree-mode special-mode "elfeed-tree"
  "Major mode for listing elfeed feeds as a tree."
  :syntax-table nil :abbrev-table nil :interactive nil
  (setq-local truncate-lines t
              mode-line-modified nil
              mode-line-mule-info nil
              mode-line-remote nil
              revert-buffer-function #'elfeed-tree--update-force
              bookmark-make-record-function #'elfeed-tree-bookmark-make-record
              default-directory (elfeed-default-directory)
              outline-regexp "\\*+"
              outline-minor-mode-cycle t
              outline-minor-mode-cycle-filter nil
              hl-line-sticky-flag t)
  (elfeed--header-line-format 'elfeed-tree-header-function)
  (buffer-disable-undo)
  (hl-line-mode)
  (add-hook 'elfeed-untag-hook #'elfeed-tree--tag)
  (add-hook 'elfeed-tag-hook #'elfeed-tree--tag)
  (add-hook 'elfeed-update-hook #'elfeed-tree--update-debounce)
  (add-hook 'elfeed-update-init-hook #'elfeed-tree--update-force)
  (elfeed-db--save-on-quit)
  (elfeed-tree-update :force)
  (outline-minor-mode)
  (outline-hide-sublevels 1))

;;;###autoload
(defun elfeed-tree ()
  "Enter `elfeed-tree' buffer."
  (interactive)
  (switch-to-buffer (elfeed-tree--buffer))
  (unless (eq major-mode 'elfeed-tree-mode)
    (elfeed-tree-mode)))

(defun elfeed-tree--buffer ()
  "Create and return tree buffer."
  (get-buffer-create "*elfeed-tree*"))

(defun elfeed-tree--tag (_entries tags)
  "Refresh if unread TAGS have changed."
  (when (memq 'unread tags)
    (setq elfeed-tree--last-update 0)
    (elfeed-tree-update)))

(defun elfeed-tree--update-force (&rest _)
  "Call `elfeed-tree-update' with argument :force.
The function is used as hook.  Instead of this function, you usually
want to use `elfeed-tree-update'."
  (elfeed-tree-update :force))

(defun elfeed-tree--update-debounce (&rest _)
  "Call `elfeed-tree-update' with debouncing.
The function is used as hook.  Instead of this function, you usually
want to use `elfeed-tree-update'."
  (elfeed-tree-update))

(defun elfeed-tree-update (&optional force)
  "Update the `elfeed-tree' buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed.
Otherwise debounce by `elfeed-search-update-delay' and only redraw when
there are changes.  When called interactively FORCE is t, and the
command behaves just like `revert-buffer'."
  (when elfeed-tree--update-timer
    (cancel-timer elfeed-tree--update-timer)
    (setq elfeed-tree--update-timer nil))
  (when-let* ((buffer (get-buffer "*elfeed-tree*")))
    (if force
        (elfeed-tree--update-immediately buffer :force)
      (setf elfeed-tree--update-timer
            (run-at-time elfeed-search-update-delay nil
                         #'elfeed-tree--update-immediately buffer)))))

(defun elfeed-tree--sort (nodes)
  "Sort tree NODES by unread count and name."
  (sort nodes (lambda (x y)
                (if (= (cadr x) (cadr y))
                    (string< (car x) (car y))
                  (> (cadr x) (cadr y))))))

(defun elfeed-tree--collect ()
  "Collect list of feeds and tags from the database.
Returns a pair of two lists of the format.  The feed list entries have
the format (title unread read feed tags).  The tag list entries have the
format (tag unread read)."
  (let ((feeds-ht (make-hash-table :test #'equal))
        (tags-ht (make-hash-table :test #'eq)))
    (elfeed-db-visit (entry feed)
      (let* ((tags (elfeed-entry-tags entry))
             (unread (memq 'unread tags))
             (feed-id (elfeed-feed-id feed)))
        ;; Collect tags in tags hash table.
        (dolist (tag tags)
          (unless (hash-table-contains-p tag tags-ht)
            (puthash tag (list tag 0 0) tags-ht))
          (if unread
              (cl-incf (cadr (gethash tag tags-ht)))
            (cl-incf (caddr (gethash tag tags-ht)))))
        ;; Collect feeds in feeds hash table.
        (unless (hash-table-contains-p feed-id feeds-ht)
          (puthash feed-id (list (elfeed-meta--title feed) 0 0 feed
                                 (elfeed-feed-autotags feed))
                   feeds-ht))
        (if unread
            (cl-incf (cadr (gethash feed-id feeds-ht)))
          (cl-incf (caddr (gethash feed-id feeds-ht))))))
    (cons (hash-table-values feeds-ht) (hash-table-values tags-ht))))

(defun elfeed-tree--build-nested (nodes)
  "Build a nested tree from a flat list of NODES.
For each node the list of tags is taken and turned into parent nodes."
  (let (children leaves)
    (cl-loop
     for (title unread read feed tags) in nodes
     for item = (list title unread read feed (cdr tags)) do
     (if (car tags)
         (push item (alist-get (car tags) children))
       (push item leaves)))
    (cl-loop for x in children do
             (cl-callf elfeed-tree--build-nested (cdr x)))
    (list children leaves)))

(defun elfeed-tree--build-tags (feeds tags stats)
  "Build an all tags tree from the list of all FEEDS and TAGS.
STATS is the unread/read/count statistics."
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (feed feeds)
      (dolist (tag (nth 4 feed))
        (push feed (gethash tag ht))))
    (let ((children
           (cl-loop
            for (tag unread read) in tags
            for feeds = (gethash tag ht)
            collect (list tag unread read (length feeds) nil feeds))))
      `(("[all tags]" ,@stats ,children nil)))))

(defun elfeed-tree--depth (nodes)
  "Compute tree depth given a list of NODES."
  (if nodes
      (cl-loop for (_tag _unread _read _count children _leaves) in nodes
               maximize (if children (1+ (elfeed-tree--depth children)) 0))
    0))

(defun elfeed-tree--stats (nodes)
  "Compute sum of unread and read counts for parent nodes.
NODES is a list of tree nodes."
  (cl-loop for (tag children leaves) in nodes
           for rec = (elfeed-tree--stats children)
           collect
           (list
            tag
            ;; Sum unread counts from children and leaves
            (+ (cl-loop for (_t u _r . _) in rec sum u)
               (cl-loop for (_t u _r . _) in leaves sum u))
            ;; Sum read counts from children and leaves
            (+ (cl-loop for (_t _u r . _) in rec sum r)
               (cl-loop for (_t _u r . _) in leaves sum r))
            ;; Sum feed number from children and leaves
            (+ (cl-loop for (_t _u _r c . _) in rec sum c)
               (length leaves))
            rec leaves)))

(defun elfeed-tree--flatten (nodes)
  "Flatten tree NODES."
  (cl-loop
   for (tag children leaves) in nodes collect
   (progn
     ;; Merge nodes if a node has only a single subnode.
     (while (and (not leaves) (length= children 1))
       (setq tag (format "%s %s" tag (caar children))
             leaves (caddar children)
             children (cadar children)))
     ;; Drop parent nodes with only a single feed as leaf.
     ;; Keep these nodes with this code:
     ;;  (list tag (elfeed-tree--flatten children) leaves)
     (cl-loop for entry in (elfeed-tree--flatten children)
              for (_tg ts us) = entry
              if (and (not ts) (length= us 1)) collect (car us) into new-leaves
              else collect entry into new-children
              finally return
              (list tag new-children (nconc leaves new-leaves))))))

(defun elfeed-tree--count-unread (unread read)
  "Format unread/total count for a feed line given UNREAD and READ."
  (format "%4s/%-5s"
          (if (> unread 0)
              (format
               (propertize "%s" 'face 'elfeed-tree-highlight-unread-face)
               unread)
            unread)
          (+ unread read)))

(defun elfeed-tree--node (idx)
  "Return string for the tree visualization given IDX."
  (propertize (if (< idx 2) " " "*") 'display
              (substring (aref elfeed-tree-nodes idx))))

(defun elfeed-tree--title (indent title unread read count tags)
  "Insert TITLE into buffer.
INDENT is the indentation prefix, UNREAD and READ the respective counts,
COUNT the number of feeds and TAGS the list of tags."
  (setq title
        (concat
         indent (propertize " " 'invisible t) (format "%s" title)
         (propertize " "
                     'display (format " (%s/%s:%s)"
                                      (if (> unread 0)
                                          (format
                                           (propertize
                                            "%s" 'face 'elfeed-tree-highlight-unread-face)
                                           unread)
                                        unread)
                                      (+ unread read)
                                      count))))
  (add-face-text-property
   0 (length title)
   (aref outline-font-lock-faces
         (1- (min (length indent) (length outline-font-lock-faces))))
   'append title)
  (elfeed-add-properties
   title
   'elfeed-tree (mapconcat (lambda (x) (format "%s" x)) tags " ")
   'elfeed-filter
   (elfeed-search--tag-filter
    (let ((tags (cl-loop for x in tags
                         if (and (stringp x) (not (string-prefix-p "[" x)))
                         nconc (mapcar #'intern (split-string x))
                         else if (symbolp x)
                         nconc (list x))))
      (if (and (> unread 0) (not (memq 'unread tags)))
          `(,@tags unread)
        tags)))
   'follow-link [elfeed-filter]
   'mouse-face 'highlight)
  (insert title ?\n))

(defun elfeed-tree--print (indent tags title-width depth nodes)
  "Print tree NODES.
INDENT is the current indentation prefix string.
TAGS the list of outer tags which are added to the filter.
TITLE-WIDTH the width of the feed title.
DEPTH the tree depth."
  (setq indent (or indent (propertize "*" 'invisible t)))
  (cl-loop
   with align1 = (+ 10 (* (+ 2 depth) (length (aref elfeed-tree-nodes 0))))
   with align2 = (+ 1 align1 title-width)
   with align1 = (propertize " " 'display `(space :align-to ,align1))
   with align2 = (propertize " " 'display `(space :align-to ,align2))
   with level = (length indent)
   for (tag unread read count children leaves) in (elfeed-tree--sort nodes)
   for node-idx downfrom (length nodes) do
   (let ((subtags (append tags (list tag)))
         (subindent (concat indent
                            (elfeed-tree--node
                             (if (or (= level 1) (= node-idx 1)) 3 2)))))
     (elfeed-tree--title indent tag unread read count subtags)
     (cl-loop
      for (title unread read feed _tags) in (elfeed-tree--sort leaves)
      for leaf-idx downfrom (length leaves) do
      (insert
       (elfeed-add-properties
        (concat subindent
                (elfeed-tree--node
                 (if (and (not children) (= leaf-idx 1)) 1 0))
                (mapconcat (lambda (_)
                             (propertize
                              " " 'display
                              (substring (aref elfeed-tree-nodes 3))))
                           (make-list (- depth level -1) 0))
                (elfeed-tree--count-unread unread read)
                align1
                (elfeed-add-properties
                 (elfeed-format-column title title-width :left)
                 'face 'elfeed-search-feed-face)
                align2
                (elfeed-feed-id feed))
        'elfeed-feed feed
        'elfeed-tree (concat
                      (mapconcat (lambda (x) (format "%s" x)) subtags " ")
                      " " (elfeed-feed-id feed))
        'elfeed-filter (concat (elfeed-search--feed-filter feed)
                               (and (> unread 0) " +unread"))
        'follow-link [elfeed-filter]
        'mouse-face 'highlight)
       "\n"))
     (elfeed-tree--print subindent subtags title-width depth children))))

(defun elfeed-tree--update-immediately (buffer &optional force)
  "Immediately update the `elfeed-tree' BUFFER.
If FORCE is nil, only refresh the buffer when the database changed.  Do
not use this function directly.  Instead use `elfeed-tree-update'."
  (when (and (buffer-live-p buffer)
             (or force (< elfeed-tree--last-update (elfeed-db-last-update))))
    (with-current-buffer buffer
      (elfeed-with-position elfeed-tree
        (let* ((restore (outline-revert-buffer-restore-visibility))
               (inhibit-read-only t)
               (feeds+tags (elfeed-tree--collect))
               (feeds (car feeds+tags))
               (tags (cdr feeds+tags))
               (nodes (elfeed-tree--build-nested feeds))
               (tree (elfeed-tree--stats (elfeed-tree--flatten (car nodes))))
               (tree-depth (max 2 (elfeed-tree--depth tree)))
               (untagged-feeds-tree (when (cadr nodes)
                                      (elfeed-tree--stats
                                       `(("[untagged feeds]" nil ,(cadr nodes))))))
               (all-feeds-tree (elfeed-tree--stats `(("[all feeds]" nil ,feeds))))
               (title-width (cl-loop for (title . _) in feeds
                                     maximize (string-width title))))
          (erase-buffer)
          (goto-char (point-min))
          (elfeed-tree--print nil nil title-width tree-depth tree)
          (elfeed-tree--print nil nil title-width tree-depth untagged-feeds-tree)
          (elfeed-tree--print nil nil title-width tree-depth all-feeds-tree)
          (elfeed-tree--print nil nil title-width tree-depth
                              (elfeed-tree--build-tags
                               feeds tags
                               (take 3 (cdar all-feeds-tree))))
          (when restore (funcall restore))
          (setq elfeed-tree--last-update (float-time))
          (run-hooks 'elfeed-tree-update-hook)))))
  ;; Always force a header line update
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (force-mode-line-update))))

;;;###autoload
(defun elfeed-tree-bookmark-handler (_record)
  "Jump to an `elfeed-tree' bookmark RECORD."
  (elfeed-tree))
(put 'elfeed-tree-bookmark-handler 'bookmark-handler-type "Elfeed")

(defun elfeed-tree-bookmark-make-record ()
  "Return a bookmark record for the current `elfeed-tree' buffer."
  `("elfeed tree" (handler . ,#'elfeed-tree-bookmark-handler)))

(provide 'elfeed-tree)
;;; elfeed-tree.el ends here
