;;; elfeed-summary.el --- show feed summary -*- lexical-binding: t; -*-
;;
;; This is free and unencumbered software released into the public domain.
;;
;; Authors: mnp, rgemulla

;;; Code:

(require 'elfeed-search)

(defcustom elfeed-summary-sort-key "TITLE"
  "Sort the summary by TOTAL, UNREAD, DATE, or TITLE."
  :group 'elfeed
  :type 'string)

(defcustom elfeed-summary-show-read nil
  "Show feads with without unread messages in the summary, too."
  :group 'elfeed
  :type 'boolean)

(defcustom elfeed-summary-line-format "%10s %-55s % 7s % 5s"
  "Format of summary. Arguments are: date, title, unread count, total count."
  :group 'elfeed
  :type 'string)

(defcustom elfeed-summary-date-format '("%Y-%m-%d" 10 :left)
  "The `format-time-string' format, target width, and alignment for dates.

This should be (string integer keyword) for (format width
alignment). Possible alignments are :left and :right."
  :group 'elfeed
  :type '(list string integer (choice (const :left) (const :right))))

(defface elfeed-summary-date-face
  '((t :inherit elfeed-search-date-face))
  "Face used in summary mode for dates."
  :group 'elfeed)

(defface elfeed-summary-feed-face
  '((t :inherit elfeed-search-feed-face :weight bold))
  "Face used in summary mode for feed titles."
  :group 'elfeed)

(defface elfeed-summary-feed-face-read
  '((t :inherit elfeed-summary-feed-face :weight normal))
  "Face used in summary mode for feed titles."
  :group 'elfeed)

(defface elfeed-summary-counts-face
  '((t :inherit elfeed-search-tag-face))
  "Face used in summary mode for unread/total count."
  :group 'elfeed)

;; steal from siblings
(defalias 'elfeed-summary-format-date #'elfeed-search-format-date)

(defun elfeed-summary--gather ()
  (let ((table (make-hash-table :test 'eq)))
    ;; Stuff all the feeds we care about in a table
    ;; Each entry is (total-count unread-count most-recent-entry)
    (dolist (url (elfeed-feed-list))
      (setf (gethash (elfeed-db-get-feed url) table)
            (list 0 0 nil)))

    ;; Visit every database entry efficiently ordered by time, descending
    (with-elfeed-db-visit (entry feed)
      (let ((info (gethash feed table)))
        (unless info
          (setq info (list 0 0 nil))
          (puthash feed info table))
        (when (= 1 (cl-incf (car info)))
          (setf (caddr info) entry))
          (when (memq 'unread (elfeed-entry-tags entry))
            (cl-incf (cadr info)))))

    ;; Create a table of the results
    (cl-loop for feed being the hash-keys of table
             using (hash-values info)
             for (total unread most-recent) = info
                 for title = (or (elfeed-feed-title feed) "<no-title>")
		 for date = (when most-recent
			      (elfeed-summary-format-date (elfeed-entry-date most-recent)))
                 if (and (> total 0)
                         (or elfeed-summary-show-read (> unread 0)))
		 collect (list
			  :feedid
                          ;; trim feed options
                          (string-trim-right
                           ;; trim main feed for elfeed protocol, keep subfeed
                           (string-trim-left (elfeed-feed-id feed) ".*::")
                           "\\?.*")
			  :title title
                          :total total
                          :unread unread
                          :date (if date date ""))
		 into items
		 finally return (elfeed-summary--sort! items))))

(defun elfeed-summary--sort! (items)
   (cl-sort items
	    (cond ((equal elfeed-summary-sort-key "DATE") #'string<)
		  ((equal elfeed-summary-sort-key "TOTAL") #'<)
		  ((equal elfeed-summary-sort-key "UNREAD") #'<)
		  ((equal elfeed-summary-sort-key "TITLE") #'string<)
		  (t (error "elfeed-summary-sort-key must be DATE, TOTAL, UNREAD, or TITLE")))
	    :key (lambda (x)
                   (plist-get x
	                      (cond ((equal elfeed-summary-sort-key "DATE") :date)
		                    ((equal elfeed-summary-sort-key "TOTAL") :total)
		                    ((equal elfeed-summary-sort-key "UNREAD") :unread)
		                    ((equal elfeed-summary-sort-key "TITLE") :title))))))

(defun elfeed-summary--selected-feed ()
  "Locate the feedid hidden in the text property of the current line"
  (plist-get (plist-get (text-properties-at (point)) 'feed) :feedid))

(defun elfeed-summary--selected-feed-unread-count ()
  (plist-get (plist-get (text-properties-at (point)) 'feed) :unread))

(defun elfeed-summary-show-entry ()
  "Display the currently selected item in a buffer."
  (interactive)
  (let ((feedid (elfeed-summary--selected-feed))
        (unread (elfeed-summary--selected-feed-unread-count)))
    (when feedid
      (elfeed)
      (if (> unread 0)
          (elfeed-search-set-filter (concat "+unread =" feedid))
        (elfeed-search-set-filter (concat "=" feedid))))))

(defun elfeed-summary-search-entry ()
  "Display the currently selected item in a buffer."
  (interactive)
  (let ((feedid (elfeed-summary--selected-feed)))
    (when feedid
      (elfeed)
      (let ((elfeed-search-filter (concat "=" feedid " ")))
        (elfeed-search-live-filter)))))

(defvar elfeed-summary-mode-map
  (setq elfeed-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" 'quit-window)
      (define-key map (kbd "RET") 'elfeed-summary-show-entry)
      (define-key map "gr" 'elfeed-update)
      (define-key map "n" 'next-line)
      (define-key map "p" 'previous-line)
      (define-key map "s" 'elfeed-summary-search-entry)
      (define-key map "f" 'elfeed))))
      ;; TODO: command to force update all feeds
      ;; TODO: command to force update current feed
      ;; TODO: command to mark all of current feed up to date
      ;;
  "Keymap for elfeed-summary-mode.")

(defun elfeed-summary--buffer ()
  (get-buffer-create "*elfeed-summary*"))

(defun elfeed-summary-update (&rest _)
  "Recompute feed summaries"
  (interactive)
  (with-current-buffer (elfeed-summary--buffer)
    (let ((p (point))
          (inhibit-read-only t))
      (erase-buffer)
      (dolist (item (elfeed-summary--gather))
	(let ((line (format elfeed-summary-line-format
			    (propertize (plist-get item :date) 'face 'elfeed-summary-date-face)
			    (propertize (plist-get item :title) 'face
                                        (if (> (plist-get item  :unread) 0) 'elfeed-summary-feed-face 'elfeed-summary-feed-face-read))
			    (propertize (int-to-string (plist-get item  :unread)) 'face 'elfeed-summary-counts-face)
			    (propertize (int-to-string (plist-get item :total)) 'face 'elfeed-summary-counts-face))))
	  ;; A little trick here: add a property field holding the feed ID; read back when jumping to that feed
	  (insert (propertize line 'feed item))
	  (insert "\n")))
      (insert "End of summary.\n")
      (goto-char p))))

(defun elfeed-summary--update (&rest _)
  (when (buffer-live-p (elfeed-summary--buffer))
    (elfeed-summary-update)))

(defun elfeed-summary-mode ()
  "Major mode for listing elfeed feeds and show some info about them.
\\{elfeed-summary-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map elfeed-summary-mode-map)
  (setq major-mode 'elfeed-summary-mode
        mode-name "elfeed-summary"
        header-line-format (format elfeed-summary-line-format "Date" " Title" " Unread" "Total")
        truncate-lines t
        buffer-read-only t)
  (buffer-disable-undo)
  (hl-line-mode)
  (elfeed-summary-update)
  (add-hook 'elfeed-update-hooks #'elfeed-summary--update)
)

(defun elfeed-summary ()
  "Show a summary of all feeds."
  (interactive)
  (switch-to-buffer (get-buffer-create (elfeed-summary--buffer)))
  (elfeed-summary-mode))

(provide 'elfeed-summary)

;;; elfeed-summary.el ends here
