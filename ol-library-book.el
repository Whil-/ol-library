;;; ol-library-book-*
(require 'ol-library-core)

(defcustom ol-library-book-dir "~/books"
  "The path to be used for book-links."
  :group 'ol-library
  :type 'directory
  :safe t)

(defcustom ol-library-book-regexp (concat "\\`[^.].*\\."
                                          (regexp-opt (list "pdf" "epub" "djvu" "djv"))
                                          "\\'")
  "Regular expression to match files for `ol-library-book-store-link'"
  :group 'ol-library
  :type 'regexp
  :safe t)

(defcustom ol-library-book-to-path-function-list '(ol-library-plain-folder-format
                                                   ol-library-year-folder-format)
  "List of functions parsing a book string into a folder-path.
The first function in this list defines the preferred function
which will be used when creating new attachment folders. All
functions of this list will be tried when looking for books."
  :group 'ol-library
  :type '(repeat (function :tag "Function with bookname as input"))
  :safe t)

(defun ol-library-book--collect ()
    "Create collection of books."
    (ol-library-collect ol-library-book-dir
                        ol-library-book-regexp
                        #'ol-library-cleanup-remove-tags-and-suffix))

(defun ol-library-book--expand (book)
  "Expand an book into a path.
Based on `ol-library-book-to-path-function-list' andn
`ol-library-book-dir'."
  (ol-library-dir-from-library-object book
                                       ol-library-book-to-path-function-list
                                       ol-library-book-dir
                                       #'ol-library-fixup-complete-filename-from-path))

(defun ol-library-book-follow (book arg)
  "Open BOOK attachment.
See `org-open-file' for details about ARG."
  (org-link-open-as-file (ol-library-book--expand book) arg))

;;;###autoload
(defun ol-library-book-open-in-dired ()
  (interactive)
  (dired ol-library-book-dir))

(defun ol-library-book-complete-link ()
  "Advise the user with the available files in the attachment directory."
  (if (file-exists-p ol-library-book-dir)
      (concat "book:" (completing-read "Book: " (ol-library-book--collect)))
    (error "No book directory exist")))

;;; org-link-set-parameters
(org-link-set-parameters "book"
                         :follow #'ol-library-book-follow
                         :complete #'ol-library-book-complete-link)

(provide 'ol-library-book)
