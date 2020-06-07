;;; ol-library-article-*
(require 'ol-library-core)

(defcustom ol-library-article-dir "~/articles"
  "The path to be used for article-links."
  :group 'ol-library
  :type 'directory
  :safe t)

(defcustom ol-library-article-regexp (concat "\\`[^.].*\\." (regexp-opt (list "pdf" "epub" "doc" "docx")) "\\'")
  "Regular expression to match files for `ol-library-article-store-link'"
  :group 'ol-library
  :type 'regexp
  :safe t)

(defcustom ol-library-article-to-path-function-list '(ol-library-plain-folder-format
                                                      ol-library-year-folder-format)
  "List of functions parsing a article string into a folder-path.
The first function in this list defines the preferred function
which will be used when creating new attachment folders. All
functions of this list will be tried when looking for articles."
  :group 'ol-library
  :type '(repeat (function :tag "Function with articlename as input"))
  :safe t)

(defun ol-library-article--collect ()
    "Create collection of articles."
    (ol-library-collect ol-library-article-dir
                        ol-library-article-regexp))

(defun ol-library-article--expand (article)
  "Expand an article into a path.
Based on `ol-library-article-to-path-function-list' and
`ol-library-article-dir'."
  (ol-library-dir-from-library-object article
                                       ol-library-article-to-path-function-list
                                       ol-library-article-dir))

(defun ol-library-article-follow (article arg)
  "Open ARTICLE attachment.
See `org-open-file' for details about ARG."
  (org-link-open-as-file (ol-library-article--expand article) arg))

(defun ol-library-article-complete-link ()
  "Advise the user with the available files in the attachment directory."
  (if (file-exists-p ol-library-article-dir)
      (concat "article:" (completing-read "Article: " (ol-library-article--collect)))
    (error "No article directory exist")))

;;; org-link-set-parameters
(org-link-set-parameters "article"
                         :follow #'ol-library-article-follow
                         :complete #'ol-library-article-complete-link)

(provide 'ol-library-article)
