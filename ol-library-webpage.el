;;; ol-library-webpage-*
(require 'ol-library-core)

(defcustom ol-library-webpage-dir "~/webpages"
  "The path to be used for webpage-links."
  :group 'ol-library
  :type 'directory
  :safe t)

(defcustom ol-library-webpage-regexp (concat "\\`[^.].*\\."
                                             (regexp-opt (list "html" "pdf" "zip"))
                                             "\\'")
  "Regular expression to match files for `ol-library-webpage-store-link'"
  :group 'ol-library
  :type 'regexp
  :safe t)

(defcustom ol-library-webpage-to-path-function-list '(ol-library-plain-folder-format
                                                      ol-library-year-folder-format)
  "List of functions parsing a webpage string into a folder-path.
The first function in this list defines the preferred function
which will be used when creating new attachment folders. All
functions of this list will be tried when looking for webpages."
  :group 'ol-library
  :type '(repeat (function :tag "Function with webpagename as input"))
  :safe t)

(defun ol-library-webpage--collect ()
  "Create collection of webpages."
  (ol-library-collect ol-library-webpage-dir
                      ol-library-webpage-regexp))

(defun ol-library-webpage--expand (webpage)
  "Expand an webpage into a path.
Based on `ol-library-webpage-to-path-function-list' and
`ol-library-webpage-dir'."
  (ol-library-dir-from-library-object webpage
                                      ol-library-webpage-to-path-function-list
                                      ol-library-webpage-dir))

(defun ol-library-webpage-follow (webpage arg)
  "Open WEBPAGE attachment.
See `org-open-file' for details about ARG."
  (org-link-open-as-file (ol-library-webpage--expand webpage) arg))

(defun ol-library-webpage-complete-link ()
  "Advise the user with the available files in the attachment directory."
  (if (file-exists-p ol-library-webpage-dir)
      (concat "webpage:" (completing-read "Webpage: " (ol-library-webpage--collect)))
    (error "No webpage directory exist")))

;;; org-link-set-parameters
(org-link-set-parameters "webpage"
                         :follow #'ol-library-webpage-follow
                         :complete #'ol-library-webpage-complete-link)

(provide 'ol-library-webpage)
