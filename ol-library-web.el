;;; ol-library-web-*
(require 'ol-library)

(defcustom ol-library-web-dir "~/webs"
  "The path to be used for web-links."
  :group 'ol-library
  :type 'directory
  :safe t)

(defcustom ol-library-web-regexp (concat "\\`[^.].*\\."
                                         (regexp-opt (list "html" "pdf" "zip"))
                                         "\\'")
  "Regular expression to match files for `ol-library-web-store-link'"
  :group 'ol-library
  :type 'regexp
  :safe t)

(defcustom ol-library-web-to-path-function-list '(ol-library-plain-folder-format
                                                   ol-library-year-folder-format)
  "List of functions parsing a web string into a folder-path.
The first function in this list defines the preferred function
which will be used when creating new attachment folders. All
functions of this list will be tried when looking for webs."
  :group 'ol-library
  :type '(repeat (function :tag "Function with webname as input"))
  :safe t)

(defun ol-library-web--collect ()
    "Create collection of webs."
    (ol-library-collect ol-library-web-dir
                        ol-library-web-regexp))

(defun ol-library-web--expand (web)
  "Expand an web into a path.
Based on `ol-library-web-to-path-function-list' and
`ol-library-web-dir'."
  (ol-library-dir-from-library-object web
                                       ol-library-web-to-path-function-list
                                       ol-library-web-dir))

(defun ol-library-web-follow (web arg)
  "Open WEB attachment.
See `org-open-file' for details about ARG."
  (org-link-open-as-file (ol-library-web--expand web) arg))

(defun ol-library-web-complete-link ()
  "Advise the user with the available files in the attachment directory."
  (if (file-exists-p ol-library-web-dir)
      (concat "web:" (completing-read "Web: " (ol-library-web--collect)))
    (error "No web directory exist")))

;;; org-link-set-parameters
(org-link-set-parameters "web"
                         :follow #'ol-library-web-follow
                         :complete #'ol-library-web-complete-link)

(provide 'ol-library-web)
