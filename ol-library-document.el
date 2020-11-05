;;; ol-library-document-*
(require 'ol-library-core)

(defcustom ol-library-document-dir "~/documents"
  "The path to be used for document-links."
  :group 'ol-library
  :type 'directory
  :safe t)

(defcustom ol-library-document-regexp (concat "\\`[^.].*\\..+\\'")
  "Regular expression to match files for `ol-library-document-store-link'"
  :group 'ol-library
  :type 'regexp
  :safe t)

(defcustom ol-library-document-to-path-function-list '(ol-library-plain-folder-format
                                                       ol-library-year-folder-format)
  "List of functions parsing a document string into a folder-path.
The first function in this list defines the preferred function
which will be used when creating new attachment folders. All
functions of this list will be tried when looking for documents."
  :group 'ol-library
  :type '(repeat (function :tag "Function with documentname as input"))
  :safe t)

(defun ol-library-document--collect ()
    "Create collection of documents."
    (ol-library-collect ol-library-document-dir
                        ol-library-document-regexp))

(defun ol-library-document--expand (document)
  "Expand an document into a path.
Based on `ol-library-document-to-path-function-list' and
`ol-library-document-dir'."
  (ol-library-dir-from-library-object document
                                       ol-library-document-to-path-function-list
                                       ol-library-document-dir))

(defun ol-library-document-follow (document arg)
  "Open DOCUMENT attachment.
See `org-open-file' for details about ARG."
  (org-link-open-as-file (ol-library-document--expand document) arg))

(defun ol-library-document-complete-link ()
  "Advise the user with the available files in the attachment directory."
  (if (file-exists-p ol-library-document-dir)
      (concat "document:" (completing-read "Document: " (ol-library-document--collect)))
    (error "No document directory exist")))

;;; org-link-set-parameters
(org-link-set-parameters "document"
                         :follow #'ol-library-document-follow
                         :complete #'ol-library-document-complete-link)

(provide 'ol-library-document)
