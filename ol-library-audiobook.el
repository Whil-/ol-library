;;; ol-library-audiobook-*
(require 'ol-library-core)

(defcustom ol-library-audiobook-dir "~/audiobooks"
  "The path to be used for audiobook-links."
  :group 'ol-library
  :type 'directory
  :safe t)

(defcustom ol-library-audiobook-regexp (concat "\\`[^.].*\\." (regexp-opt (list "mp3" "ogg" "m4b" "wav")) "\\'")
  "Regular expression to match files for `ol-library-audiobook-store-link'"
  :group 'ol-library
  :type 'regexp
  :safe t)

(defcustom ol-library-audiobook-to-path-function-list '(ol-library-plain-folder-format
                                                        ol-library-year-folder-format)
  "List of functions parsing a audiobook string into a folder-path.
The first function in this list defines the preferred function
which will be used when creating new attachment folders. All
functions of this list will be tried when looking for audiobooks."
  :group 'ol-library
  :type '(repeat (function :tag "Function with audiobookname as input"))
  :safe t)

(defun ol-library-audiobook--collect ()
  "Create collection of audiobooks."
  (ol-library-collect ol-library-audiobook-dir
                      ol-library-audiobook-regexp))

(defun ol-library-audiobook--expand (audiobook)
  "Expand an audiobook into a path.
Based on `ol-library-audiobook-to-path-function-list' and
`ol-library-audiobook-dir'."
  (ol-library-dir-from-library-object audiobook
                                      ol-library-audiobook-to-path-function-list
                                      ol-library-audiobook-dir))

(defun ol-library-audiobook-follow (audiobook arg)
  "Open AUDIOBOOK attachment.
See `org-open-file' for details about ARG."
  (org-link-open-as-file (ol-library-audiobook--expand audiobook) arg))

(defun ol-library-audiobook-complete-link ()
  "Advise the user with the available files in the attachment directory."
  (if (file-exists-p ol-library-audiobook-dir)
      (concat "audiobook:" (completing-read "Audiobook: " (ol-library-audiobook--collect)))
    (error "No audiobook directory exist")))

;;; org-link-set-parameters
(org-link-set-parameters "audiobook"
                         :follow #'ol-library-audiobook-follow
                         :complete #'ol-library-audiobook-complete-link)

(provide 'ol-library-audiobook)
