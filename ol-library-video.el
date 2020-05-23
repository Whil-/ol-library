;;; ol-library-video-*
(require 'ol-library)

(defcustom ol-library-video-dir "~/videos"
  "The path to be used for video-links."
  :group 'ol-library
  :type 'directory)

(defcustom ol-library-video-regexp (concat "\\`[^.].*\\." (regexp-opt (list "mkv" "mp4" "mov" "webm" "avi" "mpeg" "mpg")) "\\'")
  "Regular expression to match files for `ol-library-video-store-link'"
  :group 'ol-library
  :type 'regexp)

(defcustom ol-library-video-to-path-function-list '(ol-library-plain-folder-format
                                                   ol-library-year-folder-format)
  "List of functions parsing a video string into a folder-path.
The first function in this list defines the preferred function
which will be used when creating new attachment folders. All
functions of this list will be tried when looking for videos."
  :group 'ol-library
  :type '(repeat (function :tag "Function with videoname as input")))

(defun ol-library-video--collect ()
    "Create collection of videos."
    (ol-library-collect ol-library-video-dir
                        ol-library-video-regexp))

(defun ol-library-video--expand (video)
  "Expand an video into a path.
Based on `ol-library-video-to-path-function-list' and
`ol-library-video-dir'."
  (ol-library-dir-from-library-object video
                                       ol-library-video-to-path-function-list
                                       ol-library-video-dir))

(defun ol-library-video-follow (video arg)
  "Open VIDEO attachment.
See `org-open-file' for details about ARG."
  (org-link-open-as-file (ol-library-video--expand video) arg))

(defun ol-library-video-complete-link ()
  "Advise the user with the available files in the attachment directory."
  (if (file-exists-p ol-library-video-dir)
      (concat "video:" (completing-read "Video: " (ol-library-video--collect)))
    (error "No video directory exist")))

;;; org-link-set-parameters
(org-link-set-parameters "video"
                         :follow #'ol-library-video-follow
                         :complete #'ol-library-video-complete-link)

(provide 'ol-library-video)
