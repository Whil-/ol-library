;;; ol-library-audio-*
(require 'ol-library-core)

(defcustom ol-library-audio-dir "~/audio"
  "The path to be used for audio file links."
  :group 'ol-library
  :type 'directory
  :safe t)

(defcustom ol-library-audio-regexp (concat "\\`[^.].*\\." (regexp-opt (list "mp3" "ogg" "m4b" "wav")) "\\'")
  "Regular expression to match files for `ol-library-audio-store-link'"
  :group 'ol-library
  :type 'regexp
  :safe t)

(defcustom ol-library-audio-to-path-function-list '(ol-library-plain-folder-format
                                                        ol-library-year-folder-format)
  "List of functions parsing a audio string into a folder-path.
The first function in this list defines the preferred function
which will be used when creating new attachment folders. All
functions of this list will be tried when looking for audio files."
  :group 'ol-library
  :type '(repeat (function :tag "Function with filename as input"))
  :safe t)

(defun ol-library-audio--collect ()
  "Create collection of audio-files."
  (ol-library-collect ol-library-audio-dir
                      ol-library-audio-regexp))

(defun ol-library-audio--expand (audio)
  "Expand an audio file into a path.
Based on `ol-library-audio-to-path-function-list' and
`ol-library-audio-dir'."
  (ol-library-dir-from-library-object audio
                                      ol-library-audio-to-path-function-list
                                      ol-library-audio-dir))

(defun ol-library-audio-follow (audio arg)
  "Open AUDIO attachment.
See `org-open-file' for details about ARG."
  (org-link-open-as-file (ol-library-audio--expand audio) arg))

(defun ol-library-audio-complete-link ()
  "Advise the user with the available files in the attachment directory."
  (if (file-exists-p ol-library-audio-dir)
      (concat "audio:" (completing-read "Audio: " (ol-library-audio--collect)))
    (error "No audio directory exist")))

;;; org-link-set-parameters
(org-link-set-parameters "audio"
                         :follow #'ol-library-audio-follow
                         :complete #'ol-library-audio-complete-link)

(provide 'ol-library-audio)
