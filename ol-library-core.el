;;; ol-library.el - Support for links to books in Org mode
;;
;; Gustav Wikström
;; Copyright (C) 2020
;;
;; Author: Gustav Wikström
;;
;; This file is an extension to GNU Emacs Org mode.
;;
;;; Commentary:
;;
;; Installation and Activation
;; ---------------------------
;;
;; ...
;;
;; Documentation
;; -------------
;;
;; ...
;;
;;; Code:

(require 'ol)

(defun ol-library-plain-folder-format (library-object)
  "Translate a library-object to a basic folder-path."
  (format "%s" library-object))

(defun ol-library-year-folder-format (book)
  "Translate a library-object to a folder-path based on year.
Assumes the first four characters in the library-object name
represents a folder in which the object resides."
  (format "%s/%s"
	  (substring library-object 0 4)
	  library-object))

(defun ol-library-cleanup-remove-tags-and-suffix (link)
  "Remove tags and tag delimiter in string.
As described by Karl Voit in his tagtree setup. I.e. ' -- tag1
tag2 etc.suffix' will be removed."
  (let ((name (file-name-sans-extension link)))
    (if (string-match "^\\(.*?\\) -- .+\\'" name)
        (match-string 1 name)
      name)))

(defun ol-library-fixup-complete-filename-from-path (path)
  "Try to complete a path to get a correct filename.
Assumes PATH is missing a suffix for it to be a valid filename.
By ol-library convention, there should not be more than one file
that matches path in order for it to be completed."
  (let* ((directory (file-name-directory path))
         (file-prefix (file-name-nondirectory path))
         (file-name-candidate (file-name-completion file-prefix directory)))
    (if file-name-candidate
        (let ((fullpath (expand-file-name file-name-candidate directory)))
          (cond
           ((file-exists-p fullpath)
            fullpath)
           (t (expand-file-name
               (completing-read "Which file?"
                                (file-name-all-completions
                                 file-name-candidate directory)
                                nil t)
               directory))))
      path)))

(defun ol-library-collect (library-dir library-filename-regexp &optional link-cleanup-function)
  "Create collection of library files.

Given LIBRARY-DIR which is a path in the filesystem, collect
files matching LIBRARY-FILENAME-REGEXP recursively and clean the
names with an optional function LINK-CLEANUP-FUNCTION, which
expects a string as input and shall return a string as output as
well."
  (if (file-exists-p library-dir)
      (let ((objects (mapcar #'file-name-nondirectory
                           (directory-files-recursively library-dir library-filename-regexp))))
        (if (functionp link-cleanup-function)
            (mapcar link-cleanup-function
                    objects)
          objects))
    (error "Cannot find `library-dir': %s" library-dir)))

(defun ol-library-dir-from-library-object (library-object library-object-to-path-function-list library-dir &optional link-fixup-function)
  "Returns a folder path based on library object.
Uses LIBRARY-DIR, LIBRARY-OBJECT and a
LIBRARY-OBJECT-TO-PATH-FUNCTION-LIST to return a directory DIR
where the supposed LIBRARY-OBJECT resides in the filesystem. If
no path exist in filesystem, return last non-matching path .

Optional LINK-FIXUP-FUNCTION can be used to aid this function in
providing a correct path to the LIBRARY-OBJECT."
  (let ((fun-list library-object-to-path-function-list)
        path)
    (while (or (not path) (and (not (file-exists-p path)) fun-list))
      (setq path (expand-file-name
                  (funcall (car fun-list) library-object)
                  (expand-file-name library-dir)))
      (when (functionp link-fixup-function)
        (setq path (funcall link-fixup-function path)))
      (setq fun-list (cdr fun-list)))
    (if (file-exists-p path)
        path
      (error "Cannot find file at any locations: %s" (file-name-nondirectory path)))))


(provide 'ol-library-core)
