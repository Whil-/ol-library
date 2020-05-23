;;; ol-library.el - Support for links to books in Org mode
(require 'ol)

;;; ol-library-*
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

(defun ol-library-collect (library-dir library-filename-regexp)
    "Create collection of library files."
    (if (file-exists-p library-dir)
        (mapcar #'file-name-nondirectory
         (directory-files-recursively library-dir library-filename-regexp))
      (error "Cannot find `library-dir': %s" library-dir)))

(defun ol-library-dir-from-library-object (library-object library-object-to-path-function-list library-dir)
  "Returns a folder path based on library object.
Uses LIBRARY-DIR, LIBRARY-OBJECT and a
LIBRARY-OBJECT-TO-PATH-FUNCTION-LIST to return a directory DIR
where the supposed LIBRARY-OBJECT resides in the filesystem. If
no path exist in filesystem, path according to first function in
function list is returned."
  (let ((path (expand-file-name
               (funcall (car library-object-to-path-function-list) library-object)
               (expand-file-name library-dir)))
        (fun-list (cdr library-object-to-path-function-list)))
    (while (and fun-list (not (file-exists-p path)))
      (setq path (expand-file-name
                  (funcall (car fun-list) library-object)
                  (expand-file-name library-dir)))
      (setq fun-list (cdr fun-list)))
    (if (file-exists-p path)
        path
      path-preferred)))


(provide 'ol-library)
