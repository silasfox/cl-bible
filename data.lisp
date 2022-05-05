;;;; cl-bible.lisp

(in-package #:cl-bible.data)

(defun init-bible (&optional (bible "mng"))
  (setf *bible*
        (mapcar #'v:string-to-verse
                (uiop:read-file-lines
                 (asdf:system-relative-pathname
                  "cl-bible"
                  (format nil "resources/~A.tsv" bible))))))

(defvar *bible*)
(defvar *mng* (init-bible))
(defvar *vul* (init-bible "vul"))
(defvar *grb* (init-bible "grb"))
(defvar *kjv* (init-bible "kjv"))

(defun update-bible (str)
  (let ((bibles `(("mng" . ,*mng*)
                  ("kjv" . ,*kjv*)
                  ("vul" . ,*vul*)
                  ("grb" . ,*grb*))))
  (setf *bible* (cdr (assoc str bibles :test #'string=)))))
