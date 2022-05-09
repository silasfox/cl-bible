;;;; cl-bible.lisp

(in-package #:cl-bible.data)

(defun init-bible (&optional (bible "mng"))
  (setf *bible*
        (mapcar #'v:string-to-verse
                (uiop:read-file-lines
                 (asdf:system-relative-pathname
                  "cl-bible"
                  (format nil "resources/~A.tsv" bible))))))

(defun load-bible (filename)
  (v:from-sexp
   (read-from-string
    (uiop:read-file-string (uiop:native-namestring filename)))))

(defvar *mng* (load-bible "~/.bible/menge.sexp"))
(defvar *vul* (load-bible "~/.bible/vulgata.sexp"))
(defvar *grb* (load-bible "~/.bible/greek.sexp"))
(defvar *kjv* (load-bible "~/.bible/kjv.sexp"))
(defvar *bible* *mng*)

(defun load-bibles ()
  (setf *mng* (load-bible "~/.bible/menge.sexp"))
  (setf *vul* (load-bible "~/.bible/vulgata.sexp"))
  (setf *grb* (load-bible "~/.bible/greek.sexp"))
  (setf *kjv* (load-bible "~/.bible/kjv.sexp")))

(defun update-bible (str)
  (let ((bibles `(("mng" . ,*mng*)
                  ("kjv" . ,*kjv*)
                  ("vul" . ,*vul*)
                  ("grb" . ,*grb*))))
  (setf *bible* (cdr (assoc str bibles :test #'string=)))))

(defun persist-bible (bible filename)
  (with-open-file (file (uiop:native-namestring (concatenate 'string "~/.bible/" filename ".sexp"))
                        :direction :output
                        :if-exists :overwrite
                        :if-does-not-exist :create)
    (print (v:to-sexp bible) file)))

(defun persist ()
  (let ((bibles (list *mng* *vul* *grb* *kjv*))
        (files '("menge" "vulgata" "greek" "kjv")))
    (ensure-directories-exist (uiop:native-namestring "~/.bible/"))
    (mapcar #'persist-bible bibles files)))
