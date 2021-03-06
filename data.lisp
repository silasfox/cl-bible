;;;; cl-bible.lisp

(in-package #:cl-bible.data)

(defun load-bible (filename)
  (v:from-sexp
   (read-from-string
    (uiop:read-file-string (uiop:native-namestring filename)))))

(defvar *translation* :mng)
(defvar *bible*)

(defun load-bibles ()
  (ensure-directories-exist (uiop:native-namestring "~/.bible/"))
  (if (uiop:file-exists-p (uiop:native-namestring "~/.bible/bible.sexp"))
      (setf *bible* (load-bible "~/.bible/bible.sexp"))
      (setf *bible*
            (load-bible
             (asdf:system-relative-pathname :cl-bible
                                            "resources/bible.sexp")))))

(load-bibles)

(defun update-bible (translation)
  (setf *translation* (intern translation :keyword)))

(defun persist-bible (bible filename filepath)
  (with-open-file (file (uiop:native-namestring (concatenate 'string filepath filename ".sexp"))
                        :direction :output
                        :if-exists :overwrite
                        :if-does-not-exist :create)
    (print (v:to-sexp bible) file)))

(defun persist (&optional (filepath "~/.bible/"))
  (ensure-directories-exist (uiop:native-namestring filepath))
  (persist-bible *bible* "bible" filepath))

(defvar mapping '(("Genesis" . "Genesis")
                  ("Exodus" . "Exodus")
                  ("Levitikus" . "Levitikus")
                  ("Numeri" . "Numeri")
                  ("Deuteronomium" . "Deuteronomium")
                  ("Josua" . "Josua")
                  ("Richter" . "Richter")
                  ("Rut" . "Ruth")
                  ("1 Samuel" . "1 Samuel")
                  ("2 Samuel" . "2 Samuel")
                  ("1 Könige" . "1 Könige")
                  ("2 Könige" . "2 Könige")
                  ("1 Chronik" . "1 Chronika")
                  ("2 Chronik" . "2 Chronika")
                  ("Esra" . "Esra")
                  ("Nehemia" . "Nehemia")
                  ("Ester" . "Esther")
                  ("Ijob" . "Hiob")
                  ("Psalmen" . "Psalmen")
                  ("Sprüche" . "Sprüche")
                  ("Kohelet" . "Kohelet")
                  ("Hohelied" . "Hohelied")
                  ("Jesaja" . "Jesaja")
                  ("Jeremia" . "Jeremia")
                  ("Klagelieder" . "Klagelieder")
                  ("Ezechiel" . "Hesekiel")
                  ("Daniel" . "Daniel")
                  ("Hosea" . "Hosea")
                  ("Joël" . "Joel")
                  ("Amos" . "Amos")
                  ("Obadja" . "Obadja")
                  ("Jona" . "Jona")
                  ("Micha" . "Micha")
                  ("Nahum" . "Nahum")
                  ("Habakuk" . "Habakuk")
                  ("Zephanja" . "Zephania")
                  ("Haggai" . "Haggai")
                  ("Sacharja" . "Sacharia")
                  ("Maleachi" . "Maleachi")
                  ("Matthäus" . "Matthäus")
                  ("Markus" . "Markus")
                  ("Lukas" . "Lukas")
                  ("Johannes" . "Johannes")
                  ("Apostelgeschichte" . "Apostelgeschichte")
                  ("Römer" . "Römer")
                  ("1 Korinther" . "1 Korinther")
                  ("2 Korinther" . "2 Korinther")
                  ("Galater" . "Galater")
                  ("Epheser" . "Epheser")
                  ("Philipper" . "Philipper")
                  ("Kolosser" . "Kolosser")
                  ("1 Thessalonicher" . "1 Thessalonicher")
                  ("2 Thessalonicher" . "2 Thessalonicher")
                  ("1 Timotheus" . "1 Timotheus")
                  ("2 Timotheus" . "2 Timotheus")
                  ("Titus" . "Titus")
                  ("Philemon" . "Philemon")
                  ("Hebräer" . "Hebräer")
                  ("Jakobus" . "Jakobus")
                  ("1 Petrus" . "1 Petrus")
                  ("2 Petrus" . "2 Petrus")
                  ("1 Johannes" . "1 Johannes")
                  ("2 Johannes" . "2 Johannes")
                  ("3 Johannes" . "3 Johannes")
                  ("Judas" . "Judas")
                  ("Offenbarung" . "Offenbarung")
                  ("Judit" . "Judit")
                  ("Weisheit" . "Weisheit")
                  ("Tobit" . "Tobit")
                  ("Sirach" . "Sirach")
                  ("Baruch" . "Baruch")
                  ("1 Makkabäer" . "1 Makkabäer")
                  ("2 Makkabäer" . "2 Makkabäer")
                  ("xDaniel" . "xDaniel")
                  ("Manasse" . "Manasse")
                  ("xEster" . "xEster")))

(defun add-bible (bible new name mapping)
  (mapc (lambda (map)
          (mapc (lambda (old new)
                  (push (cons name (v:text new))
                        (v::translations old)))
                (cl-bible.search:find-book bible (car map))
                (cl-bible.search:find-book new (cdr map))))
        mapping))
