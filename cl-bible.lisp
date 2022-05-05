;;;; cl-bible.lisp

(in-package #:cl-bible)

(defun init-bible (&optional (bible "mng"))
  (setf *bible*
        (mapcar #'string->verse
                (uiop:read-file-lines
                 (format nil "resources/~A.tsv" bible)))))

(defvar *bible*)
(defvar *mng* (init-bible))
(defvar *vul* (init-bible "vul"))
(defvar *grb* (init-bible "grb"))
(defvar *kjv* (init-bible "kjv"))

(defun lift-search-window (body search)
  (lambda (obj)
    (declare (ignore obj))
    (let ((result (mapcar #'car (lift-search search)))
          (win (create-gui-window body)))
      (create-p (window-content win)
                :content (str:join ", " result)))))

(defun search-in-bible (phrase canvas)
  (let* ((win (window-content (create-gui-window canvas :title phrase
                                                        :height 400
                                                        :width 650)))
         (lift-search (create-button win :content "Lift Search"))
         (div (create-div win))
         (results (find-in-bible *bible* phrase)))
    (set-on-click lift-search (lift-search-window canvas results))
    (create-p div :content (format nil "~A Ergebnis(se)"
                                   (length results)))
    (mapc (lambda (verse)
            (create-p div :content
                      (verse-to-string verse)))
          results)))

(defun searcher (window)
  (lambda (obj)
    (declare (ignore obj))
    (input-dialog window "What do you want to search?"
                  (lambda (phrase)
                    (search-in-bible phrase window))))) 

(defun update-bible (str)
  (let ((bibles `(("mng" . ,*mng*)
                  ("kjv" . ,*kjv*)
                  ("vul" . ,*vul*)
                  ("grb" . ,*grb*))))
  (setf *bible* (cdr (assoc str bibles :test #'string=)))))

(defun reload (window)
  (lambda (obj)
    (declare (ignore obj))
    (form-dialog window "Which bible do you want?"
                 '(("Bible" "bible" :select (("Menge" "mng")
                                             ("King James Version" "kjv")
                                             ("Vulgata" "vul")
                                             ("Greek Bible" "grb"))))
                 (lambda (results)
                   (update-bible (cadar results))
                 :title "Load a Bible"))))

(defun load-chapter (canvas)
  (lambda (data)
    (let* ((book (cadr (assoc "book" data :test #'string=)))
           (chapter (cadr (assoc "chapter" data :test #'string=)))
           (win (window-content
                 (create-gui-window canvas :title (format nil "~A ~A"
                                                          book
                                                          chapter)
                                           :height 400
                                           :width 650)))
           (div (create-div win)))
      (mapc (lambda (verse)
              (create-p div :content
                        (verse-to-string verse)))
            (find-chapter (find-book *bible* book) chapter)))))

(defun chapter (window body)
  (lambda (obj)
    (declare (ignore obj))
    (form-dialog window "Which bible do you want?"
                 '(("Book" "book" :text "Book")
                   ("Chapter" "chapter" :text "Chapter"))
                 (load-chapter body)
                 :title "Load a Chapter")))

(defun setup-window (body)
  (let ((window (create-gui-window body :title "Search"
                                        :hidden t)))
    (window-normalize window)
    (window-center window)
    window))

(defun setup-menu-bar (body window)
  (let* ((mbar (create-gui-menu-bar body))
         (drop-down (create-gui-menu-drop-down mbar
                                               :content "Options")))
    (create-gui-menu-full-screen mbar)
    (create-gui-menu-item drop-down
                          :content "Search"
                          :on-click (searcher window))
   (create-gui-menu-item drop-down
                          :content "Get Chapter"
                          :on-click (chapter window body))
    (create-gui-menu-item drop-down
                          :content "Load Bible"
                          :on-click (reload window))
    (create-gui-menu-item mbar
                          :content "Close all windows"
                          :on-click (lambda (obj)
                                      (declare (ignore obj))
                                      (loop for win = (current-window body)
                                            unless win do (return) 
                                            do (window-close win))))))


(defun on-new-window (body)
  (setf (title (html-document body)) "Bible")
  (clog-gui-initialize body)
  (setup-menu-bar body (setup-window body)))

(defun start ()
  (initialize #'on-new-window)
  (open-browser))
