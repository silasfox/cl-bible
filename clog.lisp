;;;; cl-bible.lisp

(in-package #:cl-bible.clog)

(defun lift-search-window (body search)
  (lambda (obj)
    (declare (ignore obj))
    (let ((result (mapcar #'car (l:lift-search search)))
          (win (create-gui-window body)))
      (create-p (window-content win)
                :content (str:join ", " result)))))

(defun ergebnis/se (n)
  (format nil "~A Ergebnis~A" n (if (= n 1) "" "se")))

(defun search-in-bible (phrase bible canvas)
  (let* ((win (window-content
               (create-gui-window canvas :title
                                  (format nil "~A: ~A"
                                          d:*translation*
                                          phrase)
                                  :height 400
                                  :width 650)))
         (lift-search (create-button win :content "Lift Search"))
         (div (create-div win))
         (results (s:find-in-bible bible phrase)))
    (set-on-click lift-search (lift-search-window canvas results))
    (create-p div :content (ergebnis/se
                            (length results)))
    (mapc (lambda (verse)
            (v:verse-to-clog verse div :translation d:*translation*))
          results)))

(defun %bible-book-or-chapter (bible book chapter)
  (if (string= book "")
      bible
      (let ((book (s:find-book bible book)))
        (if (string= chapter "")
            book
            (s:find-chapter book chapter)))))

(defun search-with-chapter (window)
  (lambda (data)
    (let ((book (cadr (assoc "book" data :test #'string=)))
          (chapter (cadr (assoc "chapter" data :test #'string=)))
          (phrase (cadr (assoc "phrase" data :test #'string=))))
      (search-in-bible phrase
                       (%bible-book-or-chapter d:*bible* book chapter)
                       window))))

(defun searcher (window)
  (lambda (obj)
    (declare (ignore obj))
    (form-dialog window "What do you want to search?"
                 '(("Phrase" "phrase" :text)
                   ("Book" "book" :text)
                   ("Chapter" "chapter" :text))
                 (search-with-chapter window)
                 :title "Search a phrase")))

(defun reload (window)
  (lambda (obj)
    (declare (ignore obj))
    (form-dialog window "Which bible do you want?"
                 '(("Bible" "bible" :select (("Menge" :mng)
                                             ("King James Version" :kjv)
                                             ("Vulgata" :vul)
                                             ("Greek Bible" :grb)
                                             ("Elberfelder Übersetzung  1871" :elb1871)
                                             ("NEÜ" :neue)
                                             ("Luther 1545" :luth1545)
                                             ("Luther 1912" :luth1912)
                                             ("Schlachter 1951" :sch1951)
                                             ("Ukrainische Version" :ukr))))
                 (lambda (results)
                   (d:update-bible (cadar results))
                   :title "Load a Bible"))))

(defun load-book-or-chapter (canvas)
  (lambda (data)
    (let* ((book (cadr (assoc "book" data :test #'string=)))
           (chapter (cadr (assoc "chapter" data :test #'string=))))
      (if (string= chapter "")
          (load-book canvas book)
          (load-chapter canvas book chapter)))))


(defun load-book (canvas book)
  (let* ((win (window-content
               (create-gui-window canvas :title (format nil "~A: ~A"
                                                        d:*translation*
                                                        book)
                                         :height 400
                                         :width 650)))
         (div (create-div win)))
    (mapc (lambda (verse)
            (v:verse-to-clog verse div :translation d:*translation*))
          (s:find-book d:*bible* book))))

(defun load-chapter (canvas book chapter)
  (let* ((win (window-content
               (create-gui-window canvas :title (format nil "~A: ~A ~A"
                                                        d:*translation*
                                                        book
                                                        chapter)
                                         :height 400
                                         :width 650)))
         (div (create-div win)))
    (mapc (lambda (verse)
            (v:verse-to-clog verse div :translation d:*translation*))
          (s:find-chapter (s:find-book d:*bible* book) chapter))))

(defun get-chapter (window body)
  (lambda (obj)
    (declare (ignore obj))
    (form-dialog window "Which chapter do you want?"
                 '(("Book" "book" :text)
                   ("Chapter" "chapter" :text))
                 (load-book-or-chapter body)
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
                          :on-click (get-chapter window body))
    (create-gui-menu-item drop-down
                          :content "Load Bible"
                          :on-click (reload window))
    (create-gui-menu-item mbar
                          :content "Close all windows"
                          :on-click (lambda (obj)
                                      (declare (ignore obj))
                                      (loop for win = (current-window body)
                                            unless win do (return) 
                                              do (window-close win))))
    (create-gui-menu-item mbar
                          :content "Save notes"
                          :on-click (lambda (obj)
                                      (declare (ignore obj))
                                      (d:persist)))
    (create-gui-menu-item mbar
                          :content "Load notes"
                          :on-click (lambda (obj)
                                      (declare (ignore obj))
                                      (d:load-bibles)))))


(defun on-new-window (body)
  (setf (title (html-document body)) "Bible")
  (clog-gui-initialize body)
  (setup-menu-bar body (setup-window body)))
