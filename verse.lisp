;;;; search.lisp

(in-package #:cl-bible.verse)

(defclass verse ()
  ((%bname :initarg :bname
           :reader bname)
   (%bsname :initarg :bsname
            :reader bsname)
   (%chapter :initarg :chapter
             :reader chapter)
   (%number :initarg :number
            :reader vnumber)
   (%text :initarg :text
          :reader text)
   (%translations :initarg :translations
                  :initform '()
                  :accessor translations)
   (%notes :initarg :notes
           :initform '()
           :accessor notes)))

(defmethod print-object ((verse verse) stream)
  (format stream (verse-to-string verse :separator " ")))

(defun string-to-verse (string)
  (let ((verse (uiop:split-string string :separator '(#\Tab))))
    (make-instance 'verse :bname (car verse)
                          :bsname (nth 1 verse)
                          :chapter (nth 3 verse)
                          :number (nth 4 verse)
                          :text (nth 5 verse))))

(defgeneric verse-to-string (verse &key separator translation))
(defmethod verse-to-string (verse &key (separator " ") translation)
  (declare (ignore translation))
  (format nil "~A ~A:~A~A~A" (cadr verse)
          (nth 3 verse)
          (nth 4 verse)
          separator
          (nth 5 verse)))

(defmethod verse-to-string ((verse verse)
                            &key (separator "<br/>")
                              (translation :mng))
  (format nil "~A ~A:~A~A ~A" (bsname verse)
          (chapter verse)
          (vnumber verse)
          separator
          (cdr (assoc translation (translations verse)))))

(defgeneric get-text (verse))
(defmethod get-text ((verse verse))
  (cdr (assoc cl-bible.data:*translation* (translations verse))))

(defmethod show-notes ((verse verse) (parent clog:clog-obj))
  (let* ((win (clog-gui:create-gui-window parent
                              :title "Notes"
                              :content (let ((notes (notes verse)))
                                         (if notes
                                             (format nil "~{~A~^<br/>~}" notes)
                                             "No notes found"))))
         (_ (clog:create-br (clog-gui:window-content win)))
         (button (clog:create-button (clog-gui:window-content win)
                                     :content "Add notes")))
    (declare (ignore _))
    (clog:set-on-click button
                       (lambda (obj)
                         (declare (ignore obj))
                         (add-notes verse parent)))))

(defmethod add-notes ((verse verse) (parent clog:clog-obj))
  (let* ((win (clog-gui:window-content
               (clog-gui:create-gui-window parent
                                           :title "Add note")))
         (form (clog:create-form win))
         (text (clog:create-text-area form :rows 4))
         (_ (clog:create-br form))
         (button (clog:create-button form :content "submit")))
    (declare (ignore _))
    (clog:set-on-click button
                       (lambda (obj)
                         (declare (ignore obj))
                         (push (clog:value text) (notes verse))))))

(defgeneric verse-to-clog (verse parent &key translation))
(defmethod verse-to-clog ((verse verse) (parent clog:clog-obj) &key (translation :mng))
  (let* ((verse-string (verse-to-string verse :translation translation))
         (display (clog:create-p parent
                                 :content verse-string)))
    (clog:set-on-click display
                       (lambda (obj)
                         (declare (ignore obj))
                         (show-notes verse parent)))))

(defgeneric to-sexp (verse))
(defmethod to-sexp ((verse verse))
  (list (bname verse)
        (bsname verse)
        (chapter verse)
        (vnumber verse)
        (translations verse)
        (notes verse)))

(defmethod to-sexp ((bible cons))
  (mapcar #'to-sexp bible))

(defun verse-from-sexp (sexp)
  (make-instance 'verse
                 :bname (car sexp)
                 :bsname (cadr sexp)
                 :chapter (caddr sexp)
                 :number (nth 3 sexp)
                 :translations (nth 4 sexp)
                 :notes (nth 6 sexp)))

(defun from-sexp (bible)
  (mapcar #'verse-from-sexp bible))

