;;;; search.lisp

(in-package #:cl-bible.search)

(defun find-in-bible (bible phrase)
  (remove-if-not (lambda (verse) (search phrase (text verse))) bible))

(defun find-book (bible book)
  (remove-if-not (lambda (verse) (search book (bname verse))) bible))

(defun find-chapter (book chapter)
  (remove-if-not (lambda (verse) (string= chapter (chapter verse))) book))
