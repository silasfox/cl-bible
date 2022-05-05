;;;; annotate.lisp

(in-package #:cl-bible)

(defstruct metadata
  notes
  tags)

(defun add-metadata (verse metadata)
  (cons verse metadata))

(defun get-notes (verse)
  (metadata-notes (cdr verse)))

(defun get-tags (verse)
  (metadata-tags (cdr verse)))

(defun add-note (verse note)
  (setf (metadata-notes (cdr verse)) (cons note (get-notes verse))))

(defun add-tag (verse tag)
  (setf (metadata-tags (cdr verse)) (cons tag (get-tags verse))))
