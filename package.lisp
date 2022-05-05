;;;; package.lisp

(defpackage #:cl-bible.verse
  (:use #:cl)
  (:export verse-to-string
           string-to-verse
           bname
           bsname
           chapter
           vnumber
           text))

(defpackage #:cl-bible.search
  (:use #:cl)
  (:local-nicknames (#:v #:cl-bible.verse))
  (:export find-in-bible
           find-book
           find-chapter))

(defpackage #:cl-bible.lift-search
  (:use #:cl)
  (:local-nicknames (#:s #:cl-bible.search)
                    (#:v #:cl-bible.verse))
  (:export lift-search))

(defpackage #:cl-bible.data
  (:use #:cl)
  (:local-nicknames (#:s #:cl-bible.search)
                    (#:v #:cl-bible.verse))
  (:export update-bible
           *bible*))

(defpackage #:cl-bible.clog
  (:use #:cl #:clog #:clog-gui)
  (:local-nicknames (#:l #:cl-bible.lift-search)
                    (#:d #:cl-bible.data)
                    (#:s #:cl-bible.search)
                    (#:v #:cl-bible.verse))
  (:export on-new-window))

(defpackage #:cl-bible
  (:use #:cl #:clog #:clog-gui)
  (:local-nicknames (#:c #:cl-bible.clog))
  (:export start
           build))
