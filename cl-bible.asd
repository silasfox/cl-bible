;;;; cl-bible.asd

(asdf:defsystem #:cl-bible
  :description "Describe cl-bible here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:str #:clog)
  :components ((:file "package")
               (:file "verse")
               (:file "search")
               (:file "lift-search")
               (:file "annotate")
               (:file "data")
               (:file "clog")
               (:file "cl-bible")))
