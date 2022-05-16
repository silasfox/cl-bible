;;;; cl-bible.asd

(asdf:defsystem #:cl-bible
  :description "A bible study app"
  :author "Silas Vedder <silas@silasvedder.xyz>"
  :license  "MIT"
  :version "0.0.4"
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
