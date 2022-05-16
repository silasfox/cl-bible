;;;; lift-search.lisp

(in-package #:cl-bible.lift-search)

(defun frequent-words () (str:words "der die das dir mir wir ihr sie sein mein dein euer unser dem den in zu und"))

(defun lift-search (search-result)
  (comb (diff-verses (mapcar #'v:get-text search-result))
        (length search-result)))

(defun count-words (words)
  (let (result)
    (mapc (lambda (word)
            (if (assoc word result :test #'string=)
                (incf (cdr (assoc word result :test #'string=)))
                (setf result (acons word 1 result))))
          words)
    (sort result (lambda (x y)
                   (> (cdr x) (cdr y))))))

(defun diff-verses (verses)
  (count-words (remove-if (lambda (word) (member word
                                                 (frequent-words)
                                                 :test #'string=))
                          (mapcan #'str:words verses))))

(defun comb (freqs length)
  (remove-if (lambda (word)
               (< (cdr word) (* 0.25 length)))
             freqs))
