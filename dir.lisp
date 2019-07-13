;;;; page-tree.lisp

(in-package :fanfic2)

(defun grab-root-directories ()
  "Find the urls of all works."
  (->>
   (mapcar #'grab
           '("https://www.fanfiction.net/anime/"
             "https://www.fanfiction.net/book/"
             "https://www.fanfiction.net/cartoon/"
             "https://www.fanfiction.net/comic/"
             "https://www.fanfiction.net/game/"
             "https://www.fanfiction.net/misc/"
             "https://www.fanfiction.net/movie/"
             "https://www.fanfiction.net/play/"
             "https://www.fanfiction.net/tv/"))
   (mapcar (lambda (x)
            (mapcar
                (trivia:lambda-match
                    ((list* "div" nil
                            (list "a" (list (list "href" link) _) _)
                            _)
                     link))
                (find-tags-recursive "div" x))))
   (reduce #'append)
   (remove-if #'not)
   (mapcar (lambda (x)
             (concatenate 'string "https://www.fanfiction.net" x)))))

(defun num-pages (url)
  (-<>>
   (grab url)
   (find-tags-recursive "a")
   (remove-if-not (lambda (x) (equalp (third x) "Last")))
   (first)
   (trivia:match <> ((list _ (list (list _ url)) _) url))
   (cl-strings:split <> "=")
   (last)
   (first)
   (parse-integer)))

(defun pages (url)
  "Find the individual pages for the URL. Returns them in reverse numerical order."
  (let ((max (num-pages url)))
    ;; This may look like gibberish but I swear it makes sense
    ;; ~a means "substitute a thing here", "?&p=" tells ffnet what page to get
    ;; &r=10 means include all ratings
    ;; So it would expand to something like https://www.fanfiction.net/anime/Naruto/?&p=2
    (loop for i from 1 to max collect
                              (format nil "~a?&r=10&p=~a" url i))))
