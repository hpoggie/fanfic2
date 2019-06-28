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
   (find-tags-recursive "div")
   (mapcar
    (trivia:lambda-match
        ((list* "div" nil
                (list "a" (list (list "href" link) _) _)
                _)
         link)))
   (remove-if #'not)))
