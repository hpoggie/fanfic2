;;;; fanfic2.lisp

(in-package #:fanfic2)

(defun extract-fic-descriptions (tree)
  "Take a parsed tree of a page (e.g., 'just in', https://www.fanfiction.net/j/0/0/0/)
and extract the fic descriptions"
  (->>
   (find-tags-recursive "div" tree)
   (remove-if-not (lambda (x)
                    (and (listp x) (equalp (cadr x) '(("class" "z-indent z-padtop"))))))
   (mapcar #'caddr)))

(defun sort-descs (lst)
  (sort (mapcar (lambda (x) (cons (count-matches x) x)) lst)
        (lambda (x y) (> (first x) (first y)))))

(defun worst (lst)
  "Return the description from lst with the highest number of keyword matches."
  (first (sort-descs lst)))
