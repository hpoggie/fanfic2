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
  (sort lst (lambda (x y) (> (first x) (first y)))))

(defun worst (lst)
  "Return the description from lst with the highest number of keyword matches."
  (first (sort-descs lst)))

(defun worst-of-page (url)
  (format t "Getting fic list from ~s~%" url)
  (->>
   (grab url)
   (extract-fic-descriptions)
   (mapcar (lambda (x) (cons (count-matches x) x)))
   (worst)))

(defun first-attempt-at-worst ()
  "A first attempt at finding the worst fanfic.
- TODO: get all fics, not just first page"
  (worst
   (mapcar #'worst-of-page (grab-root-directories))))

;; The following links are broken
;; https://www.fanfiction.net/anime/What-do-you-do-at-the-end-of-the-world-Are-you-busy-Will-you-save-us-%E7%B5%82%E6%9C%AB%E3%81%AA%E3%81%AB%E3%81%97%E3%81%A6%E3%81%BE%E3%81%99%E3%81%8B-%E5%BF%99%E3%81%97%E3%81%84%E3%81%A7%E3%81%99%E3%81%8B-%E6%95%91%E3%81%A3%E3%81%A6%E3%82%82%E3%82%89%E3%/
;; https://www.fanfiction.net/tv/Law-Order-Division-of-Field-Investigation-%D0%97%D0%B0%D0%BA%D0%BE%D0%BD-%D0%B8-%D0%BF%D0%BE%D1%80%D1%8F%D0%B4%D0%BE%D0%BA-%D0%BE%D1%82%D0%B4%D0%B5%D0%BB-%D0%BE%D0%BF%D0%B5%D1%80%D0%B0%D1%82%D0%B8%D0%B2%D0%BD%D1%8B%D1%85-%D1%80%D0%B0%D1%81%D1%81%D0%BB%D0%/
;; 2nd one seems to be unlisted on the actual page, but is in the source
