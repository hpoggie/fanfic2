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
  (first (sort-descs (remove-if #'not lst))))

(defun worst-of-page (url)
  (format t "Getting fic list from ~s~%" url)
  (->>
   (grab url)
   (extract-fic-descriptions)
   (mapcar (lambda (x) (list (count-matches x) x url)))
   (worst)))

(defun grab-and-cache-root-dirs ()
  (let ((rdirs
          (append (grab-root-directories) (grab-root-crossovers))))
    (format *debug-io* "Grabbed ~d root directories~%" (length rdirs))
    (setf rdirs
          ;; The following links are broken
          ;; 2nd one seems to be unlisted on the actual page, but is in the source
          (remove-if (lambda (x) (member x '("https://www.fanfiction.net/anime/What-do-you-do-at-the-end-of-the-world-Are-you-busy-Will-you-save-us-%E7%B5%82%E6%9C%AB%E3%81%AA%E3%81%AB%E3%81%97%E3%81%A6%E3%81%BE%E3%81%99%E3%81%8B-%E5%BF%99%E3%81%97%E3%81%84%E3%81%A7%E3%81%99%E3%81%8B-%E6%95%91%E3%81%A3%E3%81%A6%E3%82%82%E3%82%89%E3%/"
                                             "https://www.fanfiction.net/tv/Law-Order-Division-of-Field-Investigation-%D0%97%D0%B0%D0%BA%D0%BE%D0%BD-%D0%B8-%D0%BF%D0%BE%D1%80%D1%8F%D0%B4%D0%BE%D0%BA-%D0%BE%D1%82%D0%B4%D0%B5%D0%BB-%D0%BE%D0%BF%D0%B5%D1%80%D0%B0%D1%82%D0%B8%D0%B2%D0%BD%D1%8B%D1%85-%D1%80%D0%B0%D1%81%D1%81%D0%BB%D0%/")
                                         :test #'equalp))
                     rdirs))
    (format *debug-io* "~d dirs after filtering broken links~%" (length rdirs))
    ;; Cache this in case later steps fail
    (defparameter *root-dirs* rdirs)
    rdirs))

(defun worst-of-all ()
  "Find the worst fanfic from the entire site."
  (->>
   (grab-and-cache-root-dirs)
   (mapcar (lambda (url)
             (worst (map-pages #'worst-of-page url))))
   (worst)))
