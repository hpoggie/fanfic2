;;;; fanfic2.lisp

(in-package #:fanfic2)

(defun grab (url)
  (parse-html5 (dex:get url) :dom :xmls))

(defun wikipedia-typos ()
  "Load the Wikipedia page and get the list of typos.
We only care about the actual typo'd versions, not the correct ones."
  ;; https://github.com/nightfly19/cl-arrows
  ;; Like -<>, but if a form in FORMS has no symbols named <> as top-level element,
  ;; insertion is done like in ->>. Also known as diamond spear.
  (-<>>
    (grab "http://en.wikipedia.org/wiki/Wikipedia:Lists_of_common_misspellings/For_machines")
    (find-tag-recursive "pre")
    (split-sequence #\Newline <> :remove-empty-subseqs t)
    (mapcar (lambda (x) (first (cl-strings:split x "->"))))))

(defun find-tag-recursive (tagname tree)
  "Find the tag recursively in the tree.
NOTE: assumes that the tree is in fact a tree, with no back edges."
  (first (find-tags-recursive tagname tree :first-only t)))

(defun find-tags-recursive (tagname tree &key first-only)
  "Find the tags recursively in the tree.
NOTE: assumes that the tree is in fact a tree, with no back edges."
  (labels ((ftrsub (tagname trees ret)
            (if (not trees)
                ret
                (let (q (rt trees))
                  (loop while rt do
                        (let ((node (pop rt)))
                          (if (equal node tagname)
                              (let ((body (cadr rt)))
                                (push body ret)
                                (when first-only
                                    (return nil))))
                          (if (listp node) (setf q (append q node)))))
                  (ftrsub tagname q ret)))))
    (ftrsub tagname tree nil)))

(defun find-tags (tagname tree)
  ;; 1st element is the tag, 2nd is args. 3rd is what we want
  (remove-if-not (lambda (lst) (and (listp lst) (equalp (first lst) tagname))) (cddr tree)))

(defun extract-fic-descriptions (tree)
  "Take a parsed tree of a page (e.g., 'just in', https://www.fanfiction.net/j/0/0/0/)
and extract the fic descriptions"
  (->>
   (find-tags-recursive "div" tree)
   (mapcar (lambda (x)
             (trivia:match x
               ((list* "div" (list* (list "class" "z-indent z-padtop")) _)
                x))))))
