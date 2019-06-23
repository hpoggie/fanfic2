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
  (first (find-tags-recursive tagname tree)))

(defun find-tags-recursive (tagname tree)
  "Find the tags recursively in the tree.
NOTE: assumes that the tree is in fact a tree, with no back edges."
  (destructuring-bind (name attributes &rest content) tree
    (remove-if #'not
      (concatenate 'list
                  (mapcar (lambda (element) (find-tags-recursive tagname element))
                          (remove-if-not #'listp content))
                  (remove-if-not (lambda (lst) (and (listp lst) (equalp (first lst) tagname)))
                                 content)))))

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
