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
    (find-tags-recursive "pre")
    ;; Get the content
    ;; Representation looks like '(("name" (("arg1" "val1") ("arg2" "val2") ...) content))
    ;; (because there's only one <pre> tag)
    ;; caddar = first of rest of rest of first
    (caddar)
    (split-sequence #\Newline <> :remove-empty-subseqs t)
    (mapcar (lambda (x) (first (cl-strings:split x "->"))))))

(defparameter *keywords* (wikipedia-typos))

(defun find-tags-recursive (tagname tree)
  "Find the tags recursively in the tree.
NOTE: assumes that the tree is in fact a tree, with no back edges."
  ;; 1st and 2nd are the name and attributes, want 3rd and on
  (->>
    (destructuring-bind (name attributes &rest content) tree
        (declare (ignore attributes))
        (concatenate 'list
                    (list (if (equalp name tagname) tree))
                    (reduce #'append
                            (mapcar (lambda (element) (find-tags-recursive tagname element))
                                    (filter-match (list* _ _) content)))))
    (remove-if #'not)))

(defun extract-fic-descriptions (tree)
  "Take a parsed tree of a page (e.g., 'just in', https://www.fanfiction.net/j/0/0/0/)
and extract the fic descriptions"
  (->>
   (find-tags-recursive "div" tree)
   (remove-if-not (lambda (x)
                    (and (listp x) (equalp (cadr x) '(("class" "z-indent z-padtop"))))))
   (mapcar #'caddr)))

(defun count-matches (str)
  "Take a fanfic description and count the number of keyword matches."
  (->>
   (split-sequence #\Space str)
   (remove-if-not (lambda (x) (member-if (lambda (y) (equal y x)) *keywords*)))
   (length)))

(defun sort-descs (lst)
  (sort (mapcar (lambda (x) (cons (count-matches x) x)) lst)
        (lambda (x y) (> (first x) (first y)))))

(defun worst (lst)
  "Return the description from lst with the highest number of keyword matches."
  (first (sort-descs lst)))
