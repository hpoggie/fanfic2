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
    ;; Get the content
    ;; Representation looks like '("name" (("arg1" "val1") ("arg2" "val2") ...) content)
    ;; caddr = first of rest of rest
    (caddr)
    (split-sequence #\Newline <> :remove-empty-subseqs t)
    (mapcar (lambda (x) (first (cl-strings:split x "->"))))))

(defun find-tag-recursive (tagname tree)
  "Find the tag recursively in the tree.
NOTE: assumes that the tree is in fact a tree, with no back edges."
  (first (find-tags-recursive tagname tree)))

(defun find-tags-recursive (tagname tree)
  "Find the tags recursively in the tree.
NOTE: assumes that the tree is in fact a tree, with no back edges."
  ;; 1st and 2nd are the name and attributes, want 3rd and on
  (let ((content (cddr tree)))
    (remove-if #'not
               (concatenate 'list
                            (reduce #'append
                                    (mapcar (lambda (element) (find-tags-recursive tagname element))
                                            (remove-if-not #'listp content)))
                            (remove-if-not (lambda (lst) (and (listp lst) (equalp (first lst) tagname)))
                                           content)))))

(defun find-tags (tagname tree)
  ;; 1st element is the tag, 2nd is args. 3rd is what we want
  (remove-if-not (lambda (lst) (and (listp lst) (equalp (first lst) tagname))) (cddr tree)))

(defun first-cdr-where (lst test)
  (loop for s on lst if (funcall test s) return s))

(defun match-fic-desc (element)
  (first-cdr-where element
                   (trivia:lambda-match
                     ((list* '("div" (("class" "z-indent z-padtop"))) desc _)
                      desc))))

(defun get-fic-desc (element)
  (second (match-fic-desc element)))

(defun extract-fic-descriptions (tree)
  "Take a parsed tree of a page (e.g., 'just in', https://www.fanfiction.net/j/0/0/0/)
and extract the fic descriptions"
  (->>
   (find-tags-recursive "div" tree)
   (mapcar (lambda (x)
             (trivia:match x
               ((list* "div" (list* (list "class" "z-indent z-padtop")) _)
                x))))))
