;;;; util.lisp

(in-package #:fanfic2)

(defmacro filter-match (pattern lst)
  (let ((x (gensym)))
    `(remove-if #'not
                (mapcar (lambda (,x)
                          (trivia:match ,x (,pattern ,x)))
                        ,lst))))

(defun grab (url)
  (parse-html5 (dex:get url) :dom :xmls))

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
