;;;; util.lisp

(in-package #:fanfic2)

(defmacro filter-match (pattern lst)
  (let ((x (gensym)))
    `(remove-if #'not
                (mapcar (lambda (,x)
                          (trivia:match ,x (,pattern ,x)))
                        ,lst))))

(let ((last-time (get-internal-real-time)))
  (defun grab (url)
    "Get the html from the url.
The requests are throttled to avoid violating ffnet ToS: https://www.fanfiction.net/tos/
'You agree not to use or launch any automated system ... that sends more request messages
to the FanFiction.Net servers in a given period of time than a human can reasonably produce
in the same period ...'
A rough measurement of the time it takes me to click on links gave 0.75s/link, so that's what
I'm going with."
    (let* ((delta-time (/ (- (get-internal-real-time) last-time) internal-time-units-per-second))
           (to-sleep (- 0.75 delta-time)))
      (if (> to-sleep 0) (sleep to-sleep))
      (setf last-time (get-internal-real-time))
      (parse-html5 (dex:get url) :dom :xmls))))

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

(defun pmapcar (function list)
  (loop for f in (mapcar (lambda (x) (pexec (funcall function x))) list)
        collect (yield f)))

(defparameter *cache* '())

(defmacro cache->> (&body forms)
  "Cache the result of each form in FORMS"
  (macrolet ((let1 (name val &body forms)
                 `(let ((,name ,val)) ,@forms)))
    (append '(-<>>) (reduce #'append
                            (mapcar (lambda (x) `(,x (let1 foo <> (push foo *cache*) foo)))
                                    forms)))))
