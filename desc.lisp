;;;; desc.lisp

(in-package #:fanfic2)

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

(defun count-matches (str)
  "Take a fanfic description and count the number of keyword matches."
  (->>
   (split-sequence #\Space str)
   (remove-if-not (lambda (x) (member-if (lambda (y) (equal y x)) *keywords*)))
   (length)))
