;;;; fanfic2.lisp

(in-package #:fanfic2)

(defun grab (url)
  (parse-html5 (dex:get url) :dom :xmls))

(defun wikipedia-typos ()
  "Load the Wikipedia page and get the list of typos."
  ;; https://github.com/nightfly19/cl-arrows
  ;; Like -<>, but if a form in FORMS has no symbols named <> as top-level element,
  ;; insertion is done like in ->>. Also known as diamond spear.
  (-<>>
    (grab "http://en.wikipedia.org/wiki/Wikipedia:Lists_of_common_misspellings/For_machines")
    (find-tag-recursive "pre")
    (split-sequence #\Newline <> :remove-empty-subseqs t)))

(defun find-tag-recursive (tagname tree)
  "Find the tag recursively in the tree.
NOTE: assumes that the tree is in fact a tree, with no back edges."
  (labels ((ftrsub (tagname tree)
             (let (q)
               (cond
                 ((not tree) nil)
                 ((let ((rt tree))
                    (loop while rt do
                          (let ((node (pop rt)))
                            (if (equal node tagname)
                                (return (list tagname (cadr rt))))
                            (if (listp node) (setf q (append q node)))))))
                    ;; SBCL apparently can't tco this unless you put t for the condition
                (t (ftrsub tagname q))))))
    (cadr (ftrsub tagname tree))))
