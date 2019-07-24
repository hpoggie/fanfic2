;;;; page-tree.lisp

(in-package :fanfic2)

(defun match-links (x)
  (remove-if #'not
    (mapcar
        (trivia:lambda-match
            ((list* "div" nil
                    (list "a" (list* (list "href" link) _) _)
                    _)
             link))
        (find-tags-recursive "div" x))))

(defun make-absolute (link)
  (concatenate 'string "https://www.fanfiction.net" link))

(defun grab-root-directories ()
  "Find the urls of all works."
  (format *debug-io* "Grabbing root directories.~%")
  (->>
   (mapcar #'grab
           '("https://www.fanfiction.net/anime/"
             "https://www.fanfiction.net/book/"
             "https://www.fanfiction.net/cartoon/"
             "https://www.fanfiction.net/comic/"
             "https://www.fanfiction.net/game/"
             "https://www.fanfiction.net/misc/"
             "https://www.fanfiction.net/movie/"
             "https://www.fanfiction.net/play/"
             "https://www.fanfiction.net/tv/"))
   (mapcar #'match-links)
   (reduce #'append)
   (remove-if #'not)
   (mapcar #'make-absolute)))

(defun unique (list)
  (reduce (lambda (x y) (if (member y x :test 'equalp)
                            x
                            (append x (list y))))
        list :initial-value '()))

(defun grab-root-crossovers ()
  (format *debug-io* "Grabbing root crossovers.~%")
  (let ((first-layer
          (->>
           (mapcar (lambda (url) (match-links (grab url)))
                   '("https://www.fanfiction.net/crossovers/anime/"
                     "https://www.fanfiction.net/crossovers/book/"
                     "https://www.fanfiction.net/crossovers/cartoon/"
                     "https://www.fanfiction.net/crossovers/comic/"
                     "https://www.fanfiction.net/crossovers/game/"
                     "https://www.fanfiction.net/crossovers/misc/"
                     "https://www.fanfiction.net/crossovers/movie/"
                     "https://www.fanfiction.net/crossovers/play/"
                     "https://www.fanfiction.net/crossovers/tv/"))
           (reduce #'append)
           (mapcar #'make-absolute))))
    (format *debug-io* "Got ~a links in the first layer.~%" (length first-layer))
    (let ((urls (->>
                 ;; Don't save the page in memory. If we save the entirety of every page
                 ;; Lisp will freeze.
                 (mapcar (lambda (url) (handler-case
                                           (match-links (grab url))
                                         ;; Ignore broken links
                                         (dexador.error:http-request-bad-request nil)))
                         first-layer)
                 (reduce #'append)
                 (remove-if #'not))))
      (format *debug-io* "Grabbed root crossovers.~%")
      (setf urls (unique urls))
      (format *debug-io* "Removed duplicates.~%")
      (mapcar #'make-absolute urls))))

(defun num-pages (url)
  (-<>>
   (grab url)
   (find-tags-recursive "a")
   (remove-if-not (lambda (x) (equalp (third x) "Last")))
   (first)
   (trivia:match <> ((list _ (list (list _ url)) _)
                     (->>
                      (cl-strings:split url "=")
                      (last)
                      (first)
                      (parse-integer)))
                    ;; Handle case where there's only one page
                    (* 1))))

(defun pages (url)
  "Find the individual pages for the URL. Returns them in reverse numerical order."
  (let ((max (num-pages url)))
    ;; This may look like gibberish but I swear it makes sense
    ;; ~a means "substitute a thing here", "?&p=" tells ffnet what page to get
    ;; &r=10 means include all ratings
    ;; So it would expand to something like https://www.fanfiction.net/anime/Naruto/?&p=2
    (loop for i from 1 to max collect
                              (format nil "~a?&r=10&p=~a" url i))))
