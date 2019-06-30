(in-package #:fanfic2)

(defparameter *test-fic-data*
  '("div"
    (("class" "z-list zhover zpointer ")
     ("style" "min-height:77px;border-bottom:1px #cdcdcd solid;"))
    ("a" (("class" "stitle") ("href" "/s/13321573/1/Doing-Hermione-a-Favour"))
     ("img"
      (("class" "cimage ")
       ("style"
        "clear:left;float:left;margin-right:3px;padding:2px;border:1px solid #ccc;-moz-border-radius:2px;-webkit-border-radius:2px;")
       ("src" "//ff74.b-cdn.net/static/images/d_60_90.jpg") ("width" "50")
       ("height" "66")))
     "Doing Hermione a Favour")
    "  by " ("a" (("href" "/u/12441042/RagingHero")) "RagingHero") "  "
    ("a" (("class" "reviews") ("href" "/r/13321573/")) "reviews") "
	"
    ("div" (("class" "z-indent z-padtop"))
     "Hermione needs to go back home quickly at the start of 'Order of the Phoenix', and asks Tonks to take her place that day. Tonks tries to fool Harry that she is the real Hermione, and things quickly escalate from there..."
     ("div" (("class" "z-padtop2 xgray"))
      "Harry Potter - Rated: T - English - Adventure/Romance - Chapters: 1 - Words: 4,270 - Reviews: 4 - Favs: 5 - Follows: 5 - Published: "
      ("span" (("data-xutime" "1561545884")) "4h")
      " - Harry P., Hermione G., N. Tonks, Daphne G."))))


(defun test-find-tags-recursive ()
  (let ((expected
          '(("div"
             (("class" "z-list zhover zpointer ")
              ("style" "min-height:77px;border-bottom:1px #cdcdcd solid;"))
             ("a" (("class" "stitle") ("href" "/s/13321573/1/Doing-Hermione-a-Favour"))
              ("img"
               (("class" "cimage ")
                ("style"
                 "clear:left;float:left;margin-right:3px;padding:2px;border:1px solid #ccc;-moz-border-radius:2px;-webkit-border-radius:2px;")
                ("src" "//ff74.b-cdn.net/static/images/d_60_90.jpg") ("width" "50")
                ("height" "66")))
              "Doing Hermione a Favour")
             "  by " ("a" (("href" "/u/12441042/RagingHero")) "RagingHero") "  "
             ("a" (("class" "reviews") ("href" "/r/13321573/")) "reviews") "
	"
             ("div" (("class" "z-indent z-padtop"))
              "Hermione needs to go back home quickly at the start of 'Order of the Phoenix', and asks Tonks to take her place that day. Tonks tries to fool Harry that she is the real Hermione, and things quickly escalate from there..."
              ("div" (("class" "z-padtop2 xgray"))
               "Harry Potter - Rated: T - English - Adventure/Romance - Chapters: 1 - Words: 4,270 - Reviews: 4 - Favs: 5 - Follows: 5 - Published: "
               ("span" (("data-xutime" "1561545884")) "4h")
               " - Harry P., Hermione G., N. Tonks, Daphne G.")))
            ("div" (("class" "z-indent z-padtop"))
             "Hermione needs to go back home quickly at the start of 'Order of the Phoenix', and asks Tonks to take her place that day. Tonks tries to fool Harry that she is the real Hermione, and things quickly escalate from there..."
             ("div" (("class" "z-padtop2 xgray"))
              "Harry Potter - Rated: T - English - Adventure/Romance - Chapters: 1 - Words: 4,270 - Reviews: 4 - Favs: 5 - Follows: 5 - Published: "
              ("span" (("data-xutime" "1561545884")) "4h")
              " - Harry P., Hermione G., N. Tonks, Daphne G."))
            ("div" (("class" "z-padtop2 xgray"))
             "Harry Potter - Rated: T - English - Adventure/Romance - Chapters: 1 - Words: 4,270 - Reviews: 4 - Favs: 5 - Follows: 5 - Published: "
             ("span" (("data-xutime" "1561545884")) "4h")
             " - Harry P., Hermione G., N. Tonks, Daphne G."))))
    (assert (equalp (find-tags-recursive "div" *test-fic-data*) expected))
    (assert (equalp (first (find-tags-recursive "div" *test-fic-data*)) *test-fic-data*))))

(defparameter *test-page* (grab "https://www.fanfiction.net/j/0/0/0/"))

(defun test-count-matches ()
  (let ((test-desc "Link fights efel wiht his sword and sheild"))
    (assert (equalp (count-matches test-desc) 3))))

(defun test-root ()
  (assert (equalp
           '("anime" "book" "cartoon" "comic" "game" "misc" "movie" "play" "tv")
           (remove-duplicates
            (mapcar (lambda (x) (third (split-sequence #\/ x :remove-empty-subseqs t)))
                    (grab-root-directories))
            :test #'equalp))))
