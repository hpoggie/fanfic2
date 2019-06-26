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


(defun test-find-tags ()
  (let ((find-tags-expected (list
                             '("div" (("class" "z-indent z-padtop"))
                               "Hermione needs to go back home quickly at the start of 'Order of the Phoenix', and asks Tonks to take her place that day. Tonks tries to fool Harry that she is the real Hermione, and things quickly escalate from there..."
                               ("div" (("class" "z-padtop2 xgray"))
                                "Harry Potter - Rated: T - English - Adventure/Romance - Chapters: 1 - Words: 4,270 - Reviews: 4 - Favs: 5 - Follows: 5 - Published: "
                                ("span" (("data-xutime" "1561545884")) "4h")
                                " - Harry P., Hermione G., N. Tonks, Daphne G.")))))
    (assert (equalp (find-tags "div" *test-fic-data*) find-tags-expected))))

(defun test-find-tags-recursive ()
  (let ((expected
          '(("style" "min-height:77px;border-bottom:1px #cdcdcd solid;")
            ("style"
              "clear:left;float:left;margin-right:3px;padding:2px;border:1px solid #ccc;-moz-border-radius:2px;-webkit-border-radius:2px;"))))
    (assert (equalp (find-tags-recursive "style" *test-fic-data*) expected))
    (assert (equalp (find-tag-recursive "style" *test-fic-data*) (first expected)))
    (assert (equalp (first (find-tags-recursive "div" *test-fic-data*)) *test-fic-data*))))

(defun test-get-fic-desc ()
  (let ((expected
          "Set after Season One. Mel is still working at the bar when a demon attacks her. Luckily Niko is there. Can she save her before it's too late?"))
    (assert (equalp (get-fic-desc *test-fic-data*) expected))))

(defparameter *test-page* (grab "https://www.fanfiction.net/j/0/0/0/"))
