(in-package #:fanfic2)

(defparameter *test-fic-data*
   '("div"
      (("class" "z-list zhover zpointer "))
      ("style" "min-height:77px;border-bottom:1px #cdcdcd solid;")
      ("a" (("class" "stitle") ("href" "/s/13318460/1/The-Fight-for-Mel")))
      ("img"
        (("class" "lazy cimage "))
        ("style"
          "clear:left;float:left;margin-right:3px;padding:2px;border:1px solid #ccc;-moz-border-radius:2px;-webkit-border-radius:2px;")
        ("src" "//ff74.b-cdn.net/static/images/d_60_90.jpg")
        ("data-original" "//ff74.b-cdn.net/image/4744819/75/") ("width" "50")
        ("height" "66"))
      "The Fight for Mel"
      "  by " ("a" (("href" "/u/8045772/StormyWriter55")) "StormyWriter55") "
    "
      ("div" (("class" "z-indent z-padtop")))
      "Set after Season One. Mel is still working at the bar when a demon attacks her. Luckily Niko is there. Can she save her before it's too late?"
      ("div" (("class" "z-padtop2 xgray"))
        "Charmed, 2018 - Rated: M - English - Hurt/Comfort - Chapters: 1 - Words: 3,375 - Published: "
        ("span" (("data-xutime" "1561231243")) "3m") " - Mel V., Niko H."))
    "
  ")

(defun test-find-tags ()
  (let ((expected (list
                   '("div" (("class" "z-indent z-padtop")))
                   '("div" (("class" "z-padtop2 xgray"))
                      "Charmed, 2018 - Rated: M - English - Hurt/Comfort - Chapters: 1 - Words: 3,375 - Published: "
                      ("span" (("data-xutime" "1561231243")) "3m") " - Mel V., Niko H."))))
    (assert (equalp (find-tags "div" *test-fic-data*) expected))))
