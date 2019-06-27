(defmacro filter-match (pattern lst)
  (let ((x (gensym)))
    `(remove-if #'not
                (mapcar (lambda (,x)
                          (trivia:match ,x (,pattern ,x)))
                        ,lst))))
