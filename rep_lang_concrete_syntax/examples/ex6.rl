(defn foo
  (lam [x]
    (let ([a true]
          [b false]
          [c true]
          [d false])
      (+ 7 x))))

(foo
  (if (== (+ 1 2) (- 4 3))
    (if (== 1 1) 9 3)
    (if (== 2 3) 4 7)))
