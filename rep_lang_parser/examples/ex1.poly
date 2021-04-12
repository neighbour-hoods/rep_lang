(defn sumlist
  (foldl
    +
    0
    (list 1 2 3)))

(defn elem
  (lam [target ls]
    (foldl
      (lam [seenyet x]
        (if (== x target)
            true
            seenyet))
      false
      ls)))

(defn incr
  (lam [x counts]
    (map
      (lam [v]
        (if (== (fst v) x)
            (pair (fst v)
                  (+ 1 (snd v)))
            v))
      counts)))

(incr 2
  (incr 1
    (list
      (pair 1 1)
      (pair 2 2)
      (pair 3 3))))
