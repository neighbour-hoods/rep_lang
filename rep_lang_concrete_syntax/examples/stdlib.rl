(defn map
  (fix (lam [map]
    (lam [f xs]
      (if (null xs)
        xs
        (let ([x_ (f (head xs))])
          (cons
            x_
            (map f (tail xs)))))))))

(defn foldl
  (fix (lam [foldl]
    (lam [f acc xs]
      (if (null xs)
        acc
        (foldl
          f
          (f acc (head xs))
          (tail xs)))))))

(defn take
  (fix (lam [take]
    (lam [n xs]
      (if (== 0 n)
        nil
        (if (null xs)
          xs
          (cons
            (head xs)
            (take (- n 1) (tail xs)))))))))

(defn repeat
  (fix (lam [repeat]
    (let ([cns cons])
      (lam [x]
        (cns x (repeat x)))))))

(defn enumFromTo
  (lam [start end]
    (let ([go
           (fix (lam [go]
             (lam [x]
               (if (== x end)
                 nil
                 (cons x (go (+ x 1)))))))])
      (go start))))

(defn sum
  (lam [xs]
    (foldl + 0 xs)))

(defn ex1
  (sum
    (map (+ 1)
         (take 3 (list 9 8 7 6)))))

(defn ex2 (take 9 (repeat 1)))

(defn ex3 (sum (enumFromTo 0 20)))

ex3
