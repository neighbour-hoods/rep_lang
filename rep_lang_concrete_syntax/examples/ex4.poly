(defn reverse
  (lam [ls]
    (let ([f (lam [acc x] (cons x acc))])
      (foldl f nil ls))))

(reverse (list 1 2 3 4))
