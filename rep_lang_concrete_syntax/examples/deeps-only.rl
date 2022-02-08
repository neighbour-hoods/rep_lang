(let ([foldl
       (fix (lam [foldl]
         (lam [f acc xs]
           (if (null xs)
             acc
             (foldl
               f
               (f acc (head xs))
               (tail xs))))))]
      [folder
       (lam [acc tup]
         (if (== 1 (fst tup))
             (+ (snd tup) acc)
             acc))])
  (lam [vals]
    (foldl folder 0 vals)))
