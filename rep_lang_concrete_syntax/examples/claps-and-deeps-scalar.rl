(let ([foldl
         (fix (lam [foldl]
           (lam [f acc xs]
             (if (null xs)
               acc
               (foldl
                 f
                 (f acc (head xs))
                 (tail xs))))))]
        [claps_scalar 5]
        [deeps_scalar 7]
        [folder
         (lam [acc tup]
           (if (== 0 (fst tup))
               (pair (+ (* claps_scalar (snd tup))
                        (fst acc))
                     (snd acc))
               (if (== 1 (fst tup))
                   (pair (fst acc)
                         (+ (* deeps_scalar (snd tup))
                            (snd acc)))
                   acc)))])
    (lam [vals]
      (let ([res (foldl folder (pair 0 0) vals)])
        (+ (fst res)
           (snd res)))))
