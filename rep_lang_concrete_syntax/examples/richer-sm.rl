(let ([sm_comp
       (lam [st act]
         (if (== (fst st) 0)
           (if (== act 1)
             (pair 1 1)
             (if (== act 2)
               (pair 1 -1)
               st))
           (if (== (fst st) 1)
             (if (== act 1)
               (let ([count (+ 1 (snd st))])
                 (if (> count 5)
                   (pair 2 0)
                   (pair (fst st) count)))
               (if (== act 2)
                 (let ([count (- (snd st) 1)])
                   (if (< count -5)
                     (pair 3 0)
                     (pair (fst st) count)))
                 st))
             st)))]
      [sm_init
       (pair 0 0)])
  (sm_comp
  (sm_comp
  (sm_comp
  (sm_comp
  (sm_comp
  (sm_comp
  (sm_comp
  (sm_comp
  sm_init
  2)
  2)
  2)
  2)
  2)
  2)
  2)
  1))
