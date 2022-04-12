(let ([sm_comp
       (lam [st act]
         (if (== st 0)
           (if (== act 1)
             1
             (if (== act 2)
               2
               st))
           (if (== st 1)
             (if (== act 0)
               0
               st)
             (if (== st 2)
               (if (== act 0)
                 0
                 (if (== act 1)
                   3
                   (if (== act 2)
                     3
                     st)))
               (if (== st 3)
                 (if (== act 0)
                   2
                   st)
                 st)))))]
      [sm_init
       0])
  (sm_comp
  (sm_comp
  (sm_comp
  (sm_comp
  (sm_comp
  (sm_comp
  (sm_comp
  (sm_comp
  sm_init
  1)
  1)
  0)
  2)
  2)
  2)
  0)
  0))
