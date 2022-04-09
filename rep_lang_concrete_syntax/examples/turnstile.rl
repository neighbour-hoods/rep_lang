(let ([sm_comp
       (lam [st act]
         (if (== st 0)
           (if (== act 0)
             0
             (if (== act 1)
               1
               st))
           (if (== st 1)
             (if (== act 0)
               0
               (if (== act 1)
                 1
                 st))
             st)))]
      [sm_init
       0])
  (sm_comp
  (sm_comp
  (sm_comp
  (sm_comp
  sm_init 0)
  1)
  1)
  0))
