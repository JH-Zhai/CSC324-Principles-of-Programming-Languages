from i = j : from (i + 1)
    where
      j = seq i i

index k (x:xt) = if k==0
                 then x
                 else index (k-1) xt

fromto i n = if i < n
             then i : fromto (i+1) n
             else []

