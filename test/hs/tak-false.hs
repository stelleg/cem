import Church

tak = fix (\tak x y z -> ((((lte x) y) 
          z)
          (((tak (((tak (dec x)) y) z))
                 (((tak (dec y)) z) x))
                 (((tak (dec z)) x) y))))

seven = Ch $ \f x -> f (f (f (f (f (f (f x))))))
fourteen = mult seven $ Ch (\f x -> f (f x))

main = print.isZero $ tak fourteen seven zero
