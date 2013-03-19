{-#Language RankNTypes #-}
module Church where

newtype Church = Ch (forall a.(a->a)->a->a)

church n = if n==0 then zero else inc $ church (n-1) 

zero = Ch (\f x -> x)
one = inc zero

ten = Ch (\f x -> f (f (f (f (f (f (f (f (f (f x))))))))))
seven = Ch (\f x ->  f (f (f (f (f (f (f x)))))))

--Int -> Int
inc (Ch n) = Ch (\f x -> (f (n f x)))
dec (Ch n) = Ch (\f x -> n (\g h -> h (g f)) (\u -> x) (\u -> u))

--Int -> Int -> Int
pow (Ch n) (Ch m) = Ch $ n m
sub n (Ch m) = m dec n
mult (Ch m) (Ch n) = Ch $ (\f -> m (n f))

--Bool
true  = (\x y -> x)
false = (\x y -> y)

--Int -> Bool
iszero :: Church -> a -> a -> a
iszero (Ch n) = (n (\x -> false) true)

isZero n = iszero n True False
