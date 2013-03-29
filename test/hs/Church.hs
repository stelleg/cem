{-#Language RankNTypes #-}
module Church where

fix f = f (fix f)

newtype ChurchInt = Ch (forall a.(a->a)->a->a)
type ChurchBool a = a -> a -> a

church n = if n==0 then zero else inc $ church (n-1) 
unchurch (Ch n) = n (\x -> x + 1) 0

instance Num ChurchInt where
  fromInteger = church

zero = Ch (\f x -> x)
one = inc zero

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
iszero :: ChurchInt -> ChurchBool a
iszero (Ch n) = (n (\x -> false) true)

lte :: ChurchInt -> ChurchInt -> ChurchBool a
lte = (\n m -> iszero (sub n m))

isZero n = iszero n True False
