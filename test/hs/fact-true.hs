import Church

fact = y (\fact n -> iszero n one (mult n (fact (dec n))))

seven = Ch $ \f x -> f (f (f (f (f (f (f x))))))
main = print $ isZero $ sub (fact seven) (fact seven)
