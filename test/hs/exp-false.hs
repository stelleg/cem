import Church

ten = Ch $ \f x -> f (f (f (f (f (f (f (f (f (f x)))))))))
seven = Ch $ \f x -> f (f (f (f (f (f (f x))))))
main = print $ isZero (pow (pow seven ten) ten)
