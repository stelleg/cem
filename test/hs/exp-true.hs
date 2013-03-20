{-#OPTIONS_GHC -rtsopts -with-rtsopts=-K1024M #-}
import Church

five = Ch $ \f x -> f (f (f (f (f x))))

main = print $ isZero $ sub (pow five seven) (pow five seven)
