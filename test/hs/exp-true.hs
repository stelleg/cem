{-#OPTIONS_GHC -rtsopts -with-rtsopts=-K1024M #-}
import Church

main = print $ isZero (sub (pow seven seven) (pow seven seven))
