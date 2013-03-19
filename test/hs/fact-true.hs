import Church
import Y

fact :: Church -> Church 
fact = y (\fact n -> iszero n one (mult n (fact (dec n))))

main = print $ isZero $ sub (fact seven) (fact seven)
