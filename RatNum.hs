-- Michael Figueiredo
-- Michael Skinner
-- Christopher Lee

module RatMod ( RatNum,
                ratfrac,
                ratint,
                numerator,
                denominator,
                reduce,
              ) where

ratfrac _ 0 = error "Divide by zero: Undefined"
ratfrac a b = reduce(RatNum a b)

ratint a = RatNum a 1

reduce (RatNum a b) = RatNum (a `div` (gcd a b)) (b `div` (gcd a b))

toDouble (RatNum a b) = (fromInteger a) `div` (fromInteger b)
strToRat str
          | (str == "") = []
          | otherwise   = map read (words str)

data RatNum = RatNum {numerator :: Integer, denominator :: Integer}

instance Num RatNum where

  (+) (RatNum a b) (RatNum c d) = RatNum ((a*d) + (c*b)) (b*d)

	(*) (RatNum a b) (RatNum c d) = RatNum (a*c) (b*d)

	negate (RatNum a b)           = RatNum (a * (-1)) b

	abs (RatNum a b)
		| (a > 0) && (b > 0) = RatNum a b
		| (a > 0) && (b < 0) = RatNum a (b * (-1))
		| (a < 0) && (b > 0) = RatNum (a * (-1)) b
		| (a < 0) && (b < 0) = RatNum (a * (-1)) (b * (-1))
		| (a == 0)           = RatNum 0 1
		| (b == 0)           = error "Infinite Number"

	signum (RatNum a b)
		| ((a > 0) && (b > 0))   = 1
		| ((a > 0) && (b < 0))   = -1
		| ((a < 0) && (b > 0))   = -1
		| ((a < 0) && (b < 0))   = 1
		| ((a == 0) && (b /= 0)) = 0
		| (b == 0)             = error "Infinite Number"

	fromInteger a = RatNum a 1

instance Ord RatNum where

	(<=) (RatNum a b) (RatNum c d)
		| ((a*d) < (c*b))  = True
		| ((a*d) == (c*b)) = True
		| otherwise        = False

instance Eq RatNum where

	(==) (RatNum a b) (RatNum c d)
		| ((a*d) == (c*b)) = True
		| otherwise        = False

instance Show RatNum where
	
	show (RatNum a b) = show a ++ "/" ++ show b
	
