-- Excercise 1: We nede to find the digits of a number

-- |toDigits converts positive Integers to a list of digits.
-- (For 0 or negative inputs, should return an empty list).
toDigits    :: Integer -> [Integer]
toDigits n
  | n <= 0		= []
  | n < 10		= [n]
  | otherwise = toDigits (n `div` 10) ++ toDigits (n `mod` 10)

-- |toDigitsRev should do the same, but with the digits reversed.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
	| n <= 0 		= []
	| n < 10 		= [n]
  | otherwise = toDigitsRev (n `mod` 10) ++ toDigitsRev (n `div` 10)

-- |doubleEveryOtherRev should double every other from the beginning of the list
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []     = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:(y:zs)) = x : y * 2 : doubleEveryOtherRev zs

-- |doubleEveryOther should double every other number _beginning from the right_, that is,
-- the second-to-last, the fourth-to-last, and so forth
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = (reverse . doubleEveryOtherRev . reverse)

sumDigitsInt :: Integer -> Integer
sumDigitsInt n = foldl (+) 0 (toDigits n)

-- |sumDigits calculates the sum of all the digits
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:ys) = (sumDigitsInt x) + (sumDigits ys)

-- |validate indicates whether an Integer could be a valid credit card number
-- 1) Double the value of every second digit beginning from the right
-- 2) Add the digits of the doubled values and undoubled digits up.
-- 3) Calculate the remainder when the sum is divided by 10
-- If the result equals 0, then the number is valid
-- Example: validate 4012888888881881 = True
-- Example: validate 4012888888881882 = False
validate :: Integer -> Bool
validate n = ((sumDigits (doubleEveryOther (toDigits n))) `mod` 10) == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)
