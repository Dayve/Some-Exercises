-- Haskell Exercises (from: http://www.cis.upenn.edu/~cis194/spring13/lectures.html)
-- "Credit card number verification"

-- Ex. 1:
toDigits, toDigitsRev :: Integer -> [Integer]

{- Convert an integer to the reversed list of its digits -}
toDigitsRev number
 | number <= 0 = []
 | otherwise   = number `mod` 10 : toDigitsRev (number `div` 10)

{- Convert an integer to the list of its digits -}
toDigits number
 | number <= 0 = []
 | otherwise   = toDigits (number `div` 10) ++ [number `mod` 10]


-- Ex. 2:
{- Double every other number in a list, going from the end (left side of a list) -}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther givenList = reverse (zipWith (*) (reverse givenList) (cycle [1,2]))


-- Ex. 3:
{- Return the sum the digits of an integer -}
sumDigitsNum :: Integer -> Integer
sumDigitsNum num
 | num < 10  = num
 | otherwise = (num `mod` 10) + sumDigitsNum (num `div` 10)

{- Return the sum the digits of a list of integers -}
sumDigits :: [Integer] -> Integer
sumDigits givenList = sum [sumDigitsNum x | x <- givenList]


-- Ex. 4:
{- Returns true if a card number is valid -}
validate :: Integer -> Bool
validate cardNumber = sumDigits (doubleEveryOther (toDigits cardNumber)) `mod` 10 == 0

