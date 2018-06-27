module CreditCardValidation where

isValidNumber :: Integer -> Bool
isValidNumber cardNumber
    | cardNumber <= 0 = False
    | otherwise = sumDigits(doubleEveryOther(toDigitsRev cardNumber))  `mod` 10 == 0

toDigits :: Integer -> [Integer]
toDigits n 
    | n <= 0 = []
    | otherwise = toDigits(n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse(toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:(y:z)) = x : (y * 2) : doubleEveryOther z

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum(toDigits x)
sumDigits (x:y) = sumDigits(toDigits x) + sumDigits y 