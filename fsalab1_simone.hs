
myLength :: [a] -> Int
myLength = foldr (\_ n -> 1 + n) 0

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\a -> (\b -> x==a || b)) False

myOr :: [Bool] -> Bool
myOr = foldr (\b -> (\c -> b || c)) False

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:).f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\a ->  (\xs -> if p a then a : xs else xs)) []

mypp :: [a] -> [a] -> [a]
mypp l1 l2 = foldr (:) l2 l1

myReverse :: [a] -> [a]
myReverse = foldr (\a -> (\xs -> mypp xs [a])) []

myReversel :: [a] -> [a]
myReversel = foldl  (\xs -> (\a -> a : xs)) []

{- the second one is more efficient,
 since the first one uses the mypp function, which uses foldr itself, 
 so this means that it has to folder through a list for every 
 element of the list. The second one only folds once, so it is more efficient

foldl does not work on infinite lists, 
since it starts with applying the function on the last element of the list-}
-------------------------------------------------------------------
luhn :: Integer -> Bool
todigits :: Integer -> [Integer]
doublesecond :: [Integer] -> [Integer]
double :: Integer -> [Integer] -> [Integer]

-- convert the integer to a list with its digits
todigits 0 = []
todigits n = todigits (n `div` 10) ++ [n `mod` 10]

{- this function doubles every second integer in the list 
and sums the integers so that the result remains under 9 -}
doublesecond x = double 1 x
-- a help function for doublesecond
double i [] = []
double i (x:xs) = sum(todigits(x * i)) : double (i `mod` 2 + 1) xs

luhn n = sum (doublesecond (todigits n)) `mod` 10 == 0

isAmericanExpress :: Integer -> Bool
isMaster :: Integer -> Bool
isVisa :: Integer -> Bool

-- american express numbers have length 15 and start with 34 or 37
isAmericanExpress n = luhn n && length (todigits n) == 15 && elem (n `div` 10^13) [34,37]

-- Mastercard numbers have length 16 and start with 5 or a number in the range 2221 and 2720
isMaster n = luhn n && length (todigits n) == 16 && head (todigits n) ==5 || (n `div` 10^12 >= 2221 && n `div` 10^12 <= 2720)

-- Visa numbers start with a 4 and have length 13, 16 or 19
isVisa n = luhn n && elem (length (todigits n)) [13,16,19] && head (todigits n) == 4
-----------------------------------------------------------------
{-Crime Scene Investigation

A group of five school children is caught in a crime. 
One of them has stolen something from some kid they all dislike. 
The headmistress has to find out who did it. 
She questions the children, and this is what they say:

Matthew: Carl didn’t do it, and neither did I.

Peter: It was Matthew or it was Jack.

Jack: Matthew and Peter are both lying.

Arnold: Matthew or Peter is speaking the truth, but not both.

Carl: What Arnold says is not true.

Their class teacher now comes in. 
She says: three of these boys always tell the truth, 
and two always lie. -}

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]



accuses :: Boy -> Boy -> Bool

accusers :: Boy -> [Boy]
guilty, honest :: [Boy]

{-All statements the boys make, can actually be converted into
statements about people they accuse of being a possible thief. 
(e.g. "I did not do it" is the same as: all others might be the thief)
This translate into the following accuses function-}

accuses Matthew x = x /= Carl && x /= Matthew
accuses Peter x = x == Matthew || x == Jack
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
accuses Arnold x = ((accuses Matthew x) || (accuses Peter x)) && not ((accuses Matthew x) && (accuses Peter x))
accuses Carl x = not (accuses Arnold x)

-- now we can make a list of accusers of each boy
accusers x = [y | y <- boys, accuses y x]

{-- since only three boys tell the truth, 
the thief must have exactly those 3 boys as accusers.
This gives us the lists with the guilty one and the honest boys 
(honest boys are the ones accusing the guilty one)-}
guilty = [x | x <- boys, (length (accusers x)) == 3]
honest = [x | x <- boys, all (\y -> accuses x y) guilty]

