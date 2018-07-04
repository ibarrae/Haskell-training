module HomeworkSix.Fibonacci where

data Stream a 
 = Stream a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 15 . streamToList

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fmap fib [0..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(a,b) -> (b,a+b)) (0,1)

streamToList :: Stream a -> [a]
streamToList (Stream a b) = a : streamToList b

streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a rest) = Stream (f a) (streamMap f rest)

streamSeed :: (a -> a) -> a -> Stream a
streamSeed f a = Stream a (streamSeed f (f a))

nats :: Stream Integer
nats = streamSeed (+ 1) 0

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+ 1) ruler)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a rest) b = Stream a (interleaveStreams b rest)

fibStream :: Stream Integer
fibStream = streamMap head (streamSeed (\[a,b] -> [b,a+b]) [0,1])