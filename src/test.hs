import Prelude hiding (last, init)
myN = a `div` length xs
        where
            a = 10
            xs = [1..5]

last xs = drop (length xs - 1) xs
init xs = take (length xs - 1) xs

generateWord :: Int -> String -> [String]
generateWord 0  s = [""]
generateWord n s = [cs ++ [c]| c <- s, cs <- generateWord (n - 1) s]
generateAll :: Int -> String -> [String]
generateAll n s = [x| m <- [0..n], x <- generateWord m s]