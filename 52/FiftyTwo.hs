fingerprint :: Int -> Int
fingerprint x
    | x < 10 = 10^x
    | otherwise = fingerprint (mod x 10) + fingerprint (div x 10)

findInteger :: Int
findInteger =
    let multiples x = [n * x | n <- [2..6]];
        sameDigits x y = (fingerprint x) == (fingerprint y) in
    head [x | x <- [1..],
                   all (\q -> q) (zipWith sameDigits
                                          (multiples x)
                                          (tail (multiples x)))]

main :: IO ()
main =
    putStrLn (show findInteger)
