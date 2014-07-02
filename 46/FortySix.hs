--- Sieve of Eratosthenes ---
sieve :: Integer -> [Integer]
sieve k =
    let primes (x:xs) = x : primes [k | k <- xs, (mod k x) /= 0];
        primes x = x in
    primes [2..k]

notGolbach :: [Integer]
notGolbach =
    let squares x = [k^2 | k <- [1..x], k^2 < x] :: [Integer];
        satisfiesGolbach x sq p = p + (2 * sq) == x;
        composite x = not $ elem x $ sieve x;
        canFindGolbachValuesFor x =
            any (\t -> t)
                [satisfiesGolbach x sq p | sq <- squares x, p <- sieve x] in
    [k | k <- [2..], (odd k) && (composite k) && (not $ canFindGolbachValuesFor k)]

main :: IO ()
main =
  putStrLn $ show $ head notGolbach
