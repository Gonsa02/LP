--words::Str->[Str]
main :: IO()
main = do
    input <- getLine
    if input == "*" then
        return ()
    else do
        putStrLn (resultat input)
        main

resultat :: String -> String
resultat linia = nom ++ ": " ++ (valoracio imc)
    where
        imc = calcula_imc (read pes) (read alc)
        [nom, pes, alc] = words linia

calcula_imc :: Float -> Float -> Float
calcula_imc pes alc = pes / (alc*alc)

valoracio :: Float -> String
valoracio x
    | x < 18 = "underweight"
    | x < 25 = "normal weight"
    | x < 30 = "overweight"
    | x < 40 = "obese"
    | otherwise = "severely obese"