main :: IO()
main = do
    line <- getLine
    if head line == 'A' || head line == 'a' then
        putStrLn("Hello!")
    else
        putStrLn("Bye!")