data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add x y) = eval1 x + eval1 y
eval1 (Sub x y) = eval1 x - eval1 y
eval1 (Mul x y) = eval1 x * eval1 y
eval1 (Div x y) = div (eval1 x) (eval1 y)

eval2 :: Expr -> Maybe Int
eval2 (Val n) = Just n
--eval2 (Add e1 e2) = (+) <$> eval2 e1 <*> eval2 e2
eval2 (Add x y) = do
    n1 <- eval2 x
    n2 <- eval2 y
    return (n1+n2)
eval2 (Sub x y) = do
    n1 <- eval2 x
    n2 <- eval2 y
    return (n1-n2)
eval2 (Mul x y) = do
    n1 <- eval2 x
    n2 <- eval2 y
    return (n1*n2)
eval2 (Div x y) = do
    n2 <- eval2 y
    if n2 == 0 then
        Nothing
    else 
        do
            n1 <- eval2 x
            return (div n1 n2)

eval3 :: Expr -> Either String Int
eval3 (Val n) = Right n
eval3 (Add x y) = do
    n1 <- eval3 x
    n2 <- eval3 y
    return (n1+n2)
eval3 (Sub x y) = do
    n1 <- eval3 x
    n2 <- eval3 y
    return (n1-n2)
eval3 (Mul x y) = do
    n1 <- eval3 x
    n2 <- eval3 y
    return (n1*n2)
eval3 (Div x y) = do
    n2 <- eval3 y
    if n2 == 0 then
        Left "div0"
    else
        do
            n1 <- eval3 x
            return (div n1 n2)