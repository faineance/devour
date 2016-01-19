module Devour

record Parser a where
    constructor Parse
    parse : String -> List (a, String)



Monad Parser where
    p >>= f  = Parse (\cs => concat [parse (f a) cs' | (a,cs') <- parse p cs])

-- MonadZero Parser where
--     zero = Parse (\cs -> [])
Functor Parser where
    map f p = Parse (\cs => [(f v, cs') | (v,cs') <- parse p cs])

Applicative Parser where
    pure v = Parse (\cs => [(v, cs)])



char : Parser Char
char = Parse (\cs => case (unpack cs) of
                []     => []
                (c :: cs) =>  [(c, pack $ cs)])




main : IO ()
main = putStrLn "Hello world"
