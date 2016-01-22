module Devour

record Parser a where
    constructor Parse
    parse : String -> List (a, String)


Functor Parser where
    -- return = pure for all monads (since they're all already applicatives)
    map f p = Parse (\cs => [(f v, cs') | (v,cs') <- parse p cs])


Applicative Parser where
    pure v = Parse (\cs => [(v, cs)])
    (Parse a) <*> (Parse b) = Parse (\cs => [(f a, s') | (f, s) <- a cs, (a, s') <- b s])

Monad Parser where
    p >>= f = Parse (\cs => concat [parse (f a) cs' | (a,cs') <- parse p cs])




char : Parser Char
char = Parse (\cs => case (unpack cs) of
                []     => []
                (c :: cs) =>  [(c, pack $ cs)])




main : IO ()
main = putStrLn "Hello world"
