module Devour

record Parser a where
    constructor Parse
    parse : String -> List (a, String)


Functor Parser where
    -- return = pure for all monads (since they're all already applicatives)
    map f p = Parse (\cs => [(f v, cs') | (v,cs') <- parse p cs])


Applicative Parser where
    pure v = Parse (\cs => [(v, cs)])
    (Parse p) <*> (Parse p') = Parse (\cs => [(f a, s') | (f, s) <- p cs, (a, s') <- p' s])


Monad Parser where
    p >>= f = Parse (\cs => concat [parse (f a) cs' | (a,cs') <- parse p cs])

Alternative Parser where
    empty = Parse (\cs => [])
    p <|> q = Parse (\cs => case parse p cs of
                            [] => parse q cs
                            r => r)

char : Parser Char
char = Parse (\cs => case (unpack cs) of
                []     => []
                (c :: cs) =>  [(c, pack $ cs)])




main : IO ()
main = putStrLn "Hello world"
