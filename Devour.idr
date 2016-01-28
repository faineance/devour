module Devour
import Data.String

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


-- many and some are derived automatically
Alternative Parser where
    empty = Parse (\cs => [])
    p <|> q = Parse (\cs => case parse p cs of
                            [] => parse q cs
                            r => r)

item : Parser Char
item = Parse (\cs => case (unpack cs) of
                []     => []
                (c :: cs) =>  [(c, pack $ cs)])


satisfy : (Char -> Bool) -> Parser Char
satisfy pred = do
                x <- item
                if (pred x) then pure x else empty

optional : Parser a -> Parser (Maybe a)
optional p = map Just p <|> pure Nothing

mutual
    many : Parser a -> Parser (List a)
    many p = some p <|> pure []

    some : Parser a -> Parser (List a)
    some p = p >>= \v => many p >>= \vs => return (v :: vs)

oneOf : String -> Parser Char
oneOf s = satisfy (flip elem (unpack s))

char : Char -> Parser Char
char c = satisfy (c ==)

digit : Parser Char
digit = satisfy isDigit

upper : Parser Char
upper = satisfy isUpper

lower : Parser Char
lower = satisfy isLower

letter : Parser Char
letter = satisfy isAlpha

string : String -> Parser String
string s = map pack (traverse char (unpack s))


-- number : Parser Double
-- number = pure parseDouble ((string "-" <|> pure "") ++ some digit)

spaces : Parser ()
spaces = many (satisfy isSpace) *> return ()

padded : Parser a -> Parser a
padded p = spaces *> p <* spaces

surrounding : Char -> Char -> Parser a -> Parser a
surrounding o c p = char o *> p <* char c

parens : Parser a -> Parser a
parens = surrounding '(' ')'

sbrackets : Parser a -> Parser a
sbrackets = surrounding '[' ']'

cbrackets : Parser a -> Parser a
cbrackets = surrounding '{' '}'

squotes : Parser a -> Parser a
squotes = surrounding '\'' '\''

dquotes : Parser a -> Parser a
dquotes = surrounding '\"' '\"'

token : Parser a -> Parser a
token = padded

ident : String -> Parser String
ident s = token (string s)
