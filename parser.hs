-- type Parser = String -> Tree
-- type Parser = String -> (Tree, String)
--
-- module Parsing where
-- module Parsing where
import Data.Char
import Control.Monad
import Debug.Trace

newtype Parser a = P (String -> [(a, String)])

instance MonadPlus Parser where
	mzero        =  P (\_ -> [])
	p `mplus` q  =  P (\inp -> case parse p inp of
							   []        -> parse q inp
							   [(v,out)] -> [(v,out)])

instance Monad Parser where
	return v =  P (\inp -> [(v,inp)])
	par >>= f  =  P (\inp -> case parse par inp of
						   []        -> []
						   [(v,out)] -> parse (f v) out)


-- success function

failure :: Parser a
failure = P (\_ -> [])


item :: Parser Char
item = P (\inp ->
		case inp of
			[] -> []
			(x:xs) -> [(x,xs)])


parse :: Parser a -> String -> [(a,String)]
parse (P pa) inp = pa inp



--or else parser
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q


discardMiddleOfTriple :: Parser (Char, Char)
discardMiddleOfTriple = do
	x <- item
	_ <- item
	y <- item
	return (x,y)

sat :: (Char -> Bool) -> Parser Char
sat p = do
		x <- item
		if p x then return x else failure


digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)


string :: String -> Parser String
string [] = return []
string (x:xs) = do
	char x
	string xs
	return (x:xs)

many :: Parser a -> Parser [a]
many p =  many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p =  do
			v  <- p
			vs <- many p
			return (v:vs)

ident :: Parser String
ident = do
	x <- lower
	xs <- many alphanum
	return (x:xs)

nat :: Parser Int
nat = do
	xs <- many1 digit
	return (read xs)

space :: Parser ()
space = do
	many (sat isSpace)
	return ()


token :: Parser a -> Parser a
token p = do
	space
	v <- p
	space
	return v



identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)


application :: Parser Term
application = do
	_ <- symbol "("
	a <- term
	b <- term
	_ <- symbol ")"
	return (Application (a, b))

lambda ::  Parser Term
lambda = do
	_ <- string "(\\"
	a <- identifier
	_ <- symbol "."
	t <- term
	_ <- symbol ")"
	return (Lambda (a, t))


constant :: Parser Term
constant = do
	i <- nat
	return (Constant i)

variable :: Parser Term
variable = do
	v <- identifier
	return (Variable v)


term ::  Parser Term
term = do
		do
			l <- lambda
			return l
		+++ do
			a <- application
			return a
		+++ do
			v <- variable
			return v
		+++ do
			c <- constant
			return c




-- module Main where
main :: IO ()
main = do
	print $ parse term "(a b)"
	print $ parse term "(\\x . (f x))"
	print $ parse term "(\\x . (\\y . (y (f x))))"
	print $ parse term "(f (b x))"
	print $ parse term "(f (b 5))"





data Term = Constant Consta | Variable Var | Application (Term, Term) | Lambda (Var, Term) deriving Show
type Consta = Int
type Var = String






