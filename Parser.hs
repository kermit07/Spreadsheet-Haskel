module Parser where
import Data.Char

data Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap f m = m >>= pure . f

instance Applicative Parser where
  pure v = P (\inp -> [(v, inp)])
  f1 <*> f2 = f1 >>= \v1 -> f2 >>= (pure . v1)

instance Monad Parser where
  return = pure
  p >>= f  = P (\inp -> case parse p inp of
                          [] -> []
                          [(v, out)] -> parse (f v) out)

-- podstawowe parsery

item :: Parser Char
item =  P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

failure :: Parser a
failure = P (\inp -> [])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\inp -> case parse p inp of
                       [] -> parse q inp
                       [(v, out)] -> [(v, out)])

-- dalsze prymitywy

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

digit :: Parser Char
digit = sat isDigit

letter :: Parser Char
letter = sat isLetter

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

testString :: String -> Parser String
testString str = token (string str)

type ParserLoc = (Int, Int)

data ParserResult = ParserNum Int
                  | ParserText String
                  | ParserSum [ParserLoc]
                  | ParserMul [ParserLoc]
                  | ParserAvg [ParserLoc]

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

parseNat :: Parser Int
parseNat = do xs <- many1 digit
              return (read xs)

parseInt :: Parser Int
parseInt = do char '-'
              n <- parseNat
              return (-n)
             +++
              parseNat

parseText :: Parser String
parseText = do d <- many1 item
               return d

parseLoc :: Parser ParserLoc
parseLoc = do c <- letter
              d <- many1 digit
              return $ ((charToIntIndex c), (read d :: Int))

parseOperation :: Parser ParserResult
parseOperation = do testString "sum"
                    e <- many1 $ token parseLoc
                    return $ ParserSum e
                   +++ do
                    testString "mul"
                    e <- many1 $ token parseLoc
                    return $ ParserMul e
                   +++ do
                    testString "avg"
                    e <- many1 $ token parseLoc
                    return $ ParserAvg e

myParser :: Parser ParserResult
myParser = do space
              result <- testString "="
              parseOperation
             +++ do
              space
              result <- parseInt
              return $ ParserNum result
             +++ do
              result <- parseText
              return $ ParserText result

charToIntIndex :: Char -> Int
charToIntIndex c | fromEnum c >= 65 &&
                   fromEnum c <= 90 = fromEnum c - 64
                 | fromEnum c >= 97 &&
                   fromEnum c <= 122 = fromEnum c - 96
                 | otherwise = error "Indeksacja od A do Z!"
