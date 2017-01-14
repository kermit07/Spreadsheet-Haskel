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

item  :: Parser Char
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

char2 :: String -> Parser Char
char2 [] = failure
char2 (x:xs) = sat (== x)
              +++
               char2 xs

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

type Loc = (Int, Int)

-- w Parser.hs niestety nie można zaimportować Spreadsheet.hs, bo cykle importów nie są dozwolone
-- dlatego trzeba stworzyć jakąś kolejną strukturę i sprytnie w Main.hs ją przerabiać na typ Cell
data MyResult = ParserNum Int
              | ParserText String
              | ParserSum [Loc]
              | ParserMul [Loc]
              | ParserAvg [Loc]

instance Show MyResult where
  show (ParserNum a) = "NUM " ++ show a
  show (ParserText a) = "TEXT " ++ a
  show (ParserSum x) = "SUM " ++ show x
  show (ParserMul x) = "MUL " ++ show x
  show (ParserAvg x) = "AVG " ++ show x

parseInt :: Parser MyResult
parseInt = do d <- many1 digit
              return $ ParserNum (read d :: Int)

parseText :: Parser MyResult
parseText = do d <- many1 item
               return $ ParserText d

parseLoc :: Parser Loc
parseLoc = do c <- letter
              d <- many1 digit
              return $ ((charToIntIndex c), (read d :: Int))

parseLoc2 :: Parser Loc
parseLoc2 = do char ' '
               c <- letter
               d <- many1 digit
               return $ ((charToIntIndex c), (read d :: Int))

parseSum :: Parser MyResult
parseSum = do char2 "sS"
              char2 "uU"
              char2 "mM"
              e <- many1 parseLoc2
              return $ ParserSum e

parseMul :: Parser MyResult
parseMul = do char2 "mM"
              char2 "uU"
              char2 "lL"
              e <- many1 parseLoc2
              return $ ParserMul e

parseAvg :: Parser MyResult
parseAvg = do char2 "aA"
              char2 "vV"
              char2 "gG"
              e <- many1 parseLoc2
              return $ ParserAvg e

myParser :: Parser MyResult -- tutaj zacząłem robić swój parser. Dla testów na razie
myParser = do parseSum
             +++
              parseMul
             +++
              parseAvg
             +++
              parseInt
             +++
              parseText

charToIntIndex :: Char -> Int
charToIntIndex c | fromEnum c >= 65 &&
                   fromEnum c <= 90 = fromEnum c - 64
                 | fromEnum c >= 97 &&
                   fromEnum c <= 122 = fromEnum c - 96
                 | otherwise = error "Indeksacja od A do Z!"
