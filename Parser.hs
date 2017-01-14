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

char :: Char -> Parser Char
char x = sat (== x)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many p
             return (v:vs)

-- w Parser.hs niestety nie można zaimportować Spreadsheet.hs, bo cykle importów nie są dozwolone
-- dlatego trzeba stworzyć jakąś kolejną strukturę i sprytnie w Main.hs ją przerabiać na typ Cell
data MyResult = Num Int
              | Text String

instance Show MyResult where
  show (Num a) = show a
  show (Text a) = a

myParser :: Parser MyResult -- tutaj zacząłem robić swój parser. Dla testów na razie
myParser = do d <- many1 digit
              return $ Num (read d :: Int)
             +++
           do char '('
              e <- digit
              char ')'
              return $ Num (digitToInt e)
