{-# LANGUAGE DeriveGeneric #-}
module Spreadsheet where
import Data.Function
import Data.Maybe
import Control.Applicative
import Data.Binary
import GHC.Generics (Generic)
import System.IO
import qualified Data.ByteString.Lazy as DBSL

-- lokalizacja jest definiowana za pomocą dwóch współrzędnych
type Location = (Int, Int)

-- przyjąłem, że komórka ma następujące postaci
data Cell = Empty
          | Num Int
          | Text String
          | Add [Location] (Maybe Float) -- [Location] określa lokalizacje sumowanych komórek a (Maybe Float) przechowuje wynik
          | Mul [Location] (Maybe Float) -- jw
          | Avg [Location] (Maybe Float) -- jw
           deriving (Generic)

instance Show Cell where
  show (Empty) = "_"
  show (Num a) = show a
  show (Text a) = a
  show (Add _ (Just x)) = show x
  show (Add _ Nothing) = "SUM ERR"
  show (Mul _ (Just x)) = show x
  show (Mul _ Nothing) = "MUL ERR"
  show (Avg _ (Just x)) = show x
  show (Avg _ Nothing) = "AVG ERR"

-- arkusz jest dwuwymiarową tablicą komórek
type Spreadsheet = [[Cell]] 

--metoda serializująca obiekt do podanego w nazwie pliku
saveSpreadsheet :: Spreadsheet -> [Char] -> IO ()
saveSpreadsheet s filename = DBSL.writeFile filename (encode s)
                                 
serPutList :: [Int] -> Put 
serPutList [] = []
serPutList (x:xs) = do 
		  put x: serPutList xs
 
instance Binary Cell where
    put (Text a) = do put ('t' :: Char)
                           put a

    put (Num a)  = do put ('n' :: Char)
                           put a

    put (Add b s) = do put ('a' :: Char)
                           put s
			   serPutList ((>>= \(a,b) -> [Left a,Right b]) b)

    put (Mul b s) = do put ('m' :: Char)
                           put s
			   serPutList ((>>= \(a,b) -> [Left a,Right b]) b)

    put (Avg b s) = do put ('v' :: Char)
                           put s
			   serPutList ((>>= \(a,b) -> [Left a,Right b]) b)

--TODO: gettery   

--TODO: instancja dla spreadsheet
-- instance Binary Spreadsheet where


-- przykładowy arkusz
spreadsheet :: Spreadsheet
spreadsheet = [[Num 2, Text "abc", Num 4],
      [Mul [(1, 1), (3, 1)] (Just 99.0), Add [(1, 1), (3, 1)] (Just 99.0), Add [(1, 1), (3, 1), (3, 1)] (Just 99.0)],
      [Avg [(1, 1), (1, 2)] (Just 2.0), Mul [(1, 3), (1, 3)] (Just 99.0), Empty]]

-- początkowy arkusz
s1 :: Spreadsheet
s1 = [[Empty]]

-- aktualizaowanie podanej komórki
updateCell :: Spreadsheet -> Cell -> Cell
updateCell s (Empty) = Empty
updateCell s (Num a) = Num a
updateCell s (Text a) = Text a
updateCell s (Add l _) = Add l (doSum l s)
updateCell s (Mul l _) = Mul l (doMul l s)
updateCell s (Avg l _) = Avg l (doAvg l s)

-- aktualizowanie arkusza (bo po zmianie np A1(2) na A1(3) trzeba zaktualizować wartości wszystkich operacji, które z tego korzystają)
updateAll :: Spreadsheet -> Spreadsheet
updateAll s = map (map (updateCell s)) s

doSum :: [Location] -> Spreadsheet -> Maybe Float
doSum [] s = Just 0
doSum (l:lx) s = foldl (liftA2 (+)) val [(doSum lx s)]
    where val = case getCell s l of
                     Just Empty -> Just 0.0
                     Just (Num a) -> Just (fromIntegral a)
                     Just (Add l _) -> doSum l s
                     Just (Mul l _) -> doMul l s
                     Just (Avg l _) -> doAvg l s
                     otherwise -> Nothing

doMul :: [Location] -> Spreadsheet -> Maybe Float
doMul [] s = Just 1
doMul (l:lx) s = foldl (liftA2 (*)) val [(doMul lx s)]
    where val = case getCell s l of
                     Just Empty -> Just 1.0
                     Just (Num a) -> Just (fromIntegral a)
                     Just (Add l _) -> doSum l s
                     Just (Mul l _) -> doMul l s
                     Just (Avg l _) -> doAvg l s
                     otherwise -> Nothing

doAvg :: [Location] -> Spreadsheet -> Maybe Float
doAvg l s = case doSum l s of
                     Nothing -> Nothing
                     Just x -> Just (x / fromIntegral (length l))

-- założyłem, że wiersz dodawany jest po prostu na koniec
addRow :: Spreadsheet -> Spreadsheet
addRow s = s ++ [(take len (repeat Empty))]
    where len = case length s of
                     0 -> 0
                     _ -> length (s !! 0)

-- założyłem, że kolumna dodawana jest po prostu na koniec
addCol :: Spreadsheet -> Spreadsheet
addCol s = map (++ [Empty]) s

-- założyłem, że usunięcie wiersza usuwa po prostu ostatni wiersz
-- przy usuwaniu trzeba pamiętać o sprawdzeniu czy ktoś nie korzystał z komórek w tym wierszu i ewentualnie usunąć wszystkie do nich referecje
remRow :: Spreadsheet -> Spreadsheet
remRow s = case length s of
                0 -> s
                _ -> take rowIndex $ updateAll $ validateRow s rowIndex
                     where rowIndex = length s - 1

-- założyłem, że usunięcie kolumny usuwa po prostu ostatnią kolumnę
-- przy usuwaniu trzeba pamiętać o sprawdzeniu czy ktoś nie korzystał z komórek w tej kolumnie i ewentualnie usunąć wszystkie do nich referecje
remCol :: Spreadsheet -> Spreadsheet
remCol s = case length s of
                0 -> s
                _ -> case length (s !! 0) of
                          0 -> s
                          _ -> map (take colIndex) $ updateAll $ validateCol s colIndex
                               where colIndex = length (s !! 0) - 1

-- wywoływane przy usuwaniu kolumny. Podawany jest numer kolumny
-- dla każdej komórki uruchamia validateCell z numerem kolumny
validateCol :: Spreadsheet -> Int -> Spreadsheet
validateCol s cx = map (map (validateCell s cx (-1))) s

-- wywoływane przy usuwaniu wiersza. Podawany jest numer wiersza
-- dla każdej komórki uruchamia validateCell z numerem wiersza
validateRow :: Spreadsheet -> Int -> Spreadsheet
validateRow s rx = map (map (validateCell s (-1) rx)) s

-- gdy otrzyma komórkę z operacją dodawania, mnożenia lub średniej to aktualizuje lokalizacje poprzez usunięcie tych, które zawierają podane współrzędne 
validateCell :: Spreadsheet -> Int -> Int -> Cell -> Cell
validateCell s cx rx (Add l _) = Add newL (doSum newL s)
                where newL = remLocations l cx rx
validateCell s cx rx (Mul l _) = Mul newL (doMul newL s)
                where newL = remLocations l cx rx
validateCell s cx rx (Avg l _) = Avg newL (doAvg newL s)
                where newL = remLocations l cx rx
validateCell _ _ _ cell = cell

-- usuwa elementu z tablicy, które zawierają podane współrzędne (alternatywa)
remLocations :: [Location] -> Int -> Int -> [Location]
remLocations [] _ _ = []
remLocations ((c, r):ls) cx rx | (mkIndex c == cx || mkIndex r == rx) = remLocations ls cx rx
                               | otherwise = (c, r) : remLocations ls cx rx

-- pobiera komórkę z podanych współrzędnych lub zwraca Nothing, gdy złe współrzędne
getCell :: Spreadsheet -> Location -> Maybe Cell
getCell s (c, r) | 0 < length s && 
                   0 < length (s !! 0) &&
                   row < length s &&
                   col < length (s !! 0) = Just (s !! row !! col)
                 | otherwise = Nothing
                       where col = mkIndex c
                             row = mkIndex r

setCell :: Spreadsheet -> Location -> Cell -> Spreadsheet
setCell s (c, r) cell =
                       updateAll $ 
                       take row s ++
                       [take col (s !! row) ++ [cell] ++ drop (col + 1) (s !! row)] ++
                       drop (row + 1) s
                       where col = mkIndex c
                             row = mkIndex r

setEmpty :: Spreadsheet  -> Location -> Spreadsheet
setEmpty s l = setCell s l Empty

setInt :: Spreadsheet -> Int -> Location -> Spreadsheet
setInt s v l = setCell s l (Num v)

setText :: Spreadsheet -> String -> Location -> Spreadsheet
setText s v l = setCell s l (Text v)

setSum :: Spreadsheet -> [Location] -> Location -> Spreadsheet
setSum s ll l = setCell s l (Add ll Nothing)

setMul :: Spreadsheet -> [Location] -> Location -> Spreadsheet
setMul s ll l = setCell s l (Mul ll Nothing)
  
setAvg :: Spreadsheet -> [Location] -> Location -> Spreadsheet
setAvg s ll l = setCell s l (Avg ll Nothing)

-- funkcja, która zamienia chara na inta na zasadzie
-- a->1, b->2, A->1, C->3 itp
-- chodzi o to, żeby użytkownik mógł napisać "A2" zamiast "(1, 2)"
charToIntIndex :: Char -> Int
charToIntIndex c | fromEnum c >= 65 &&
                   fromEnum c <= 90 = fromEnum c - 64
                 | fromEnum c >= 97 &&
                   fromEnum c <= 122 = fromEnum c - 96
                 | otherwise = error "Indeksacja od A do Z!"

mkIndex :: Int -> Int
mkIndex i = case (i < 1) of
                 True  -> error "Indeksacja od 1!"
                 False -> i-1

-- tests example on console
-- let a1 = updateAll (updateAll ss)
-- let a2 = setSum a1 [('A', 2), ('c', 2)] ('b', 3)
-- let a3 = setInt a2 3 ('a', 1)
-- a3 (prints a3)
------------------
