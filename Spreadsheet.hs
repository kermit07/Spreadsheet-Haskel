module Spreadsheet where
import Data.Function
import Data.Maybe
import Control.Applicative

type Location = (Int, Int)

data Cell = Empty
          | Num Int
          | Text String
          | Add [Location] (Maybe Float)
          | Mul [Location] (Maybe Float)
          | Avg [Location] (Maybe Float)

instance Show Cell where
  show (Empty) = "_"
  show (Num a) = show a
  show (Text a) = a
  show (Add l (Just x)) = show x
  show (Add l Nothing) = "SUM ERR"
  show (Mul l (Just x)) = show x
  show (Mul l Nothing) = "MUL ERR"
  show (Avg l (Just x)) = show x
  show (Avg l Nothing) = "AVG ERR"

type Spreadsheet = [[Cell]]

ss :: Spreadsheet
ss = [[Num 2, Text "abc", Num 4],
      [Mul [(1,1), (1,3)] (Just 99.0), Add [(1,1), (1,3)] (Just 99.0), Add [(1,1), (1,3), (1,3)] (Just 99.0)],
      [Avg [(1,1), (2,1)] (Just 2.0), Empty, Empty]]

updateCell :: Spreadsheet -> Cell -> Cell
updateCell s (Empty) = Empty
updateCell s (Num a) = Num a
updateCell s (Text a) = Text a
updateCell s (Add l _) = Add l (doSum l s)
updateCell s (Mul l _) = Mul l (doMul l s)
updateCell s (Avg l _) = Avg l avg
    where avg = case doSum l s of
                     Nothing -> Nothing
                     Just x -> Just (x / fromIntegral (length l))

updateAll :: Spreadsheet -> Spreadsheet
updateAll s = map (map (updateCell s)) s

doSum :: [Location] -> Spreadsheet -> Maybe Float
doSum [] s = Just 0
doSum (l:lx) s = foldl (liftA2 (+)) val [(doSum lx s)]
    where val = case getCell s l of
                     Just Empty -> Just 0.0
                     Just (Num a) -> Just (fromIntegral a)
                     Just (Add _ a) -> a
                     Just (Mul _ a) -> a
                     Just (Avg _ a) -> a
                     otherwise -> Nothing

doMul :: [Location] -> Spreadsheet -> Maybe Float
doMul [] s = Just 1
doMul (l:lx) s = foldl (liftA2 (*)) val [(doMul lx s)]
    where val = case getCell s l of
                     Just Empty -> Just 1.0
                     Just (Num a) -> Just (fromIntegral a)
                     Just (Add _ a) -> a
                     Just (Mul _ a) -> a
                     Just (Avg _ a) -> a
                     otherwise -> Nothing

addRow :: Spreadsheet -> Spreadsheet
addRow s = s ++ [(take len (repeat Empty))]
    where len = case length s of
                     0 -> 1
                     _ -> length (s !! 0)

addCol :: Spreadsheet -> Spreadsheet
addCol s = map (++ [Empty]) s

remRow :: Spreadsheet -> Spreadsheet
remRow s = case length s of
                0 -> s
                _ -> take (length s-1) s

remCol :: Spreadsheet -> Spreadsheet
remCol s = case length s of
                0 -> s
                _ -> case length (s !! 0) of
                          0 -> s
                          _ -> map (take (length (s !! 0) - 1)) s

getCell :: Spreadsheet -> Location -> Maybe Cell
getCell s (r, c) | 0 < length s && 
                   0 < length (s !! 0) &&
                   r <= length s &&
                   c <= length (s !! 0) = Just (s !! row !! col)
                 | otherwise = Nothing
                                    where row =  mkIndex r
                                          col = mkIndex c

setCell :: Spreadsheet -> Location -> Cell -> Spreadsheet
setCell s (r, c) cell =
                       take row s ++
                       [take col (s !! row) ++ [cell] ++ drop (col + 1) (s !! row)] ++
                       drop (row + 1) s
                       where row =  mkIndex r
                             col = mkIndex c

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

mkIndex :: Int -> Int
mkIndex i = case (i < 1) of
                 True  -> error "Indeksacja od 1!"
                 False -> i-1
