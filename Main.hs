module Main where
import Spreadsheet
import Parser
import System.Environment
import Control.Monad
import System.Exit (exitSuccess)

-- kompilacja: :l Main.hs
-- uruchomienie komendą: main

-- główna funcja programu
main :: IO ()
main = do
  putStrLn "-----------START PROGRAMU-----------"
  mainMenu s1
  putStrLn "-----------KONIEC PROGRAMU-----------"

-- wyświetlenie menu i walidacja wybranej opcji
mainMenu :: Spreadsheet -> IO (Spreadsheet)
mainMenu ss = do
      putStr "Naciśnij ENTER aby kontynuować..."
      getLine
      putStrLn . unlines $ map concatNums choices
      choice <- getLine
      case validate choice of
         Just n  -> execute ss . read $ choice
         Nothing -> do
                  putStrLn "Nie rozpoznano komendy!"
                  mainMenu ss
      where concatNums (i, (s, _)) = show i ++ ". " ++ s

validate :: String -> Maybe Int
validate s = isValid (reads s)
   where isValid []            = Nothing
         isValid ((n, _):_) 
               | outOfBounds n = Nothing
               | otherwise     = Just n
         outOfBounds n = (n < 0) || (n > length choices)

choices :: [(Int, (String, Spreadsheet -> IO Spreadsheet))]
choices = zip [0.. ] [
   ("Wyjście", menuQuit),
   ("Wyświetl arkusz", menuShowSpreadsheet),
   ("Dodaj kolumnę", menuAddColumn),
   ("Dodaj wiersz", menuAddRow),
   ("Edytuj komórkę", menuEditCell),
   ("Usuń kolumnę", menuRemoveColumn),
   ("Usuń wiersz", menuRemoveRow)
 ]

execute :: Spreadsheet -> Int -> IO Spreadsheet
execute ss n = doExec $ filter (\(i, _) -> i == n) choices
      where doExec ((_, (_,f)):_) = f ss

noOption :: Spreadsheet -> IO Spreadsheet
noOption s = do
      putStrLn ("Nie ma takiej opcji")
      mainMenu s

menuShowSpreadsheet :: Spreadsheet -> IO Spreadsheet
menuShowSpreadsheet s = do
      putStrLn "Aktualny arkusz:"
      putStrLn ""
      showSpreadsheet s
      putStrLn ""
      mainMenu s

-- TODO parser dołączyć
menuEditCell :: Spreadsheet -> IO Spreadsheet
menuEditCell s = do
      putStrLn "Podaj lokalizację edytowanej komówrki:"
      loc <- getLine
      putStrLn "Podaj wartość:"
      value <- getLine
      putStrLn (" ---> Zmieniono komórkę")
      mainMenu $ setInt s 123 (1,1) -- tutaj w zależności od wyniku parsera wykonywać setInt, setText, setSum, setMul lub setAvg

menuAddColumn :: Spreadsheet -> IO Spreadsheet
menuAddColumn s = do
      putStrLn (" ---> Dodano kolumnę")
      mainMenu $ addCol s

menuAddRow :: Spreadsheet -> IO Spreadsheet
menuAddRow s = do
      putStrLn (" ---> Dodano wiersz")
      mainMenu $ addRow s

menuRemoveColumn :: Spreadsheet -> IO Spreadsheet
menuRemoveColumn s = do
      putStrLn (" ---> Usunięto kolumnę")
      mainMenu $ remCol s

menuRemoveRow :: Spreadsheet -> IO Spreadsheet
menuRemoveRow s = do
      putStrLn (" ---> Usunięto wiersz")
      mainMenu $ remRow s

menuQuit :: Spreadsheet -> IO Spreadsheet
menuQuit s = do
      exitSuccess

showSpreadsheet :: Spreadsheet -> IO ()
showSpreadsheet [] = return ()
showSpreadsheet s = do
      putStrLn (show (head s))
      showSpreadsheet (tail s)

-- budowanie:
-- ghc -o spop --make Main.hs
-- uruchomienie:
-- spop.exe
