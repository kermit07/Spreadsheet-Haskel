module Main where
import Spreadsheet
import Parser
import Data.Binary
import qualified Data.ByteString.Lazy
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
   ("Edytuj komórkę", menuReadLocation),
   ("Usuń kolumnę", menuRemoveColumn),
   ("Usuń wiersz", menuRemoveRow),
   ("Zapisz do pliku", menuSaveToFile),
   ("Wczytaj z pliku", menuReadFromFile)
 ]

execute :: Spreadsheet -> Int -> IO Spreadsheet
execute ss n = doExec $ filter (\(i, _) -> i == n) choices
      where doExec ((_, (_,f)):_) = f ss

noOption :: Spreadsheet -> IO Spreadsheet
noOption s = do
      putStrLn "Nie ma takiej opcji"
      mainMenu s

menuShowSpreadsheet :: Spreadsheet -> IO Spreadsheet
menuShowSpreadsheet s = do
      putStrLn "Aktualny arkusz:"
      putStrLn ""
      showSpreadsheet s
      putStrLn ""
      mainMenu s

menuReadLocation :: Spreadsheet -> IO Spreadsheet
menuReadLocation s = do 
      putStrLn "Podaj lokalizację: "
      loc <- getLine
      case parse parseLoc loc of
        [(a, b)] -> do
                  case a of
                    (r, c) -> do case getCell s (r,c) of
                                  Just _ -> do putStrLn $ "Wybrana lokalizacja: " ++ loc
                                               menuEditCell s (r, c)
                                  Nothing -> do putStrLn "BŁĄD! (poza zakresem)"
                                                menuReadLocation s
        [] -> do putStrLn "BŁĄD! Podaj np. A1 lub D3"
                 menuReadLocation s

menuEditCell :: Spreadsheet -> Location -> IO Spreadsheet
menuEditCell s l = do
      putStrLn "Podaj wartość:"
      value <- getLine
      case parse myParser value of
        [(a, b)] -> do putStrLn " ---> Zmieniono wartość"
                       case a of
                         ParserText text -> do mainMenu $ setText s text l
                         ParserNum num -> do mainMenu $ setInt s num l
                         ParserSum locs -> do mainMenu $ setSum s locs l
                         ParserMul locs -> do mainMenu $ setMul s locs l
                         ParserAvg locs -> do mainMenu $ setAvg s locs l
        [] -> do putStrLn "Zła wartość!" -- tutaj można wyświetlic podpowiedź jakie wartości są dozwolone
                 menuEditCell s l

menuAddColumn :: Spreadsheet -> IO Spreadsheet
menuAddColumn s = do
      putStrLn " ---> Dodano kolumnę"
      mainMenu $ addCol s

menuAddRow :: Spreadsheet -> IO Spreadsheet
menuAddRow s = do
      putStrLn " ---> Dodano wiersz"
      mainMenu $ addRow s

menuRemoveColumn :: Spreadsheet -> IO Spreadsheet
menuRemoveColumn s = do
      putStrLn " ---> Usunięto kolumnę"
      mainMenu $ remCol s

menuRemoveRow :: Spreadsheet -> IO Spreadsheet
menuRemoveRow s = do
      putStrLn " ---> Usunięto wiersz"
      mainMenu $ remRow s

menuSaveToFile :: Spreadsheet -> IO Spreadsheet
menuSaveToFile s = do
      putStrLn "Podaj nazwę pliku do zapisu: "
      fileName <- getLine
      Data.ByteString.Lazy.writeFile fileName (encode s)
      putStrLn "Zapisano!"
      mainMenu s

menuReadFromFile :: Spreadsheet -> IO Spreadsheet
menuReadFromFile s = do
      putStrLn "Podaj nazwę pliku do odczytu: "
      fileName <- getLine
      fileContent <- Data.ByteString.Lazy.readFile fileName
      let newS = (decode fileContent)
      putStrLn "Przeczytano!"
      mainMenu newS

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
