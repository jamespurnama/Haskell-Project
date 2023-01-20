------------------
-- James Purnama--
------------------
import System.IO
import System.Exit
import Control.Monad
import Data.List.Split
import Text.Printf
import System.Directory(copyFile,removeFile)
import Data.Char

main = forever (mainMenu >> readChoice >>= mainMenuAction)
mainMenu = do 
        putStrLn "\ESC[1J"
        putStrLn "+-------------------------+"
        putStrLn "| MAIN MENU - ORISTINGRAY |"
        putStrLn "+-------------------------+"
        putStrLn "(0) Exit"
        putStrLn "(1) Stingray Fish Data"
        putStrLn "(2) handicraft craftsmen Data"
        putStrLn "(3) Stingray Leather Handicraft Data"
        putStrLn "(4) Originality Tracking of Handicraft"
        putStr   "Please Input Your Choice: " >> hFlush stdout

readChoice = hSetBuffering stdin NoBuffering >> hSetEcho stdin True >> getChar
mainMenuAction '1' = modul01
mainMenuAction '2' = modul02
mainMenuAction '3' = modul03
mainMenuAction '4' = modul04
mainMenuAction '0' = do
        putStrLn "\nGood By and Thank You"
        exitSuccess
mainMenuAction _   = do 
        hPutStrLn stderr "\nPlease input a number between 0 to 4"
        hPutStrLn stderr "Press any key to continue" 
        x <- getChar
        putStrLn ""
-- data Fish = Fish {fishID :: String, fishName :: String, fishOrigin :: String} deriving Show
-- addFish :: String -> String -> String -> Fish
-- addFish x1 x2 x3 = Fish {fishID = x1, fishName = x2, fishOrigin = x3}
                                        
---------------------------------------------------------------
modul01 = forever (modul01Menu >> readChoice >>= modul01Action)
modul01Menu = do 
        putStrLn "\ESC[1J"
        putStrLn "+--------------------+"
        putStrLn "| STINGRAY FISH DATA |"
        putStrLn "+--------------------+"
        putStrLn "(0) Back to Main Menu"
        putStrLn "(1) Input Stingray Fish Data"
        putStrLn "(2) Delete Stingray Fish Data"
        putStrLn "(3) Print Stingray Fish Data"
        putStr "Please Input Your Choice: " >> hFlush stdout
modul01Action '0' = main
modul01Action '1' = modul0101_InputStingray
modul01Action '2' = delData "Fish.txt" "STINGRAY FISH" "Fish ID"
modul01Action '3' = modul0103_PrintStringray 
modul01Action _   = do 
        hPutStrLn stderr "\nPlease input a number between 0 to 3"
        hPutStrLn stderr "Press any key to continue" 
        x <- getChar
        putStrLn ""

----------------------------
modul0101_InputStingray = do
        putStrLn "\ESC[1J"
        putStrLn "*~~~~~~~~~~~~~~~~~~~~~~~~~~*"
        putStrLn "{ INPUT STINGRAY FISH DATA }"
        putStrLn "*~~~~~~~~~~~~~~~~~~~~~~~~~~*"
        putStr "Fish ID       : " >> hFlush stdout
        fishID <- getLine 
        putStr "Fish Name     : " >> hFlush stdout
        fishName <- getLine 
        putStr "Fish Origin   : " >> hFlush stdout
        fishOrigin <- getLine                           
        -- let fishData = addFish fishID fishName fishOrigin
        let fishTxt = "\n" ++ fishID ++ "," ++ fishName ++ "," ++ fishOrigin 
        appendFile "Fish.txt" fishTxt
        putStrLn "The fish data has been saved, Thank You."
        putStrLn "Press Any Key To Continue..."
        x <- getChar
        putStrLn ""

-----------------------------
delData :: [Char] -> [Char] -> [Char] -> IO()
delData jFileName jDataName jIdName = do 
-- modul0102_DeleteStingray = do 
        dataTxt <- readFile jFileName
        let y = splitOn "\n" dataTxt 
        putStrLn "\ESC[1J"
        let jTitle = "{ DELETE " ++ jDataName ++ " DATA }"
        putStr "*"
        replicateM_  ((length jTitle)-2) $ putStr "-"
        putStrLn "*"
        putStrLn jTitle 
        putStr "*"
        replicateM_  ((length jTitle)-2) $ putStr "-"
        putStrLn "*"
        putStr (jIdName ++ " : ") >> hFlush stdout
        jID <- getLine 
        delRow y jID jFileName
        putStr "" 

----------------------------------------------
delRow :: [[Char]] -> [Char] -> [Char] -> IO()
delRow [] sKey jFileName = do
        removeFile "tempJms.txt"
        putStrLn "Not Found"
        putStrLn "\nPress Any Key To Continue..."
        x <- getChar
        putStrLn ""         
delRow (x:xs) sKey jFileName = do
        let a = splitOn "," x
        if ((head a) == sKey) 
                then do 
                        putStrLn "Found: "
                        print a   
                        mapM_ (\n -> (appendFile "tempJms.txt" ("\n"++n))) xs  
                        copyFile "tempJms.txt" jFileName
                        removeFile "tempJms.txt"
                        putStrLn "\nThe data has been deleted. \nPress Any Key To Continue..."
                        x <- getChar
                        putStrLn ""         
                else do 
                        appendFile "tempJms.txt" ("\n"++x)
                        delRow xs sKey jFileName

----------------------------
modul0103_PrintStringray = do
        fishTxt <- readFile "Fish.txt"
        let y = splitOn "\n" fishTxt 
        putStrLn "\ESC[1J"
        putStrLn "*~~~~~~~~~~~~~~~~~~~~*"
        putStrLn "{ STINGRAY FISH DATA }"
        putStrLn "*~~~~~~~~~~~~~~~~~~~~*\n"
        replicateM_ 64 $ putStr "-"
        printf "\n|%-20s|%-20s|%-20s|\n" "Fish ID" "Fish Name" "Ocean Origin"  
        replicateM_ 64 $ putStr "-"
        putStrLn ""
        printRows y 64

---------------------------------------------------------------
modul02 = forever (modul02Menu >> readChoice >>= modul02Action)
modul02Menu = do 
              putStrLn "\ESC[1J"
              putStrLn "+---------------------------+"
              putStrLn "| HANDICRAFT CRAFTSMEN DATA |"
              putStrLn "+---------------------------+"
              putStrLn "(0) Back to Main Menu"
              putStrLn "(1) Input handicraft craftsmen Data"
              putStrLn "(2) Delete handicraft craftsmen Data"
              putStrLn "(3) Print handicraft craftsmen Data"
              putStr "Please Input Your Choice: " >> hFlush stdout
modul02Action '0' = main
modul02Action '1' = modul0201_InputCraftsmen
modul02Action '2' = delData "craftsmen.txt" "HANDICAFT CRAFTSMEN" "Craftsmen ID"
modul02Action '3' = modul0203_PrintCraftsmen 
modul02Action _   = do 
        hPutStrLn stderr "\nPlease input a number between 0 to 3"
        hPutStrLn stderr "Press any key to continue" 
        x <- getChar
        putStrLn ""

----------------------------
modul0201_InputCraftsmen = do
                          putStrLn "\ESC[1J"
                          putStrLn "*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*"
                          putStrLn "{ INPUT HANDICRAFT CRAFTSMEN DATA }"
                          putStrLn "*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*"
                          putStr "Craftsmen ID     : " >> hFlush stdout
                          craftsmenID <- getLine 
                          putStr "Craftsmen Name   : " >> hFlush stdout
                          craftsmenName <- getLine 
                          putStr "Craftsmen City   : " >> hFlush stdout
                          craftsmenCity <- getLine                           
                          let craftsmenTxt = "\n" ++ craftsmenID ++ "," ++ craftsmenName ++ "," ++ craftsmenCity 
                          appendFile "craftsmen.txt" craftsmenTxt
                          putStrLn "The craftsmen data has been saved, Thank You."
                          putStrLn "Press Any Key To Continue..."
                          x <- getChar
                          putStrLn ""

----------------------------
modul0203_PrintCraftsmen = do
        craftsmenTxt <- readFile "craftsmen.txt"
        let y = splitOn "\n" craftsmenTxt 
        putStrLn "\ESC[1J"
        putStrLn "*~~~~~~~~~~~~~~~~~~~~~~~~~~~*"
        putStrLn "{ HANDICRAFT CRAFTSMEN DATA }"
        putStrLn "*~~~~~~~~~~~~~~~~~~~~~~~~~~~*\n"
        replicateM_ 65 $ putStr "-"
        printf "\n|%-20s|%-20s|%-20s|" "Craftsmen ID" "Craftsmen Name" "Craftsmen City"  
        putStrLn ""
        replicateM_ 65 $ putStr "-"
        putStrLn ""
        printRows y 65

---------------------------------------------------------------
modul03 = forever (modul03Menu >> readChoice >>= modul03Action)
modul03Menu = do 
              putStrLn "\ESC[1J"
              putStrLn "+-------------------~~~~~~~--------+"
              putStrLn "| STINGRAY LEATHER HANDICRAFT DATA |"
              putStrLn "+--------------------------~~~~~~~-+"
              putStrLn "(0) Back to Main Menu"
              putStrLn "(1) Input  handicraft Data"
              putStrLn "(2) Delete handicraft Data"
              putStrLn "(3) Print  handicraft Data"
              putStr "Please Input Your Choice: " >> hFlush stdout
modul03Action '0' = main
modul03Action '1' = modul0301_InputProduct
modul03Action '2' = delData "product.txt" "STINGRAY LEATHER HANDICRAFT PRODUCT" "Product ID"
modul03Action '3' = modul0303_PrintProduct 
modul03Action _   = do 
                    hPutStrLn stderr "\nPlease input a number between 0 to 3"
                    hPutStrLn stderr "Press any key to continue" 
                    x <- getChar
                    putStrLn ""

----------------------------
modul0301_InputProduct = do
                          putStrLn "\ESC[1J"
                          putStrLn "*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*"
                          putStrLn "{ INPUT STINGRAY LEATHER HANDICRAFT DATA }"
                          putStrLn "*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*"
                          putStr "Produk ID    : " >> hFlush stdout
                          productID <- getLine 
                          putStr "Produk Name   : " >> hFlush stdout
                          productName <- getLine 
                          putStr "Oristingray Fish ID   : " >> hFlush stdout
                          fishID <- getLine                           
                          putStr "Handicraft Craftsmen ID   : " >> hFlush stdout
                          craftsmenID <- getLine                           
                          let productTxt = "\n" ++ productID ++ "," ++ productName ++ "," ++ fishID ++ "," ++ craftsmenID
                          appendFile "product.txt" productTxt
                          putStrLn "The Product data has been saved, Thank You."
                          putStrLn "Press Any Key To Continue..."
                          x <- getChar
                          putStrLn ""

----------------------------
modul0303_PrintProduct = do
        productTxt <- readFile "product.txt"
        let y = splitOn "\n" productTxt 
        putStrLn "\ESC[1J"
        putStrLn "*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*"
        putStrLn "{ STINGRAY LEATHER HANDICRAFT  DATA }"
        putStrLn "*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\n"
        replicateM_ 85 $ putStr "-"
        printf "\n|%-20s|%-20s|%-20s|%-20s|" "Product ID" "Product Name" "Fish ID" "Handicraft ID"  
        putStrLn ""
        replicateM_ 85 $ putStr "-"
        putStrLn ""
        printRows y 85

-------------------------------------
printRows :: [[Char]] -> Int ->  IO()
printRows [] rowLength = do 
        replicateM_ rowLength $ putStr "-"
        putStrLn "\nPress Any Key To Continue..."
        x <- getChar
        putStrLn ""         
printRows (x:xs) rowLength = do
        let a = splitOn "," x
        if (null (head a)) then 
                printRows xs rowLength
        else do 
                mapM_ (\n -> (printf"|%-20s" n )) a
                putStrLn "|"         
                printRows xs rowLength

modul04 = do
        dataTxt <- readFile "product.txt"
        let y = splitOn "\n" dataTxt 
        putStrLn "\ESC[1J"
        putStrLn "*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*"
        putStrLn "{ TRACKING THE ORIGINALITY OF HANDICRAFT PRODUCT}"
        putStrLn "*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*\n"
        putStrLn "To track the originality of the Stingray Handicraft product, "
        putStrLn "please input the Product ID."
        putStr   "Product ID = " >> hFlush stdout
        jProdID <- getLine 
        checkOri y (map toUpper jProdID)

checkOri :: [[Char]] -> [Char] -> IO()
checkOri [] sKey = do 
        putStrLn "The Product ID is not found in our database, \nthen we cannot guarantee the originality of this product."
        putStrLn "\nPress Any Key To Continue..."
        x <- getChar
        putStrLn ""         
checkOri (x:xs) sKey = do
        let a = splitOn "," x
        
        if ((map toUpper $ head a) == sKey) 
                then do 
                        putStrLn "Found: "
                        printf "Product ID       = %20s\n" (a!!0)
                        printf "Product Name     = %20s\n" (a!!1)   
                        printf "Stingray Fish ID = %20s\n" (a!!2)
                        printf "Craftsmen ID     = %20s\n" (a!!3)   
                        putStrLn "\nPress Any Key To Continue..."
                        x <- getChar
                        putStrLn ""         
                else do 
                        checkOri xs sKey
