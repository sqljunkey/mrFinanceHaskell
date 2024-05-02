{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Format where
import Download
import Data.List
import Presets

--color data type
data Color = Green | Red | Blue
data Issue = Crypto | LongS| ShortS
data Tag = Change | AfterHours |Ch | None

-- Remove punctuation from text String.
removePunc :: String -> String
removePunc xs = [x | x <- xs, not (x `elem` ",+?!:;\"\'")]

--compose Crypto
composeCrypto::[String]->IO String
composeCrypto [] = return ""
composeCrypto (x:xs) = do 
                        a <-getCrypto x
                        rs<-composeCrypto xs
                        let string =  ( tupleToString Crypto $ formatStockData a) ++ rs
                        return string

--compose Final String together with Special Case
composeFinal::[String]->IO String
composeFinal [] = return ""
composeFinal (x:xs)| length (removePunc x) == 0 =return ""
composeFinal (x:xs)|length xs == 0 = do 
                            a <- getTickerStat x 
                            let string = ( tupleToString LongS $ formatStockData a) ++ ","
                            return string
composeFinal xs = composeString xs



--take a list of company names, prices and percentage changes and return a string
composeString::[String]->IO String
composeString [] = return ""
composeString (x:xs) = do 
                            a <- getTicker x 
                            rs<-composeString xs
                            let string = ( tupleToString ShortS $ formatStockData a) ++ rs
                                         
                            return  string 

-- tuple to string 
tupleToString::Issue->StockData->String
tupleToString _ StockData{..}| "" == price = ""
tupleToString LongS StockData{..}  =  ticker 
                                    ++ price
                                    ++ change
                                    ++ percentageChange
                                    ++ afterHours
                                    ++ companyName 
                                    ++ dividend 
                                    ++ priceSale
                                    ++ peRatio 
                                    ++ marketCap
                                    ++ weeksChange 
                                    ++ beta
                                    ++ volume
                                    ++ avgVolume
tupleToString ShortS StockData{..}     = ticker 
                                    ++  price 
                                    ++  percentageChange 
                                    ++ ", "
tupleToString Crypto StockData{..}     = companyName 
                                    ++  price 
                                    ++  percentageChange 
                                    ++ ", "

--String Identifier and Format
tagData::Color->String->String->String
tagData  _ _ "" ="" 
tagData Green "Price:"  a =" \x03\&03" ++  a ++  "\x03 "
tagData Red "Price:"  a = " \x03\&04" ++  a ++  "\x03 "
tagData Blue "Price:"  a = " " ++ a ++ " "
tagData Blue "Ch:"  a = " " ++ a ++ " "
tagData Green "Ch:"  a = " \x03\&03" ++ a ++ "\x03 " 
tagData Red "Ch:"  a = " \x03\&04" ++ a ++ "\x03 "
tagData Green "Change:"  a = " \x03\&03" ++  a ++  "%\x03 "
tagData Red "Change:"  a =   " \x03\&04" ++  a ++  "%\x03 "
tagData Blue "Change:"  a = a ++ "% "
tagData Green "AH:"  a ="\x03\&03AH: " ++ a ++ "%\x03 | "
tagData Red "AH:"  a ="\x03\&04AH: " ++ a ++ "%\x03 | "
tagData Blue "AH:"  a ="AH: " ++ a ++ "% | "
tagData c a b = a ++" "++ b ++ " | "

--helper StockData A
formatStockData::StockData->StockData
formatStockData s@StockData{..} = StockData{
                                          ticker = ((presetConvert ticker) ++":")
                                          ,companyName = tagData (addColor s None) "" companyName
                                          ,price = tagData (addColor s Change) "Price:" price
                                          ,percentageChange = tagData (addColor s Change) "Change:" percentageChange
                                          ,change = tagData (addColor s Ch) "Ch:" change
                                          ,dividend = tagData (addColor s None) "Div:" dividend
                                          ,marketCap = tagData (addColor s None) "MCap:" marketCap
                                          ,afterHours = tagData (addColor s AfterHours) "AH:" afterHours
                                          ,peRatio = tagData (addColor s None) "P/E:" peRatio 
                                          ,weeksChange = tagData (addColor s None ) "52WR:" weeksChange
                                          ,beta = tagData (addColor s None) "B:" beta
                                          ,volume = tagData (addColor s None) "V:" volume
                                          ,avgVolume = tagData (addColor s None) "AvgV:" avgVolume
                                          ,priceSale = tagData (addColor s None) "P/S:" priceSale}
 

--add Color
addColor::StockData->Tag->Color
addColor StockData{..} Ch|"-" `isInfixOf` change = Red
addColor StockData{..} Ch|"+" `isInfixOf` change = Green
addColor StockData{..} Change|"-" `isInfixOf` percentageChange = Red
addColor StockData{..} Change|"+" `isInfixOf` percentageChange = Green
addColor StockData{..} AfterHours|"-" `isInfixOf` afterHours = Red
addColor StockData{..} AfterHours|"+" `isInfixOf` afterHours = Green
addColor StockData{..} _ = Blue





