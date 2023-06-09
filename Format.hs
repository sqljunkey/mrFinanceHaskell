{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Format where
import Download
import Data.List
import Presets

--color data type
data Color = Green | Red | Blue
data Issue = Crypto | LongS| ShortS

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
                                    ++ percentageChange
                                    ++ companyName 
                                    ++ dividend 
                                    ++ peRatio 
                                    ++ afterHours 
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
tagData Blue "Price:"  a = a ++ " "
tagData Green "Change:"  a = " \x03\&03" ++  a ++  "%\x03 "
tagData Red "Change:"  a =   " \x03\&04" ++  a ++  "%\x03 "
tagData Blue "Change:"  a = a ++ "% "
tagData _ "AfterHours:"  a ="After Hour(s): " ++ a ++ "% "
tagData _ "52 Week Change:"  a ="52 Week Change: " ++ a ++ "% "
tagData c a b = a ++" "++ b ++ " "

--helper StockData A
formatStockData::StockData->StockData
formatStockData s@StockData{..} = StockData{
                                          ticker = ((presetConvert ticker) ++":")
                                          ,companyName = tagData (addColor s) "" companyName
                                          ,price = tagData (addColor s) "Price:" price
                                          ,percentageChange = tagData (addColor s) "Change:" percentageChange
                                          ,dividend = tagData (addColor s) "Div:" dividend
                                          ,marketCap = tagData (addColor s) "Market Cap:" marketCap
                                          ,afterHours = tagData (addColor s) "AfterHours:" afterHours
                                          ,peRatio = tagData (addColor s) "P/E:" peRatio 
                                          ,weeksChange = tagData (addColor s) "52 Week Change:" weeksChange
                                          ,beta = tagData (addColor s) "Beta:" beta
                                          ,volume = tagData (addColor s) "Volume:" volume
                                          ,avgVolume = tagData (addColor s) "Avg. Volume:" avgVolume}
 

--add Color
addColor::StockData->Color
addColor StockData{..} |"-" `isInfixOf` percentageChange = Red
addColor StockData{..} |"+" `isInfixOf` percentageChange = Green
addColor StockData{..}                 = Blue





