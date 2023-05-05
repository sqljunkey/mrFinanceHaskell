{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Format where
import Download
import Data.List
import Presets

--color data type
data Color = Green | Red | Blue

-- Remove punctuation from text String.
removePunc :: String -> String
removePunc xs = [x | x <- xs, not (x `elem` ",+?!:;\"\'")]



--compose Final String together with Special Case
composeFinal::[String]->IO String
composeFinal [] = return ""
composeFinal (x:xs)|length xs == 0 = do 
                            a <- getTicker x 
                            let string = ( tupleToString True $ addColor a) 
                            return string
composeFinal xs = composeString xs

--take a list of company names, prices and percentage changes and return a string
composeString::[String]->IO String
composeString [] = return ""
composeString (x:xs) = do 
                            a <- getTicker x 
                            rs<-composeString xs
                            let string = ( tupleToString False $ addColor a) ++ ", " ++ rs
                                         
                           
                            return string 

-- tuple to string 
tupleToString::Bool->(Color,StockData)->String
tupleToString True (Green,StockData{..}) = companyName ++ " \x03\&03" ++  price ++ " " ++  percentageChange ++ "%\x03" ++ " Div:" ++ dividend ++ " MarketCap: "++ marketCap
tupleToString True (Red,StockData{..})   = companyName ++ " \x03\&04" ++ price ++ " " ++  percentageChange ++ "%\x03" ++ " Div:" ++ dividend ++ " MarketCap: "++ marketCap
tupleToString True (Blue,StockData{..})  = companyName ++ " " ++  price ++ " " ++  percentageChange ++ "%" ++ " Div:" ++ dividend ++ " MarketCap: "++ marketCap
tupleToString _ (Green,StockData{..})    = (presetConvert ticker)++ " \x03\&03" ++ price ++ " " ++  percentageChange ++ "%\x03"
tupleToString _ (Red,StockData{..})      = (presetConvert ticker) ++ "  \x03\&04" ++ price ++ " " ++  percentageChange ++ "%\x03"
tupleToString _ (Blue,StockData{..})     = (presetConvert ticker) ++ " " ++  price ++ " " ++  percentageChange ++ "%"




--add Color
addColor::StockData->(Color, StockData)
addColor s@StockData{percentageChange} |"-" `isInfixOf` percentageChange = (Red,s)
addColor s@StockData{percentageChange} |"+" `isInfixOf` percentageChange = (Green,s)
addColor s                   = (Blue,s)





