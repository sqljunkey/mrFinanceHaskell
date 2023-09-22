{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Index where
import Download
import Format
import Data.List
import Text.Printf


--Open Files
getAverage::String->IO String
getAverage a = do 
            com<-readFile a 
            let stock =  lines com
            (b,c)<- accumulatePrice stock
            let price = (b / fromIntegral (length stock))
            let change = (c / fromIntegral (length stock))
            let final = (roundToStr 2 price ++ " " ++ roundToStr 2 change)
            
            return  $ addColorToString final

--AddColor in Case of Minus Sign
addColorToString:: String->String
addColorToString a| "-" `isInfixOf` a ="\x03\&04" ++ a ++ "%\x03"
addColorToString a = "\x03\&03" ++ a ++ "%\x03"

--Accumulate Price
accumulatePrice::[String]->IO (Double,Double)
accumulatePrice [] = return (0.0,0.0)
accumulatePrice (x:xs) = do 
                           a@StockData{..} <- getTicker x
                           let f = read price :: Double
                           let h = read $ removePunc percentageChange :: Double
                           (s,t) <- accumulatePrice xs
                           return ((f + s),(h + t))

--Round Number to String
roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

--Presets
presetIndex::String->IO String
presetIndex "hammond" = do
                        a <-getAverage "recession.lst"
                        return $ "Hammond Recession Proof Index. : " ++   a
presetIndex "bad" = do
                    a <-getAverage "bad.lst"
                    return $ "Hammond's About To File For Chapter 11 Index. :" ++  a
presetIndex "ai" = do 
                    a <-getAverage "ai.lst"
                    return $ "The Artificial Intelligence Index. :" ++  a
presetIndex a = return ""

