module Format where
import Download
import Data.List
import Presets

--color data type
data Color = Green | Red | Blue

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
tupleToString::Bool->(Color,String,String, String, String)->String
tupleToString True (Green,t,a, b, c) = a ++ " \x03\&03" ++  b ++ " " ++  c ++ "%\x03"
tupleToString True (Red,t,a, b, c)   = a ++ " \x03\&04" ++ b ++ " " ++  c ++ "%\x03"
tupleToString True (Blue,t,a, b, c)  = a ++ " " ++  b ++ " " ++  c ++ "%"
tupleToString _ (Green,t,a, b, c)    = (presetConvert t)++ " \x03\&03" ++ b ++ " " ++  c ++ "%\x03"
tupleToString _ (Red,t,a, b, c)      = (presetConvert t) ++ "  \x03\&04" ++ b ++ " " ++  c ++ "%\x03"
tupleToString _ (Blue,t,a, b, c)     = (presetConvert t) ++ " " ++  b ++ " " ++  c ++ "%"




--add Color
addColor::(String,String,String,String)->(Color, String, String, String, String)
addColor (t,a,b,c) |"-" `isInfixOf` c = (Red,t,a,b,c)
addColor (t,a,b,c) |"+" `isInfixOf` c = (Green,t,a,b,c)
addColor (t,a,b,c)                    = (Blue,t,a,b,c)





