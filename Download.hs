
module Download where 

import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import Text.HTML.TagSoup
import Text.Regex.PCRE
import Text.Read
import Data.List.Utils
import Data.List
-- Remove punctuation from text String.
removePuncNR :: String -> String
removePuncNR xs = [x | x <- xs, not (x `elem` ",+?!:;nr\\\"\'")]



--give ticker, sheet , term  returns url String
--ticker
mwUrl::String->String->String->String
mwUrl a "b" "q" = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/balance-sheet/quarter"
mwUrl a "i" "q" = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials//income/quarter"                                 
mwUrl a "c" "q" = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/cash-flow/quarter"
mwUrl a "b" _ = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/balance-sheet"
mwUrl a "i" _ = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials"                                 
mwUrl a "c" _ = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/cash-flow"




---give ticker name and it returns url String
yfUrl::String->String
yfUrl a = "https://finance.yahoo.com/quote/" ++ a

yfUrlStat::String->String
yfUrlStat a = "https://finance.yahoo.com/quote/" ++ a ++ "/key-statistics?p="

--download html 
downloadHtml :: String->Int-> IO  String
downloadHtml a n = do
    request <- parseRequest a 
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings
    res <- httpLbs request manager
    
    let justText =  (fromFooter  $ parseTags $ show res)
    --putStrLn $ show justText
    
    return $ show justText 
     
        where fromFooter =  innerText  .take n . dropWhile (~/= "<body>")
     
--Stock Data Record
data StockData = StockData { companyName :: String  
                     , ticker :: String  
                     , price :: String  
                     , percentageChange :: String 
                     , dividend :: String  
                     , marketCap :: String  
                     , afterHours :: String  
                     , weeksChange :: String
                     , peRatio::String
                     , beta :: String
                     } deriving (Show)


--convert list to string
listToString::[[String]]->Int->String
listToString [] _ = ""
listToString l d =  if length (l) > 0 && length (l !! 0) > d  
                    then 
                        (l !! 0 !! d)
                    else
                        ""

--do regex on yahoo web string      
parseHtml::String ->String->Maybe StockData
parseHtml a tick = 
    let priceList =  a=~"Currency\\s+in(.+?)((\\d+,)?\\d+\\.\\d+)((-|\\+)?\\d+\\.\\d+)\\s+\\(((-|\\+)?\\d+\\.\\d+)\\%\\)"::[[String]]
        companyName = a=~"(Frame'\\);}(.*?))\\((.*?)\\)(.*?)Currency"::[[String]]
        dividend=  a=~"Dividend & Yield\\d+.\\d+\\s+\\((\\d+.\\d+%)\\)"::[[String]]
        marketCap =  a=~"Market Cap(\\d+\\.\\d+(B|T|M))"::[[String]]
        afterHours =   a=~"\\(((-|\\+)\\d+.\\d+)%\\)After hours"::[[String]]
        peRatio = a=~"PE\\s+Ratio\\s+\\(TTM\\)(\\d+.\\d+)"::[[String]]
        weeksChange = a=~"52-Week\\s+Change\\s+3(\\d+\\.\\d+)%S"::[[String]]
        beta        = a=~"Beta\\s+\\(5Y Monthly\\)\\s+(\\d+\\.\\d+)52"::[[String]]

    in  if companyName==[] || priceList== [] then
        Nothing
        else 
        Just(StockData{ companyName = (listToString companyName  2) 
                       ,ticker = tick
                       ,price = (listToString priceList  2)
                       ,percentageChange = (listToString priceList 6)
                       ,dividend= (listToString dividend  1) 
                       ,marketCap = (listToString marketCap  1)
                       ,afterHours =(listToString afterHours 1)
                       ,peRatio =(listToString peRatio 1) 
                       ,weeksChange = (listToString weeksChange 1)
                       ,beta =(listToString beta 1)
                        })


    --get ticker with simple information
getTicker :: String -> IO StockData
getTicker tick  = do
  str' <- downloadHtml  (yfUrl tick) 2000
  str1' <- downloadHtml  (yfUrlStat tick) 2000


  case parseHtml str' tick of
    Nothing -> pure StockData{companyName ="" 
                       ,ticker = tick
                       ,price = ""
                       ,percentageChange = ""
                       ,dividend= ""  
                       ,marketCap = ""
                       ,peRatio =""
                       ,afterHours ="" 
                       ,weeksChange=""
                       ,beta=""}-- from f
    Just str'' -> pure str'' -- from g if it exists

    --get ticker with extended information 
getTickerStat :: String -> IO StockData
getTickerStat tick  = do
  str' <- downloadHtml  (yfUrl tick) 2000
  str1' <- downloadHtml  (yfUrlStat tick) 2000


  case parseHtml (str'++str1') tick of
    Nothing -> pure StockData{companyName ="" 
                       ,ticker = tick
                       ,price = ""
                       ,percentageChange = ""
                       ,dividend= ""  
                       ,marketCap = ""
                       ,peRatio =""
                       ,afterHours ="" 
                       ,weeksChange=""
                       ,beta=""}-- from f
    Just str'' -> pure str'' -- from g if it exists

--Reduce Spaces
formatSpace :: String -> String
formatSpace = foldr go ""
  where
    go x acc = x:if x == ' ' then dropWhile (' ' ==) acc else acc

 
--(get Rev, Cash,Debt,Asset )
getSheets::String->String->String-> IO [String]
getSheets c tick quarter= do
       str1 <- downloadHtml  (mwUrl tick "b"  quarter) 10000
       str2 <- downloadHtml  (mwUrl tick "c"  quarter) 10000
       str3 <- downloadHtml  (mwUrl tick "i"  quarter) 10000
        
       let test = parseSheetHtml $ removePuncNR ( str1 ++ str2 ++ str3)
       
      
       return $ replaceStuffs c $ flattenSheet test


--Parse balance sheet data
parseSheetHtml::String->[[String]]
parseSheetHtml a = 
                let totalAsset =a =~"(Total\\s+Assets)(\\s+(\\d+.\\d+)(T|B|M|K))+" ::[[String]]
                    totalLiabilities = a =~ "(Total\\s+Liabilities)(\\s+(\\d+.\\d+)(T|B|M|K))+" ::[[String]] 
                    freeCashflow = a =~ "(Fee\\s+Cash\\s+Flow)(\\s+(\\d+.\\d+)(T|B|M|K))+" ::[[String]] 
                    opCashflow  = a =~ "(Net\\s+Opeatig\\s+Cash\\s+Flow)(\\s+(\\d+.\\d+)(T|B|M|K))+" ::[[String]] 
                    revenue  = a =~ "(Reveue)(\\s+(\\d+.\\d+)(T|B|M|K))+" ::[[String]] 
                    netIncome  = a =~ "(Net\\s+Icome)(\\s+(\\d+.\\d+)(T|B|M|K))+" ::[[String]] 
                    interstIncome = a =~ "(Iteest\\s+Icome)(\\s+(\\d+.\\d+)(T|B|M|K))+" ::[[String]] 
                    
               in 
                 [interstIncome !! 0 ]
                ++ revenue
                ++ [netIncome !! 0]
                ++ opCashflow 
                ++ freeCashflow 
                ++totalAsset 
                ++ totalLiabilities 
                
flattenSheet::[[String]]->[String]
flattenSheet [] = []
flattenSheet (x:xs) =    [formatSpace $ x !! 0] ++  flattenSheet xs
                                    
replaceStuffs::String->[String]->[String]
replaceStuffs c [] = []
replaceStuffs c (x:xs)| "Iteest Icome" `isInfixOf` x = [replace "Iteest Icome" ("PRIVMSG:"++c++" :Interest Income") x] ++ replaceStuffs c xs 
replaceStuffs c (x:xs)| "Reveue" `isInfixOf` x = [replace "Reveue" ("PRIVMSG:"++c++" :Revenue") x] ++ replaceStuffs c xs   
replaceStuffs c (x:xs)| "Net Opeatig Cash Flow" `isInfixOf` x = [replace "Net Opeatig Cash Flow" ("PRIVMSG:"++c++" :Net Operatig Cash Flow") x] ++ replaceStuffs c xs          
replaceStuffs c (x:xs)| "Fee Cash Flow" `isInfixOf` x = [replace "Fee Cash Flow" ("PRIVMSG:"++c++" :Free Cash Flow") x] ++ replaceStuffs c xs
replaceStuffs c (x:xs)| "Net Icome" `isInfixOf` x = [replace "Net Icome" ("PRIVMSG:"++c++" :Net Income") x] ++ replaceStuffs c xs
replaceStuffs c (x:xs) = [("PRIVMSG:"++c++" :"++x)] ++ replaceStuffs c xs