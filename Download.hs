
module Download where 

import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import Text.HTML.TagSoup
import Text.Regex.PCRE
import Text.Read
import Data.List.Utils
import Data.List
import Data.Char
import qualified Network.HTTP.Types as HTypes
import qualified Data.ByteString.Char8 as C8

-- Remove punctuation from text String.
removePuncNR :: String -> String
removePuncNR xs = [x | x <- xs, not (x `elem` ",+?!:;nr\\\"\'")]

-- Remove punctuation from text String.
removePuncc :: String -> String
removePuncc xs = [x | x <- xs, not (x `elem` ",+?!:;-\\\"\'")]


--give ticker, sheet , term  returns url String
--ticker
mwUrl::String->String->String->String
mwUrl a "b" "q" = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/balance-sheet/quarter"
mwUrl a "i" "q" = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials//income/quarter"                                 
mwUrl a "c" "q" = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/cash-flow/quarter"
mwUrl a "b" _ = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/balance-sheet"
mwUrl a "i" _ = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials"                                 
mwUrl a "c" _ = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/cash-flow"

--cUrl

cUrl::String
cUrl = "https://coinmarketcap.com/all/views/all/"


---give ticker name and it returns url String
yfUrl::String->String
yfUrl a = "https://finance.yahoo.com/quote/" ++ a

yfUrlStat::String->String
yfUrlStat a = "https://finance.yahoo.com/quote/" ++ a ++ "/key-statistics?p="

--download html 
downloadHtml :: String->Bool->Int-> IO  String
downloadHtml a b n = do
    request <- parseRequest a 
    let request' = request {requestHeaders = [(HTypes.hUserAgent, C8.pack "HTTP-Conduit")]}
   
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings
    res <- httpLbs request' manager 
    
    let justText =  (fromFooter  $ parseTags $ show res)

    --putStrLn $ show $ take 3000 justText
    if b == True then
      return $ show $ take (n * 3 )justText 
    else
      return  $ show $ justText
     
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

--process Ticker
processTicker::String->String
processTicker a | "^" `isPrefixOf` a =(map toUpper ("\\"++a))
processTicker a = (map toUpper a)


--marketList
marketList::String
marketList = "(Nasdaq|SNP|NYSE"
               ++"|NasdaqGS|AMEX|Cboe|DJI"
               ++"|Chicago|CBOT|COMEX|CCY"
               ++"|NY Mercantile|CCC|Osaka"
               ++"|HKSE|ASX|ICE|CME|SES|AXS)"

--do regex on yahoo web string      
parseHtml::String ->String->Maybe StockData
parseHtml a tick = 
    let priceList =  a=~"Currency in (USD|USX|JPY|HKD|SGD|AUD)((\\d+,)*?\\d+.\\d+)((-|\\+)(\\d+,)?\\d+.\\d+)\\s+\\(((-|\\+)\\d+.\\d+)%\\)"::[[String]]
        companyName = a=~((processTicker tick)++"\\s+-\\s+(.*?)"++marketList++".*?Currency")::[[String]]
        dividend=  a=~"Dividend & Yield\\d+.\\d+\\s+\\((\\d+.\\d+%)\\)"::[[String]]
        marketCap =  a=~"Market Cap(\\d+\\.\\d+(B|T|M))"::[[String]]
        afterHours =   a=~"\\(((-|\\+)\\d+.\\d+)%\\)After hours"::[[String]]
        peRatio = a=~"PE\\s+Ratio\\s+\\(TTM\\)(\\d+.\\d+)"::[[String]]
        weeksChange = a=~"52-Week\\s+Change\\s+3(\\d+\\.\\d+)%"::[[String]]
        beta        = a=~"Beta\\s+\\(5Y Monthly\\)\\s+(\\d+\\.\\d+)52"::[[String]]

    in  if companyName==[]  || priceList== [] then
        Nothing
        else 
        Just(StockData{ companyName = (formatSpace $ removePuncc $ listToString companyName  1) 
                       ,ticker = tick
                       ,price = (listToString priceList  2)
                       ,percentageChange = (listToString priceList 7)
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
  str' <- downloadHtml  (yfUrl tick) True 1000
  


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
  str' <- downloadHtml  (yfUrl tick) True 1000
  str1' <- downloadHtml  (yfUrlStat tick) True 1000


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
       str1 <- downloadHtml  (mwUrl tick "b"  quarter) False 10000
       str2 <- downloadHtml  (mwUrl tick "c"  quarter) False 10000
       str3 <- downloadHtml  (mwUrl tick "i"  quarter) False 10000
        
       let test = parseSheetHtml $ removePuncNR ( str1 ++ str2 ++ str3)
      
       --putStrLn  $ show $ removePuncNR str3
       return $ replaceStuffs c $ flattenSheet test


columnParse::String
columnParse = "\\s+(\\s+\\(?(\\d+.\\d+)|(\\d)?(T|B|M|K)\\)?)+"

--Parse balance sheet data
parseSheetHtml::String->[[String]]
parseSheetHtml a = 
                let totalAsset =a =~("(Total\\s+Assets)" ++ columnParse) ::[[String]]
                    totalLiabilities = a =~ ("(Total\\s+Liabilities)"++columnParse) ::[[String]] 
                    freeCashflow = a =~ ("(Fee\\s+Cash\\s+Flow)"++columnParse) ::[[String]] 
                    opCashflow  = a =~ ("(Net\\s+Opeatig\\s+Cash\\s+Flow)"++columnParse) ::[[String]] 
                    revenue  = a =~ ("(Reveue)"++columnParse) ::[[String]] 
                    netIncome  = a =~ ("(Net\\s+Icome)"++columnParse) ::[[String]] 
                    interstIncome = a =~ ("(Iteest\\s+Icome)"++columnParse) ::[[String]] 
                    
               in 
                  revenue
                ++[if length (interstIncome) > 0 && length (revenue) == 0 then   interstIncome !! 0 else [] ]
                ++ [if length (netIncome) > 0 then   netIncome !! 0 else []]
                ++ opCashflow 
                ++ freeCashflow 
                ++totalAsset 
                ++ totalLiabilities 
--Flatten List
flattenSheet::[[String]]->[String]
flattenSheet [] = []
flattenSheet (x:xs) =    [if length x>0 then formatSpace $ x !! 0 else []] ++  flattenSheet xs

--Replace Items In GetSheet                                   
replaceStuffs::String->[String]->[String]
replaceStuffs c [] = []
replaceStuffs c (x:xs)| "Iteest Icome" `isInfixOf` x = [replace "Iteest Icome" ("PRIVMSG:"++c++" :Interest Income") x] ++ replaceStuffs c xs 
replaceStuffs c (x:xs)| "Reveue" `isInfixOf` x = [replace "Reveue" ("PRIVMSG:"++c++" :Revenue") x] ++ replaceStuffs c xs   
replaceStuffs c (x:xs)| "Net Opeatig Cash Flow" `isInfixOf` x = [replace "Net Opeatig Cash Flow" ("PRIVMSG:"++c++" :Net Operatig Cash Flow") x] ++ replaceStuffs c xs          
replaceStuffs c (x:xs)| "Fee Cash Flow" `isInfixOf` x = [replace "Fee Cash Flow" ("PRIVMSG:"++c++" :Free Cash Flow") x] ++ replaceStuffs c xs
replaceStuffs c (x:xs)| "Net Icome" `isInfixOf` x = [replace "Net Icome" ("PRIVMSG:"++c++" :Net Income") x] ++ replaceStuffs c xs
replaceStuffs c (x:xs) = [("PRIVMSG:"++c++" :"++x)] ++ replaceStuffs c xs

--Parse Crypto
parseCrypto::String->String->Maybe StockData 
parseCrypto a s = let b =s =~((map toUpper a) 
                       ++"(\\w+(\\s+\\w+)?)"
                       ++(map toUpper a)
                       ++"\\$.*?\\$.*?(\\$(\\d+\\,)?\\d+\\.\\d(\\.+\\d)?\\d).*?"
                       ++(map toUpper a)
                       ++".*?(-?\\d+\\.\\d+)\\%(-?\\d+\\.\\d+)\\%(-?\\d+\\.\\d+)\\%")
                       in if b == [] then
                        Nothing
                        else
                          Just (StockData{companyName=listToString b  1 
                                          ,ticker =a 
                                          ,price = listToString b  3 
                                          ,percentageChange= if ("-" `isPrefixOf`  (listToString b  7)) then (listToString b  7) else "+"++(listToString b  7)
                                          ,dividend=listToString b  2 
                                          ,marketCap=listToString b  2 
                                          ,peRatio=listToString b  2 
                                          ,afterHours=listToString b  2
                                          ,weeksChange=listToString b 2
                                          ,beta=listToString b 2 })

getCrypto::String->IO StockData
getCrypto tick  = do
  str' <- downloadHtml  cUrl False 4000
  case parseCrypto tick (str')of
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
     Just str'' -> pure str''