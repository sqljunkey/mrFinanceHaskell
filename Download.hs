
module Download where 

import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import Text.HTML.TagSoup
import Text.Regex.PCRE
import Text.Read

-- Remove punctuation from text String.
removePuncNR :: String -> String
removePuncNR xs = [x | x <- xs, not (x `elem` ",+?!:;nr\\\"\'")]



--give ticker, sheet , term  returns url String
--ticker
formMWUrl::String->String->String->String
formMWUrl a "b" "q" = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/balance-sheet/quarter"
formMWUrl a "i" "q" = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials//income/quarter"                                 
formMWUrl a "c" "q" = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/cash-flow/quarter"
formMWUrl a "b" _ = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/balance-sheet"
formMWUrl a "i" _ = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials"                                 
formMWUrl a "c" _ = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/cash-flow"




---give ticker name and it returns url String
formYahooFinanceUrl::String->String
formYahooFinanceUrl a = "https://finance.yahoo.com/quote/" ++ a

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
        dividend=  a=~"Dividend & Yield(\\d+.\\d+)"::[[String]]
        marketCap =  a=~"Market Cap(\\d+\\.\\d+(B|T|M))"::[[String]]
        afterHours =   a=~"\\(((-|\\+)\\d+.\\d+)%\\)After hours"::[[String]]


   

    in  if companyName==[] || priceList== [] then
        Nothing
        else 
        Just(StockData{ companyName = (listToString companyName  2) , 
                       ticker = tick,
                       price = (listToString priceList  2), 
                       percentageChange = (listToString priceList 6),
                       dividend= (listToString dividend  1) , 
                       marketCap = (listToString marketCap  1),
                       afterHours =(listToString afterHours 1) })


    --(ticker, co name, price, percentage, div  )
getTicker :: String -> IO StockData
getTicker tick = do
  str' <- downloadHtml  (formYahooFinanceUrl tick) 2000
  case parseHtml str' tick of
    Nothing -> pure StockData{companyName ="" , 
                       ticker = tick,
                       price = "", 
                       percentageChange = "",
                       dividend= "" , 
                       marketCap = "",
                       afterHours ="" }-- from f
    Just str'' -> pure str'' -- from g if it exists

 
--(get Rev, Cash,Debt,Asset )
getSheets::String->String-> IO [[String]]
getSheets tick quarter= do
       str1 <- downloadHtml  (formMWUrl tick "b"  quarter) 10000
       str2 <- downloadHtml  (formMWUrl tick "c"  quarter) 10000
       str3 <- downloadHtml  (formMWUrl tick "i"  quarter) 10000
        
       let test = parseSheetHtml $ removePuncNR ( str1 ++ str2 ++ str3)
       putStrLn $  test !! 0 !! 0
       -- putStrLn $ removePuncNR str3
       return $ test

parseSheetHtml::String->[[String]]
parseSheetHtml a = 
                let totalAsset =a =~"(Total\\s+Assets)(\\s+(\\d+.\\d+)(B|M|K))+" ::[[String]]
                    totalLiabilities = a =~ "(Total\\s+Liabilities)(\\s+(\\d+.\\d+)(B|M|K))+" ::[[String]] 
                    freeCashflow = a =~ "(Fee\\s+Cash\\s+Flow)(\\s+(\\d+.\\d+)(B|M|K))+" ::[[String]] 
                    opCashflow  = a =~ "(Net\\s+Opeatig\\s+Cash\\s+Flow)(\\s+(\\d+.\\d+)(B|M|K))+" ::[[String]] 
                    revenue  = a =~ "(Reveue)(\\s+(\\d+.\\d+)(B|M|K))+" ::[[String]] 
                    netIncome  = a =~ "(Net\\s+Icome)(\\s+(\\d+.\\d+)(B|M|K))+" ::[[String]] 
                    interstIncome = a =~ "(Iteest\\s+Icome)(\\s+(\\d+.\\d+)(B|M|K))+" ::[[String]] 
                    
               in 
                netInterstIncome