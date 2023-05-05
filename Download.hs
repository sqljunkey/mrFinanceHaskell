
module Download where 

import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import Text.HTML.TagSoup
import Text.Regex.PCRE
import Text.Read

--give ticker, sheet , term  returns url String
--ticker
--"balanace, income, cashflow"
--"quarterly, yearly"
--formMarketWatchUrl::String->String->String->String
--formMarketWatchUrl a b c = let link = "https://www.marketwatch.com/investing/stock/" ++ a 
--                                        if b == "balance" 
--                                             link ++ " /financials/balance-sheet/"
 --                                       else if b== "income"
 --                                           link ++ ""
                                        

---give ticker name and it returns url String
formYahooFinanceUrl::String->String
formYahooFinanceUrl a = "https://finance.yahoo.com/quote/" ++ a

--download html 
downloadHtml :: String-> IO  String
downloadHtml a = do
    request <- parseRequest a 
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings
    res <- httpLbs request manager
    
    let justText =  (fromFooter  $ parseTags $ show res)
    --putStrLn $ show justText
    
    return $ show justText 
     
        where fromFooter =  innerText  .take 2000 . dropWhile (~/= "<body>")
     
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
  str' <- downloadHtml $ formYahooFinanceUrl tick
  case parseHtml str' tick of
    Nothing -> pure StockData{companyName ="" , 
                       ticker = tick,
                       price = "", 
                       percentageChange = "",
                       dividend= "" , 
                       marketCap = "",
                       afterHours ="" }-- from f
    Just str'' -> pure str'' -- from g if it exists

 
