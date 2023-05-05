
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


--do regex on yahoo web string      
parseHtml::String ->String->Maybe StockData
parseHtml a tick = 
    let priceList = a=~"Currency\\s+in(.+?)((\\d+,)?\\d+\\.\\d+)((-|\\+)?\\d+\\.\\d+)\\s+\\(((-|\\+)?\\d+\\.\\d+)\\%\\)"::[[String]]
        
        companyName = a=~"(Frame'\\);}(.*))\\((.*)\\)(.*?)Currency"::[[String]]
        dividend= a=~"Dividend & Yield(\\d+.\\d+)"::[[String]]
        marketCap = a=~"Market Cap(\\d+\\.\\d+(B|T|M))"::[[String]]
        afterHours = a=~"\\(((-|\\+)\\d+.\\d+)%\\)After hours"::[[String]]
        price =   (priceList !! 0 !! 2)
        percentage =   (priceList !! 0 !! 6) 

    in  if companyName==[] || priceList== [] then
        Nothing
        else 
        Just(StockData{companyName =(companyName !! 0 !! 3) , 
                       ticker = tick,
                       price = price, 
                       percentageChange = percentage,
                       dividend= (dividend !! 0 !! 1) , 
                       marketCap = (marketCap !! 0 !! 1),
                       afterHours =(afterHours !! 0 !! 1) })


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

 
