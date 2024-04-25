
module Download where 

import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import Text.HTML.TagSoup
import Text.Regex.TDFA
import Text.Read
import Data.List.Utils
import Data.List
import Data.List.Split
import Data.Char

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified Network.HTTP.Types as HTypes
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8

-- Remove punctuation from text String.
removePuncNR :: String -> String
removePuncNR xs = [x | x <- xs, not (x `elem` ",+?!:;nr\'\n'")]

removeNewLine :: String -> String
removeNewLine xs = [x | x <- xs, not (x `elem` "'\n'")]

-- Remove punctuation from text String.
removePuncc :: String -> String
removePuncc xs = [x | x <- xs, not (x `elem` ",+?!:;-\\\"\'")]


--give ticker, sheet , term  returns url String
--ticker
mwUrl::String->String->String->String
mwUrl a "b" "q" = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/balance-sheet/quarter"
mwUrl a "i" "q" = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/income/quarter"                                 
mwUrl a "c" "q" = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/cash-flow/quarter"
mwUrl a "b" _ = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/balance-sheet"
mwUrl a "i" _ = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials"                                 
mwUrl a "c" _ = "https://www.marketwatch.com/investing/stock/" ++ a ++ "/financials/cash-flow"

--cUrl

cUrl::String
cUrl = "https://coinmarketcap.com/"


---give ticker name and it returns url String
yfUrl::String->String
yfUrl a = "https://finance.yahoo.com/quote/" ++ a

yfUrlStat::String->String
yfUrlStat a = "https://finance.yahoo.com/quote/" ++ a ++ "/key-statistics?p="++ a


get_tags :: String -> Int -> String
get_tags html n = take n $ innerText $ dropWhile (~/= "<body>") $ parseTags html


--download html 
downloadHtml :: String->Int-> IO  String
downloadHtml a n = do
    request <- parseRequest a 
    let request' = request {requestHeaders = [(HTypes.hUserAgent, C8.pack "HTTP-Conduit")]}
   
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings
    res <- httpLbs request' manager 
    
                       

    putStrLn $ (L8.unpack $ responseBody res) 

    return $ get_tags (L8.unpack $ responseBody res) n



--download html
download_raw_html :: String->Int-> IO  String
download_raw_html a n = do
    request <- parseRequest a
    let request' = request {requestHeaders = [(HTypes.hUserAgent, C8.pack "HTTP-Conduit")]}

    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings
    res <- httpLbs request' manager



    --putStrLn $ (L8.unpack $ responseBody res)

    return $ (L8.unpack $ responseBody res) 





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
                     , beta ::String
                     , volume::String
                     , avgVolume::String 
                     , priceSale::String
                    } deriving (Show)


--convert list to string
listToString::[[String]]->Int->String
listToString [] _ = ""
listToString l d =  if length (l) > 0 && length (l !! 0) > d  
                    then 
                        (l !! 0 !! d)
                    else
                        ""
lts::[String]->Int->String->String
lts [] _ _= ""
lts l d c=  if length (l) > d  
                    then 
                        ((l !! d)++ c)
                    else
                        ""
                  

--add ^ to ticker
processTicker::String->String
processTicker a | "^" `isPrefixOf` a =(map toUpper ("\\"++a))
processTicker a = (map toUpper a)


--marketList regex match
marketList::String
marketList = "(Nasdaq|SNP|NYSE|Other OTC"
               ++"|NasdaqGS|AMEX|Cboe|DJI"
               ++"|Chicago|CBOT|COMEX|CCY"
               ++"|NY Mercantile|CCC|Osaka"
               ++"|HKSE|ASX|ICE|CME|SES|AXS|Stockholm"
               ++"|Shanghai)"

--currency list regex match
currencyList::String
currencyList = "(USD|CAD|USX|JPY|HKD|SGD|AUD|SEK|MXN|CNY|TRY)"



--do regex on yahoo html string      
parseHtml::String ->String->Maybe StockData
parseHtml a tick = 
    let priceList =  a=~("<current_price>(.*)</current_price>")::[[String]]
        companyName = a=~("<long_name>(.*)</long_name>")::[[String]]
        percentage = a=~("<percentage>(.*)</percentage>")::[[String]]
        dividend=  a=~ ("<dividend_yield>(.*)</dividend_yield>")::[[String]]
        marketCap =  a=~("<market_cap>(.*)</market_cap>")::[[String]]
        afterHours =   a=~"\\(((-|\\+)?[0-9]+.[0-9]+)%\\)After hours"::[[String]]
        peRatio = a=~("<p_e_ratio>(.*)</p_e_ratio>")::[[String]]
        weeksChange = a=~("<week_52_range>(.*)</week_52_range>")::[[String]]
        beta        = a=~("<beta>(.*)</beta>")::[[String]]
        volume      = a=~("<volume>(.*)</volume>")::[[String]]
        avgVolume   = a=~("<average_volume>(.*)</average_volume>")::[[String]]
        priceSale   = a=~("<price_sales>(.*)</price_sales>")::[[String]]
       

    in  if priceList== [] || companyName == [] then
        Nothing
        else 
        Just(StockData{ companyName = ( listToString companyName  1) 
                       ,ticker = tick
                       ,price = (listToString priceList  1)
                       ,percentageChange = (listToString percentage 1)
                       ,dividend=(listToString dividend  1) 
                       ,marketCap =(listToString marketCap  1)
                       ,afterHours =(listToString afterHours 1)
                       ,peRatio =(listToString peRatio 1) 
                       ,weeksChange =(listToString weeksChange 1)
                       ,beta =(listToString beta 1)
                       ,volume =(listToString volume 1)
                       ,avgVolume=(listToString avgVolume 1)
                       ,priceSale=( listToString priceSale 1)
                       
                        })


--get ticker with simple information
getTicker :: String -> IO StockData
getTicker tick  = do
  
  str1' <- download_raw_html ("http://127.0.0.1:9092/"++tick) 300000
  case parseHtml (str1') tick of
    Nothing -> pure StockData{companyName ="" 
                       ,ticker = tick
                       ,price = ""
                       ,percentageChange = ""
                       ,dividend= ""  
                       ,marketCap = ""
                       ,peRatio =""
                       ,afterHours ="" 
                       ,weeksChange=""
                       ,beta=""
                       ,volume =""
                       ,avgVolume=""
                       ,priceSale=""
                       }-- from f
    Just str'' -> pure str'' -- from g if it exists

    --get ticker with extended information 
getTickerStat :: String -> IO StockData
getTickerStat tick  = do
  
  str1'  <- download_raw_html ("http://127.0.0.1:9092/"++tick) 300000


  case parseHtml ( str1') tick of
    Nothing -> pure StockData{companyName ="" 
                       ,ticker = tick
                       ,price = ""
                       ,percentageChange = ""
                       ,dividend= ""  
                       ,marketCap = ""
                       ,peRatio =""
                       ,afterHours ="" 
                       ,weeksChange=""
                       ,beta=""
                       ,volume=""
                       ,avgVolume=""
                       ,priceSale=""
                       }-- from f
    Just str'' -> pure str'' -- from g if it exists

--Reduce Spaces
formatSpace :: String -> String
formatSpace = foldr go ""
  where
    go x acc = x:if x == ' ' then dropWhile (' ' ==) acc else acc

--get type
getType::String->IO String 
getType tick = do 
              str1 <- download_raw_html ("http://127.0.0.1:9091/"++tick) 300000
             
             
           
              let final = str1 =~ ("<type>"
                                  ++"(.*)"
                                  ++ "</type>")::[[String]]

	      
	      let s =  listToString final 1
            
              

              
              return $ s



--(get Rev, Cash,Debt,Asset )
getSheets::String->String->String-> IO [String]
getSheets nick tick quarter= do
       str1 <- download_raw_html ("http://127.0.0.1:9091/"++tick) 300000
        
       let html = parseSheetHtml   str1

       return $ formatSheets nick $ firstElement html


--space matches because the regex lib is faulty
space::String
space ="(\\t|\\n|\\r|\\v|\\f| )+"

--number matches
numbers::String
numbers = "("++space++"\\(?([0-9]+|[0-9]+.[0-9]+)+(B|M|K|T)\\)?)+"

--number with percentage matches
percentage::String
percentage = "("++space++"-?([0-9]+|[0-9]+.[0-9]+)?(%|-)?)+"

--use Regex to parseSheet
parseSheetHtml::String->[[String]]
parseSheetHtml a = 
                let totalAsset =a =~("<total_asset>(.*)</total_asset>") :: [[String]]
                    totalLiabilities = a =~ ("<total_debt>(.*)</total_debt>") ::[[String]] 
                    freeCashflow = a =~("<free_cashflow>(.*)</free_cashflow>") ::[[String]] 
                    opCashflow  = a =~ ("<cashflow>(.*)</cashflow>") ::[[String]] 
                    revenue  = a =~ ("<revenue>(.*)</revenue>") ::[[String]] 
                    netIncome  = a =~ ("<net_income>(.*)</net_income>") ::[[String]]
                    year =      a =~("<years>(.*)</years>")::[[String]]
              
               in
                  year
                 ++revenue 
                 ++netIncome 
                 ++opCashflow
                 ++freeCashflow 
                 ++totalAsset
                 ++ totalLiabilities
                
--get First element
firstElement::[[String]]->[String]
firstElement [] = []
firstElement (x:xs) =    [if length x>0 then  x !! 1 else []] ++  firstElement xs

--add PrivMSG to Rows                                  
formatSheets :: String -> [String] -> [String]
formatSheets nick [] = []
formatSheets nick (x:xs)
    | null x = formatSheets nick xs
    | otherwise = ("PRIVMSG:" ++ nick ++ " :" ++ x) : formatSheets nick xs

--Parse Crypto
parseCrypto::String->String->Maybe StockData 
parseCrypto a s = let b =s =~("(([A-Z]+)?([a-z]+)?)+"
                       ++(map toUpper a)
                       ++"(\\$(([0-9]+),)?[0-9]+.[0-9][0-9])"
                       ++"(-?[0-9]+.[0-9][0-9])\\%"
                       ++"(-?[0-9]+.[0-9][0-9])\\%")
                      
                       in if b==[] then
                        Nothing
                        else
                          Just (StockData{companyName=listToString b  1 
                                          ,ticker =a 
                                          ,price = listToString b  4
                                          ,percentageChange= listToString b 8
                                          ,dividend=listToString b  2 
                                          ,marketCap=listToString b  2 
                                          ,peRatio=listToString b  2 
                                          ,afterHours=listToString b  2
                                          ,weeksChange=listToString b 2
                                          ,beta=listToString b 2 
                                          ,volume = listToString b 2
                                          ,avgVolume=listToString b 2
                                          ,priceSale=listToString b 2
                                          })
--download crypto data
getCrypto::String->IO StockData
getCrypto tick  = do
  str' <- downloadHtml  cUrl 5000
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
                       ,beta=""
                       ,volume=""
                       ,avgVolume=""
                       ,priceSale=""
                       }-- from f
     Just str'' -> pure str''
