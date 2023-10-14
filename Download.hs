
module Download where 

import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import Text.HTML.TagSoup
--import Text.Regex.PCRE
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
    
                       

    putStrLn $ get_tags (L8.unpack $ responseBody res) n

    return $ get_tags (L8.unpack $ responseBody res) n






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
                  

--process Ticker
processTicker::String->String
processTicker a | "^" `isPrefixOf` a =(map toUpper ("\\"++a))
processTicker a = (map toUpper a)


--marketList
marketList::String
marketList = "(Nasdaq|SNP|NYSE|Other OTC"
               ++"|NasdaqGS|AMEX|Cboe|DJI"
               ++"|Chicago|CBOT|COMEX|CCY"
               ++"|NY Mercantile|CCC|Osaka"
               ++"|HKSE|ASX|ICE|CME|SES|AXS|Stockholm"
               ++"|Shanghai)"

--currency
currencyList::String
currencyList = "(USD|CAD|USX|JPY|HKD|SGD|AUD|SEK|MXN|CNY|TRY)"



extractNumberFromText :: String -> String
extractNumberFromText text = go text ""
  where
    go [] result = reverse result
    go (c:cs) result
        | isDigit c = go cs (c : result)
        | c == '.' && not (null result) = goDecimal cs (c : result)
        | otherwise = go cs result

    goDecimal cs result = goDecimal' cs 2 (result)
    goDecimal' cs 0 result = reverse result
    goDecimal' [] _ result = reverse result
    goDecimal' (c:cs) n result
        | isDigit c = goDecimal' cs (n - 1) (c : result)
        | otherwise = reverse result






--do regex on yahoo web string      
parseHtml::String ->String->Maybe StockData
parseHtml a tick = 
    let priceList =  a=~("(([0-9]+,)?[0-9]+.[0-9]+)((-|\\+)?([0-9]+,)?[0-9]+.[0-9]+)"
                        ++space
                        ++"\\(((-|\\+)?[0-9]+.[0-9]+)%\\)")::[[String]]
        companyName = a=~("(([A-Z]|[a-z]|\\.|,|\\^|\\&|'|"
                        ++space
                        ++")+)\\("++ processTicker tick ++"\\)")::[[String]]
        dividend=  a=~ ("Dividend & Yield[0-9]+\\.[0-9]+"
                      ++space
                      ++"\\(([0-9]+\\.[0-9]+\\%)\\)")::[[String]]
        marketCap =  a=~"Market Cap([0-9]+\\.[0-9]+(B|T|M))"::[[String]]
        afterHours =   a=~"\\(((-|\\+)?[0-9]+.[0-9]+)%\\)After hours"::[[String]]
        peRatio = a=~("PE"
                    ++space
                    ++"Ratio"
                    ++space
                    ++"\\(TTM\\)([0-9]+.[0-9]+)")::[[String]]
        weeksChange = a=~("52"
                    ++space
                    ++"Week"
                    ++space
                    ++"Range(([0-9]+,)?[0-9]+.[0-9]+"
                    ++space
                    ++"-"
                    ++space
                    ++"([0-9]+,)?[0-9]+.[0-9]+)")::[[String]]
        beta        = a=~("Beta"
                    ++space
                    ++"\\(5Y Monthly\\)([0-9]+\\.[0-9]+)")::[[String]]
        volume      = a=~"Volume(([0-9]+,)+[0-9]+)"::[[String]]
        avgVolume   = a=~"Avg. Volume(([0-9]+,)+[0-9]+)"::[[String]]
        priceSale   = a=~("Price\\/Sales"
                        ++space
                        ++"\\(ttm\\)([0-9]+.[0-9][0-9])")::[[String]]
       

    in  if priceList== [] || companyName == [] then
        Nothing
        else 
        Just(StockData{ companyName = ( listToString companyName  1) 
                       ,ticker = tick
                       ,price = (listToString priceList  1)
                       ,percentageChange = (listToString priceList 7)
                       ,dividend= (listToString dividend  2) 
                       ,marketCap = (listToString marketCap  1)
                       ,afterHours =(listToString afterHours 1)
                       ,peRatio =(listToString peRatio 3) 
                       ,weeksChange = (listToString weeksChange 3)
                       ,beta =(listToString beta 2)
                       ,volume =(listToString volume 1)
                       ,avgVolume=(listToString avgVolume 1)
                       ,priceSale=( listToString priceSale 2)
                       
                        })


--get ticker with simple information
getTicker :: String -> IO StockData
getTicker tick  = do
  str' <- downloadHtml  (yfUrl tick) 6000
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
                       ,beta=""
                       ,volume =""
                       ,avgVolume=""
                       ,priceSale=""
                       }-- from f
    Just str'' -> pure str'' -- from g if it exists

    --get ticker with extended information 
getTickerStat :: String -> IO StockData
getTickerStat tick  = do
  str' <- downloadHtml  (yfUrl tick) 6000
  str1' <- downloadHtml  (yfUrlStat tick) 6000


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
              str1 <- downloadHtml ("https://www.marketwatch.com/investing/stock/"++tick) 30000
              --putStrLn  $ show  str1

              let final = str1 =~("(([A-Z]|[a-z]|\\.|\\d|\\s|,|-|\\&|\\')+"
                                  ++"(explores|produces|provides|engaged|engages|operates)"
                                  ++"(.*?)\\.)")::[[String]]
              let s = drop 1 $formatSpace $drop 1 $ getTypeF final
            
              let m = splitOn "." s
              
              return ((lts m 0 ".")++ (lts m 1 "."))

--function
getTypeF::[[String]]->String
getTypeF [] = ""
getTypeF (x:xs) | "\\" `isInfixOf`  (lts x 0 "")  = getTypeF xs
getTypeF  (x:xs) =  lts x 1 ""

--(get Rev, Cash,Debt,Asset )
getSheets::String->String->String-> IO [String]
getSheets nick tick quarter= do
       str1 <- downloadHtml  (mwUrl tick "b"  quarter) 5000000
       str2 <- downloadHtml  (mwUrl tick "c"  quarter) 5000000
       str3 <- downloadHtml  (mwUrl tick "i"  quarter) 5000000
        
       let html = parseSheetHtml $formatSpace $ removeNewLine $  str1++ str2 ++ str3
      
       putStrLn $ show html

       --putStrLn  $ show $ removePuncNR str3
       return $ formatSheets nick $ firstElement html

space::String
space ="(\\t|\\n|\\r|\\v|\\f| )+"

numbers::String
numbers = "("++space++"\\(?([0-9]+|[0-9]+.[0-9]+)+(B|M|K|T)\\)?)+"

percentage::String
percentage = "("++space++"-?([0-9]+|[0-9]+.[0-9]+)?(%|-)?)+"


parseSheetHtml::String->[[String]]
parseSheetHtml a = 
                let totalAsset =a =~("Total"++space++"Assets"++numbers) :: [[String]]
                    totalLiabilities = a =~ ("Total"++space++"Liabilities"++numbers) ::[[String]] 
                    freeCashflow = a =~ ("Free"++space++"Cash"++space++"Flow"++numbers) ::[[String]] 
                    opCashflow  = a =~ ("Net"++space++"Operating"++space++"Cash"++space++"Flow"++numbers) ::[[String]] 
                    revenue  = a =~ ("Revenue"++numbers) ::[[String]] 
                    netIncome  = a =~ ("(Net"++space++"Income)"++numbers) ::[[String]] 
                    interstIncome = a =~ ("Interest"++space++"Income"++numbers) ::[[String]] 
                    cogs = a =~ ("Gross"++space++"Income"++space++"Growth"++percentage) ::[[String]] 
               in 
                  revenue
                 ++[if length (interstIncome) > 0 && length (revenue) == 0 then   interstIncome !! 0 else [] ]
                 ++ [if length (netIncome) > 0 then   netIncome !! 0 else []]
                 ++ cogs
                 ++opCashflow
                 ++freeCashflow 
                 ++totalAsset
                 ++ totalLiabilities
                

firstElement::[[String]]->[String]
firstElement [] = []
firstElement (x:xs) =    [if length x>0 then formatSpace $ x !! 0 else []] ++  firstElement xs

                                  
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