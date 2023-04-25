
module Download where 

import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import Text.HTML.TagSoup
import Text.Regex.PCRE
import Text.Read

--download html from yahoo
downloadHtml :: String-> IO  String
downloadHtml tick = do
    request <- parseRequest ("https://finance.yahoo.com/quote/" ++ tick )
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    manager <- newManager settings
    res <- httpLbs request manager
    
    let justText =  (fromFooter  $ parseTags $ show res)
    --putStrLn $ show justText
    
    return $ show justText 
     
        where fromFooter =  innerText  .take 1000 . dropWhile (~/= "<body>")
     
  
--do regex on yahoo web string      
parseHtml::String ->String->Maybe (String,String, String, String)
parseHtml a tick = 
    let priceList = a=~"Currency\\s+in(.+?)((\\d+,)?\\d+\\.\\d+)((-|\\+)?\\d+\\.\\d+)\\s+\\(((-|\\+)?\\d+\\.\\d+)\\%\\)"::[[String]]
        price =   (priceList !! 0 !! 2)
        percentage =   (priceList !! 0 !! 6) 
        companyName = a=~"(;}(.*);}(.*))\\((.*)\\)(.*?)Currency"::[[String]]
    
    in  if companyName==[] || priceList== [] then
        Nothing
        else 
        Just((tick,(companyName !! 0 !! 3), price , percentage ))


    --(ticker, co name, price, percentage, div  )
getTicker :: String -> IO (String,String, String, String)
getTicker tick = do
  str' <- downloadHtml tick
  case parseHtml str' tick of
    Nothing -> pure (tick,"", "", "")-- from f
    Just str'' -> pure str'' -- from g if it exists

 
