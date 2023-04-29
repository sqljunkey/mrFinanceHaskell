module Index where
import Download

--Open Files
openFile::String->IO [String]
openFile a = do 
            com<-readFile a 
            let stock =  lines com
            a<- accumulatePrice stock
            print (a/ fromIntegral (length stock))
            return $ lines com


--Accumulate Price
accumulatePrice::[String]->IO Double
accumulatePrice [] = return 0.0
accumulatePrice (x:xs) = do 
                           (a,b,c,d) <- getTicker x
                           let f = read c :: Double
                           s <- accumulatePrice xs
                           return (f + s)

--Presets
presetIndex::String->IO String
presetIndex "hammond" = return "IntC Price,Change" 


