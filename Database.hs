--Database

import Control.Exception
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)



account::String
account = "host=localhost port=5432 dbname=test user=mrfinance password=%v.8DwUp$Ha(X/YQ"

query_users::IO
query_users =  catch (do
                       c <- connectPostgreSQL account

  
                       select <- prepare c "SELECT * FROM user_accounts;"


  
                       execute select []


                       result <- fetchAllRows select
                       putStrLn $ show result


                       commit c


                       disconnect c
                       return ()
                 )handler
        where 
          handler :: SqlError -> IO ()
          handler err = print $ "Oh no: " ++ show err
