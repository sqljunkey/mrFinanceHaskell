--Database

import Control.Exception
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)

main =  catchDyn (do
                       c <- connectPostgreSQL "host=localhost dbname=testdb user=mrfinance password=qwerty"

    state <- prepare c "INSERT INTO testtable values (?,?);"
    select <- prepare c "SELECT * FROM testtable;"


    execute state [toSql "muhmuh", toSql (40::Int)]
    execute select []


    result <- fetchAllRows select
    putStrLn $ show result


   commit c


    disconnect c
    return ()
)
handler where 
        handler :: SqlError -> IO ()
        handler err = print $ "Oh no: " ++ show err
