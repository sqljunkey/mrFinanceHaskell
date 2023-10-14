-- File 4.hs
module Test where 

import Text.Regex.PCRE
import Control.Exception              -- base
import Control.Monad.IO.Class         --
import Control.Concurrent (threadDelay)
import Control.Lens
import Data.List                      --
import Data.List.Split
import System.Exit                    --
import System.IO                      --
import qualified Network.Socket as N  -- network
import Control.Monad.Trans.Reader     -- transformers
import Command
import Data.Foldable

import Control.Monad.State



-- Configuration options
myServer = "irc.libera.chat" :: String
myPort   = 6667 :: N.PortNumber
myChan   = "##investments" :: String
myNick   = "mrfinance" :: String


-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
data Bot = Bot { botSocket :: Handle }
type Net = ReaderT Bot IO

-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
    h <- connectTo myServer myPort
    return (Bot h)
  where
    notify a = bracket_
      (putStrLn ("Connecting to " ++ myServer ++ " ...") >> hFlush stdout)
      (putStrLn "done.")
      a

-- Connect to the server and return a Handle (helper for connect)
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
    addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do



    write "NICK" myNick
    write "USER" (myNick ++ " 0 * :mrfinance")
    write "PRIVMSG" "nickserv :id qwerty"
    liftIO (threadDelay (10000 * 1000))
    write "JOIN" "##investments"
    write "JOIN" "##econometrics"
    write "JOIN" "#materia"
    write "JOIN" "#bitcoin-pricetalk"
    write "JOIN" "##economics"
   
    
    listen

-- Send a message to the server we're currently connected to
write :: String -> String -> Net ()
write cmd args = do
    h <- asks botSocket
    let msg = cmd ++ " " ++ args ++ "\r\n"
    liftIO $ hPutStr h msg          -- Send message on the wire
    liftIO $ putStr ("> " ++ msg)   -- Show sent message on the command line

-- Process each line from the server
listen :: Net ()
listen = forever $ do
    h <- asks botSocket
    line <- liftIO $ hGetLine h
    liftIO (putStrLn line)
    let s = init line
    let c = getChannel s


    
    if isPing s then pong s else evalList (command c ( splitOn " " (clean s)))
  where
    forever :: Net () -> Net ()
    forever a = do a; forever a

    clean :: String -> String
    clean = drop 1 . dropWhile (/= ':') . drop 1

    isPing :: String -> Bool
    isPing x = "PING :" `isPrefixOf` x

    pong :: String -> Net ()
    pong x = write "PONG" (':' : drop 6 x)
             

eval :: String -> Net ()
eval  a = do
--  s <-liftIO a
  
  let action | a == "!quit" = write "QUIT" ":Exiting" >> liftIO exitSuccess
             | "PRIVMSG:" `isPrefixOf` a =write "PRIVMSG"  (drop 8 a)
             | otherwise = return ()
  action

evalList::IO [String]->Net()
evalList a = do 
  s <-liftIO a
  traverse_ eval s
  
-- Send a privmsg to the current chan + server
privmsg ::  String -> Net ()
privmsg msg = write "PRIVMSG" (myChan ++ " :" ++ msg)

--Return Channel name
getChannel::String-> String
getChannel s = let a = s =~"PRIVMSG\\s+(.+)\\s+:" ::[[String]] 
                in getSafely (a ^? element 0 )

--Extract Contents Safely
getSafely::Maybe [String] -> String
getSafely (Just xs) = (xs !! 1)
getSafely (Nothing) = ""
