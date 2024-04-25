-- Command.hs:
module Command where

import Download
import Data.List
import Format
import Presets
import Index
import Control.Lens
--List of commands
command :: String->[String] -> IO [String]
command c (x:xs) = do
    let action
          |  x== ".quit"  =  return ["!quit"]
          |  x== ".quote" || x== ".q"  = do  
                              a <- composeFinal xs
                              return $ ["PRIVMSG:" ++ c ++ " :" ++ (take ((length a)-2) a)]

          |  x== ".crypto" || x== ".c"  = do  
                              a <- composeCrypto xs
                              return $ ["PRIVMSG:" ++ c ++ " :" ++ (take ((length a)-2) a)]
           
          |  x==".list" || x== ".l"   = do
                             a <- presetIndex (xs !! 0)
                             return $ ["PRIVMSG:" ++ c ++ " :" ++ a]
          |  x==".f"      = 
                            getSheets c (xs !! 0) "n"
          |  x==".type" || x==".t" = do 
                                    a <- getType (xs !! 0)
                                    return $ ["PRIVMSG:" ++ c ++ " :" ++ a]

          | "." `isPrefixOf` x = do
                                  a <- composeFinal $ presetCommand x 
                                  return $ ["PRIVMSG:" ++ c ++ " :" ++ (take ((length a)-2) a)]
          |otherwise = return [""]  -- ignore everything else
    action
