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
          |  x== "!quit"  =  return ["!quit"]
          |  x== "!quote" || x== "!q" = do  
                              a <- composeFinal xs
                              return $ ["PRIVMSG:" ++ c ++ " :" ++ a]
          |  x=="!list"   = do
                             a <- presetIndex (xs !! 0)
                             return $ ["PRIVMSG:" ++ c ++ " :" ++ a]
          |  x=="!f"      = 
                            getSheets c (xs !! 0) "n"
          | "!" `isPrefixOf` x = do
                                  a <- composeFinal $ presetCommand x 
                                  return $ ["PRIVMSG:" ++ c ++ " :" ++ a]
          |otherwise = return [""]  -- ignore everything else
    action
