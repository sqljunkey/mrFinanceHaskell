-- Command.hs:
module Command where

import Download
import Data.List
import Format
import Presets
import Index
import Control.Lens
--List of commands
command :: [String] -> IO [String]
command (x:xs) = do
    let action
          |  x== "!quit"  =  return ["!quit"]
          |  x== "!quote" || x== "!q" = do  
                              a <- composeFinal xs
                              return $ ["PRIVMSG:" ++ a]
          |  x=="!list"   = do
                             a <- presetIndex (xs !! 0)
                             return $ ["PRIVMSG:" ++ a]
          |  x=="!f"      =
            getSheets (xs !! 0) "n"
          | "!" `isPrefixOf` x = do
                                 a <- composeFinal $ presetCommand x 
                                 return $ ["PRIVMSG:" ++ a]
          |otherwise = return [""]  -- ignore everything else
    action
