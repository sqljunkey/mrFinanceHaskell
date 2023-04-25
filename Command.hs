-- Command.hs:
module Command where

import Download
import Data.List
import Format
import Presets

--List of commands
command :: [String] -> IO String
command (x:xs) = do
    let action
          |  x== "!quit"  =  return "!quit"
          |  x== "!quote" = do  
                              a <- composeFinal xs
                              return $ "PRIVMSG:" ++ a
        --  |  x=="!list"   = do
          --                   a <- compose xs

          | "!" `isPrefixOf` x = do
                                 a <- composeFinal $ presetCommand x 
                                 return $ "PRIVMSG:" ++ a
          |otherwise = return ""  -- ignore everything else
    action
