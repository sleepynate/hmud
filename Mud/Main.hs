{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Main (main) where

import Mud.Convenience
import Mud.PlayerCmds
import Mud.TheWorld
import Mud.TopLvlDefs

import Control.Lens.Operators ((^.))
import Control.Monad.Trans.State
import Data.Text.Strict.Lens (packed)
import qualified Data.Text.IO as T
import System.Directory (setCurrentDirectory)
import System.Environment (getEnv)
import System.Environment (getProgName)


-- TODO: Consider what, if anything, should be done about indexing in commands like this:
-- r 1.sw 3.sw
-- ...when you have 3 swords.


main :: IO ()
main = setCurrentDirectory mudDir >> welcomeMsg >> 
       execStateT initialize initWS >>= evalStateT game


welcomeMsg :: IO ()
welcomeMsg = do
    un <- getEnv "USER"
    mn <- whatsMyName
    T.putStrLn $ "\nHello, " <> un^.packed <> ". Welcome to " <> dblQuote mn <> " ver " <> ver <> ".\n"
  where
    whatsMyName = getProgName >>= \mn ->
        return $ if mn == "<interactive>" then "Y U NO COMPILE ME?" else mn^.packed
