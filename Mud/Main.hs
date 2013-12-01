{-# OPTIONS_GHC -Wall -Werror -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Main (main) where

import Mud.Convenience
import Mud.PlayerCmds
import Mud.TheWorld

import Control.Lens.Operators ((^.))
import Control.Monad.Trans.State
import Data.Text.Strict.Lens (packed)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (setCurrentDirectory)
import System.Environment (getEnv)
import System.Environment (getProgName)


-- TODO: Consider what, if anything, should be done about indexing in commands like this:
-- r 1.sw 3.sw
-- ...when you have 3 swords.


ver :: T.Text
ver = "0.0 since 2013-10"


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
        return $ if mn == "<interactive>" then "why u no compile me?" else mn^.packed
