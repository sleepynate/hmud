{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

--H;ghc -O2 -rtsopts -fforce-recomp Mud/mud.hs
--time cat sampleinput | ./Mud/mud

module Main (main) where

import Mud.Convenience
import Mud.PlayerCmds
import Mud.TheWorld
import Mud.TopLvlDefs

import Control.Lens.Operators ((^.))
import Control.Monad.Trans.State
import Data.Text.Strict.Lens (packed)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (setCurrentDirectory)
import System.Environment (getEnv, getProgName)


-- TODO: There are bugs in the user messaging. Try the sample input and see.


main :: IO ()
main = setCurrentDirectory mudDir >> welcomeMsg >> 
       execStateT initialize initWS >>= evalStateT game


welcomeMsg :: IO ()
welcomeMsg = do
    un <- getEnv "USER"
    mn <- whatsMyName
    T.putStrLn . T.concat $ [ "\nHello, ", un^.packed, ". Welcome to ", dblQuote mn, " ver ", ver, ".\n" ]
  where
    whatsMyName = getProgName >>= \mn ->
        return $ if mn == "<interactive>" then "Y U NO COMPILE ME?" else mn^.packed
