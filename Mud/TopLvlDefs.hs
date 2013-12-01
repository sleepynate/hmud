{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.TopLvlDefs where

import Control.Lens.Operators ((^.))
import Data.Text.Strict.Lens (packed, unpacked)
import qualified Data.Text as T
import System.Environment (getEnv)
import System.IO.Unsafe (unsafePerformIO)


ver :: T.Text
ver = "0.0 since 2013-10"


mudDir :: FilePath
mudDir = let home = unsafePerformIO . getEnv $ "HOME"
         in home ++ "/hmud/Mud/"^.unpacked


helpDir :: FilePath
helpDir = mudDir ++ "help/"


allChar, amountChar, indexChar, slotChar, rmChar :: Char
allChar    = '\''
amountChar = '/'
indexChar  = '.'
slotChar   = ':'
rmChar = '-'


slotR, slotL :: T.Text
slotR = (slotChar : "r")^.packed
slotL = (slotChar : "l")^.packed
