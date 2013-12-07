{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.MiscDataTypes where

import Mud.StateDataTypes

import qualified Data.Text as T

-----

type Input   = (CmdName, Rest)
type CmdName = T.Text
type Rest    = [T.Text]

type Action = Rest -> MudStack ()

-----

data Cmd = Cmd { cmdName :: CmdName
               , action  :: Action
               , cmdDesc :: T.Text }

-----

data InvType = PlaInv | PlaEq | RmInv deriving Eq

data GetOrDrop = Get | Drop

data PutOrRem = Put | Rem

data RightOrLeft = R
                 | L
                 | RI | RM | RR | RP
                 | LI | LM | LR | LP deriving (Show, Read)

-----

type BothGramNos = (Sing, Plur)

-----

type Amount = Int
type Index  = Int
type NameSearchedFor = T.Text

data GetEntResult = Mult    Amount NameSearchedFor (Maybe [Ent])
                  | Indexed Index  NameSearchedFor (Either Plur Ent)
                  | Sorry          NameSearchedFor

-----

type ConName = T.Text
