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
                 | RIF | RMF | RRF | RPF
                 | LIF | LMF | LRF | LPF

-----

type BothGramNos = (Sing, Plur)

-----

type Amount = Int
type Index  = Int
type NameSearchedFor = T.Text

data GetEntResult = Mult NameSearchedFor (Maybe [Ent])
                  | Indexed Index NameSearchedFor (Either Plur Ent)
                  | Sorry

-----

type ConName = T.Text
