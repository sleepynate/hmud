{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Mud.DataTypes where

import Control.Lens (Lens', lens, makeLenses)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Text as T

-----

class HasNameDesc a where
  name, desc :: Lens' a T.Text

class HasFlags a where
  flags :: Lens' a Int

-----

type Id = Int

type Sing = T.Text
type Plur = T.Text

data Ent = Ent { _entId    :: Id
               , _entName  :: T.Text
               , _sing     :: Sing
               , _plur     :: Plur
               , _entDesc  :: T.Text
               , _entFlags :: Int } deriving (Eq, Show)

instance HasNameDesc Ent where
  name = lens _entName (\e v -> e { _entName = v })
  desc = lens _entDesc (\e v -> e { _entDesc = v })

instance HasFlags Ent where
  flags = lens _entFlags (\e v -> e { _entFlags = v })

-----

data ObjType = Headwear
             | Necklace
             | TorsoArm
             | Bracelet
             | OneHanded
             | TwoHanded
             | LowBodyArm
             | Footwear
             | FullBodyArm
             | UpBodyCloth
             | LowBodyCloth
             | Unequipable deriving (Eq, Show)

-----

-- Has an entity.

data Obj = Obj { _weight  :: Int
               , _vol     :: Int
               , _objType :: ObjType } deriving (Eq, Show)

-----

-- Has an object (and an entity) and an inventory.

type Cap = Int

data Con = Con Cap deriving (Eq, Show)

-----

-- Has an object (and an entity).

data Wpn = Wpn { _minDmg :: Int
               , _maxDmg :: Int } deriving (Eq, Show)

-----

-- Has an object (and an entity).

type AC = Int

data Arm = Arm AC deriving (Eq, Show)

-----

data Sex = Male | Female | NA deriving (Eq, Show)

-----

-- Has an entity and an inventory and equipment.

data Mob = Mob { _sex               :: Sex
               , _st, _dx, _iq, _ht :: Int
               , _hp, _fp           :: Int
               , _xp                :: Int } deriving (Eq, Show)

-----

data Equip = Head
           | Neck
           | Torso
           | RWrist
           | LWrist
           | RHand
           | LHand
           | BothHands
           | LowBody
           | Feet
           | FullBody
           | ClothUp
           | ClothLow deriving (Eq, Show)

type EqMap = M.Map Equip Id

-----

-- Has an inventory.

data Room = Room { _rmName  :: T.Text
                 , _rmDesc  :: T.Text
                 , _rmFlags :: Int
                 , north, south, east, west, up, down :: Id } deriving (Eq, Show)

instance HasNameDesc Room where
  name = lens _rmName (\e v -> e { _rmName = v })
  desc = lens _rmDesc (\e v -> e { _rmDesc = v })

instance HasFlags Room where
  flags = lens _rmFlags (\e v -> e { _rmFlags = v })

-----

data Race = Human | Elf | Dwarf | Halfling | Nymph | Felinoid | Lagomorph | Vulpenoid deriving (Eq, Show)

-----

-- Has a mob (and an entity and an inventory).

data Pla = Pla { _rmId :: Id
               , _race :: Race } deriving (Eq, Show)

-----

data Type = ObjType | ConType | WpnType | ArmType | MobType | RmType deriving (Eq, Show)

-----

type Inv = [Id]

type TypeTbl = IM.IntMap Type
type EntTbl  = IM.IntMap Ent
type ObjTbl  = IM.IntMap Obj
type InvTbl  = IM.IntMap Inv
type ConTbl  = IM.IntMap Con
type WpnTbl  = IM.IntMap Wpn
type ArmTbl  = IM.IntMap Arm
type MobTbl  = IM.IntMap Mob
type EqTable = IM.IntMap EqMap
type RmTbl   = IM.IntMap Room

data WorldState = WorldState { _pla     :: Pla
                             , _typeTbl :: TypeTbl
                             , _entTbl  :: EntTbl
                             , _objTbl  :: ObjTbl
                             , _invTbl  :: InvTbl
                             , _conTbl  :: ConTbl
                             , _wpnTbl  :: WpnTbl
                             , _armTbl  :: ArmTbl
                             , _mobTbl  :: MobTbl
                             , _eqTable :: EqTable
                             , _rmTbl   :: RmTbl  }

-----

makeLenses ''Ent
makeLenses ''Obj
makeLenses ''Wpn
makeLenses ''Mob
makeLenses ''Room
makeLenses ''Pla
makeLenses ''WorldState
