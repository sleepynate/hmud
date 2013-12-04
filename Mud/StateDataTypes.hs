{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Mud.StateDataTypes where

import Control.Lens (Lens', lens, makeLenses)
import Control.Monad.Trans.State
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
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

-- Has an entity.

data Obj = Obj { _weight :: Int
               , _vol    :: Int } deriving (Eq, Show)

-----

-- Has an object (and an entity).

data Cloth = EarC
           | NoseC
           | NeckC
           | WristC
           | FingerC
           | UpBodyC
           | LowBodyC
           | FullBodyC
           | BackC
           | FeetC deriving (Eq, Show)

-----

-- Has an object (and an entity) and an inventory.

type Cap = Int

newtype Con = Con Cap deriving (Eq, Show)

-----

-- Has an object (and an entity).

data Wpn = Wpn { _wpnSub :: WpnSub
               , _minDmg :: Int
               , _maxDmg :: Int } deriving (Eq, Show)

data WpnSub = OneHanded
            | TwoHanded deriving (Eq, Show)

-----

-- Has an object (and an entity).

type AC = Int

data Arm = Arm { _armSub :: ArmSub
               , _ac     :: AC } deriving (Eq, Show)

data ArmSub = HeadA
            | UpBodyA
            | LowBodyA
            | FullBodyA deriving (Eq, Show)

-----

-- Has an entity and an inventory and equipment.

data Mob = Mob { _sex               :: Sex
               , _st, _dx, _iq, _ht :: Int
               , _hp, _fp           :: Int
               , _xp                :: Int
               , _hand              :: Hand } deriving (Eq, Show)

data Sex = Male
         | Female
         | NoSex deriving (Eq, Show)

data Hand = RHand
          | LHand
          | NoHand deriving (Eq, Show)

-----

-- Has a mob (and an entity and an inventory and equipment).

data Pla = Pla { _rmId :: Id
               , _race :: Race } deriving (Eq, Show)

data Race = Human
          | Elf
          | Dwarf
          | Halfling
          | Nymph
          | Felinoid
          | Vulpenoid
          | Lagomorph deriving (Eq, Show)

----

-- Has an inventory.

data Rm = Rm { _rmName  :: T.Text
             , _rmDesc  :: T.Text
             , _rmFlags :: Int
             , north, south, east, west, up, down :: Id } deriving (Eq, Show)

instance HasNameDesc Rm where
  name = lens _rmName (\e v -> e { _rmName = v })
  desc = lens _rmDesc (\e v -> e { _rmDesc = v })

instance HasFlags Rm where
  flags = lens _rmFlags (\e v -> e { _rmFlags = v })

-----

type Inv = [Id]

----

type EqMap = M.Map Slot Id

data Slot = HeadS
          | REar1S | REar2S
          | LEar1S | LEar2S
          | Nose1S | Nose2S
          | Neck1S | Neck2S | Neck3S
          | RWrist1S | RWrist2S | RWrist3S
          | LWrist1S | LWrist2S | LWrist3S
          | RIndexFS | RMidFS | RRingFS | RPinkyFS
          | LIndexFS | LMidFS | LRingFS | LPinkyFS
          | RHandS
          | LHandS
          | BothHandsS
          | UpBodyCS
          | LowBodyCS
          | FullBodyCS
          | UpBodyAS
          | LowBodyAS
          | FullBodyAS
          | BackS
          | FeetS deriving (Eq, Ord, Enum)

instance Show Slot where
  show HeadS      = "head"
  show REar1S     = "right ear"
  show REar2S     = "right ear"
  show LEar1S     = "left ear"
  show LEar2S     = "left ear"
  show Nose1S     = "nose"
  show Nose2S     = "nose"
  show Neck1S     = "neck"
  show Neck2S     = "neck"
  show Neck3S     = "neck"
  show RWrist1S   = "right wrist"
  show RWrist2S   = "right wrist"
  show RWrist3S   = "right wrist"
  show LWrist1S   = "left wrist"
  show LWrist2S   = "left wrist"
  show LWrist3S   = "left wrist"
  show RIndexFS   = "right index finger"
  show RMidFS     = "right middle finger"
  show RRingFS    = "right ring finger"
  show RPinkyFS   = "right pinky finger"
  show LIndexFS   = "left index finger"
  show LMidFS     = "left middle finger"
  show LRingFS    = "left ring finger"
  show LPinkyFS   = "left pinky finger"
  show RHandS     = "right hand"
  show LHandS     = "left hand"
  show BothHandsS = "both hands"
  show UpBodyCS   = "upper body"
  show LowBodyCS  = "lower body"
  show FullBodyCS = "full body"
  show UpBodyAS   = "upper body"
  show LowBodyAS  = "lower body"
  show FullBodyAS = "full body"
  show BackS      = "back"
  show FeetS      = "feet"

-----

data Type = ObjType
          | ClothType
          | ConType
          | WpnType
          | ArmType
          | MobType
          | PlaType
          | RmType  deriving (Eq, Show)

-----

type EntTbl   = IM.IntMap Ent
type ObjTbl   = IM.IntMap Obj
type ClothTbl = IM.IntMap Cloth
type ConTbl   = IM.IntMap Con
type WpnTbl   = IM.IntMap Wpn
type ArmTbl   = IM.IntMap Arm
type MobTbl   = IM.IntMap Mob
type RmTbl    = IM.IntMap Rm
type InvTbl   = IM.IntMap Inv
type EqTable  = IM.IntMap EqMap
type TypeTbl  = IM.IntMap Type

data WorldState = WorldState { _entTbl   :: EntTbl
                             , _objTbl   :: ObjTbl
                             , _clothTbl :: ClothTbl
                             , _conTbl   :: ConTbl
                             , _wpnTbl   :: WpnTbl
                             , _armTbl   :: ArmTbl
                             , _mobTbl   :: MobTbl
                             , _rmTbl    :: RmTbl
                             , _invTbl   :: InvTbl
                             , _eqTbl    :: EqTable
                             , _typeTbl  :: TypeTbl
                             , _pla      :: Pla }

-----

type MudStack a = StateT WorldState IO a

-----

makeLenses ''Ent
makeLenses ''Obj
makeLenses ''Wpn
makeLenses ''Arm
makeLenses ''Mob
makeLenses ''Pla
makeLenses ''Rm
makeLenses ''WorldState
