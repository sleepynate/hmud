{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld where

import Mud.DataTypes
import Mud.Ids
import Mud.StateHelpers

import Control.Lens (at, to)
import Control.Lens.Operators ((^.), (?=))
import Control.Monad (liftM, unless, when)
import Control.Monad.Trans.State
import Data.List ((\\))
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M


getUnusedId :: StateT WorldState IO Id
getUnusedId = liftM findAvailKey allKeys


findAvailKey :: [Int] -> Int
findAvailKey xs = head $ [0..] \\ xs


allKeys :: StateT WorldState IO Inv
allKeys = do
    ws <- get
    return (ws^.typeTbl.to IM.keys)


ensureSafeId :: Id -> StateT WorldState IO ()
ensureSafeId i = do
    ks <- allKeys
    unless (null ks) $ when (i `elem` ks) (error $ "Attempted to use key " ++ show i ++ " more than once.")


putObj :: Id -> Ent -> Obj -> StateT WorldState IO ()
putObj i e o = do
    ensureSafeId i
    typeTbl.at i ?= ObjType
    entTbl.at i  ?= e
    objTbl.at i  ?= o


putCon :: Id -> Ent -> Obj -> Inv -> Con -> StateT WorldState IO ()
putCon i e o is c = do
    ensureSafeId i
    typeTbl.at i ?= ConType
    entTbl.at i  ?= e
    objTbl.at i  ?= o
    invTbl.at i  ?= is
    conTbl.at i  ?= c


putWpn :: Id -> Ent -> Obj -> Wpn -> StateT WorldState IO ()
putWpn i e o w = do
    ensureSafeId i
    typeTbl.at i ?= WpnType
    entTbl.at i  ?= e
    objTbl.at i  ?= o
    wpnTbl.at i  ?= w


putArm :: Id -> Ent -> Obj -> Arm -> StateT WorldState IO ()
putArm i e o a = do
    ensureSafeId i
    typeTbl.at i ?= ArmType
    entTbl.at i  ?= e
    objTbl.at i  ?= o
    armTbl.at i  ?= a


putMob :: Id -> Ent -> Inv -> EqMap -> Mob -> StateT WorldState IO ()
putMob i e is em m = do
    ensureSafeId i
    typeTbl.at i ?= MobType
    entTbl.at i  ?= e
    invTbl.at i  ?= is
    eqTbl.at i   ?= em
    mobTbl.at i  ?= m


putRm :: Id -> Inv -> Rm -> StateT WorldState IO ()
putRm i is r = do
    ensureSafeId i
    typeTbl.at i ?= RmType
    invTbl.at i  ?= is
    rmTbl.at i   ?= r


-----


initWS :: WorldState
initWS = WorldState
  (IM.fromList [])
  (IM.fromList [])
  (IM.fromList [])
  (IM.fromList [])
  (IM.fromList [])
  (IM.fromList [])
  (IM.fromList [])
  (IM.fromList [])
  (IM.fromList [])
  (IM.fromList [])
  (IM.fromList [])
  initPla


initPla :: Pla
initPla = Pla { _rmId = iHill
              , _race = Human }


createWorld :: StateT WorldState IO ()
createWorld = do
    putMob iPla (Ent iPla "" "" "" "" 0) [iKewpie1, iBag1, iClub] (M.fromList [(RHandS, iSword1), (LHandS, iSword2)]) (Mob Male 10 10 10 10 10 10 0 LHand)

    putRm  iHill [iGP1, iBastard] (Rm "The hill" "You stand atop a tall hill." 0 deadEnd deadEnd iCliff deadEnd deadEnd deadEnd)

    putRm  iCliff [iElephant, iBag2] (Rm "The cliff" "You have reached the edge of a cliff." 0 deadEnd deadEnd deadEnd iHill deadEnd deadEnd)
    
    putObj iKewpie1 (Ent iKewpie1 "kewpie" "kewpie doll" "" "The red kewpie doll is disgustingly cute." 0) (Obj 1 1)
    putObj iKewpie2 (Ent iKewpie2 "kewpie" "kewpie doll" "" "The orange kewpie doll is disgustingly cute." 0) (Obj 1 1)
    
    putObj iGP1 (Ent iGP1 "guinea" "guinea pig" "" "The yellow guinea pig is charmingly cute." 0) (Obj 1 1)
    putObj iGP2 (Ent iGP2 "guinea" "guinea pig" "" "The green guinea pig is charmingly cute." 0) (Obj 1 1)
    putObj iGP3 (Ent iGP3 "guinea" "guinea pig" "" "The blue guinea pig is charmingly cute." 0) (Obj 1 1)

    putObj iElephant (Ent iElephant "elephant" "elephant" "" "The elephant is huge and smells terrible." 0) (Obj 1 1)

    putCon iBag1 (Ent iBag1 "sack" "cloth sack" "" "It's a typical cloth sack, perfect for holding all your treasure. It's red." 0) (Obj 1 1) [iGP2, iGP3] (Con 10)
    putCon iBag2 (Ent iBag2 "sack" "cloth sack" "" "It's a typical cloth sack, perfect for holding all your treasure. It's blue." 0) (Obj 1 1) [iKewpie2] (Con 10)

    putWpn iSword1 (Ent iSword1 "short" "short sword" "" "It's a sword; short but still sharp! It's silver." 0) (Obj 1 1) (Wpn OneHanded 1 10)
    putWpn iSword2 (Ent iSword2 "short" "short sword" "" "It's a sword; short but still sharp! It's gold." 0) (Obj 1 1) (Wpn OneHanded 1 10)

    putWpn iClub (Ent iClub "club" "wooden club" "" "It's a crude wooden club; the type a neanderthal might use to great effect." 0) (Obj 1 1) (Wpn OneHanded 1 5)

    putWpn iBastard (Ent iBastard "bastard" "two-handed bastard sword" "" "What a big sword! With the right technique it could do a great deal of damage." 0) (Obj 1 1) (Wpn TwoHanded 5 20)


-----


mkOkapi :: StateT WorldState IO Id
mkOkapi = do
    i <- getUnusedId
    let e = Ent { _entId = i
                , _entName = "okapi"
                , _sing = "okapi"
                , _plur = ""
                , _entDesc = "It looks like a cross between a horse and a zebra."
                , _entFlags = 0 }
    let m = Mob { _sex = Male
                , _st = 10
                , _dx = 10
                , _iq = 10
                , _ht = 10
                , _hp = 10
                , _fp = 10
                , _xp = 50
                , _hand = NoHand }
    putMob i e [] (M.fromList []) m
    addToInv [i] iHill
    return i
