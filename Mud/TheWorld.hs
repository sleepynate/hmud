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
import Data.List ((\\), sort)
import qualified Data.IntMap as IM


getUnusedId :: StateT WorldState IO Id
getUnusedId = liftM findAvailKey allKeys


findAvailKey :: [Int] -> Int
findAvailKey xs = head $ [0..] \\ (sort xs)


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
putCon i e o xs c = do
    ensureSafeId i
    typeTbl.at i ?= ConType
    entTbl.at i  ?= e
    objTbl.at i  ?= o
    invTbl.at i  ?= xs
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


putMob :: Id -> Ent -> Inv -> Mob -> StateT WorldState IO ()
putMob i e xs m = do
    ensureSafeId i
    typeTbl.at i ?= MobType
    entTbl.at i  ?= e
    invTbl.at i  ?= xs
    mobTbl.at i  ?= m


putRm :: Id -> Inv -> Room -> StateT WorldState IO ()
putRm i xs r = do
    ensureSafeId i
    typeTbl.at i ?= RmType
    invTbl.at i  ?= xs
    rmTbl.at  i  ?= r


-----


initWS :: WorldState
initWS = WorldState initPla (IM.fromList []) (IM.fromList []) (IM.fromList []) (IM.fromList []) (IM.fromList []) (IM.fromList []) (IM.fromList []) (IM.fromList []) (IM.fromList []) (IM.fromList [])


initPla :: Pla
initPla = Pla { _rmId = iHill
              , _race = Human }


createWorld :: StateT WorldState IO ()
createWorld = do
    putMob iPla (Ent iPla "" "" "" "" 0) [iKewpie1, iBag1] (Mob Male 10 10 10 10 10 10 0)
    putRm  iHill [iGP1] (Room "The hill" "You stand atop a tall hill." 0 deadEnd deadEnd iCliff deadEnd deadEnd deadEnd)
    putRm  iCliff [iElephant, iBag2] (Room "The cliff" "You have reached the edge of a cliff." 0 deadEnd deadEnd deadEnd iHill deadEnd deadEnd)
    
    putObj iKewpie1 (Ent iKewpie1 "kewpie" "kewpie doll" "" "The red kewpie doll is disgustingly cute." 0) (Obj 1 1 Unequipable)
    putObj iKewpie2 (Ent iKewpie2 "kewpie" "kewpie doll" "" "The orange kewpie doll is disgustingly cute." 0) (Obj 1 1 Unequipable)
    
    putObj iGP1 (Ent iGP1 "guinea" "guinea pig" "" "The yellow guinea pig is charmingly cute." 0) (Obj 1 1 Unequipable)
    putObj iGP2 (Ent iGP2 "guinea" "guinea pig" "" "The green guinea pig is charmingly cute." 0) (Obj 1 1 Unequipable)
    putObj iGP3 (Ent iGP3 "guinea" "guinea pig" "" "The blue guinea pig is charmingly cute." 0) (Obj 1 1 Unequipable)

    putObj iElephant (Ent iElephant "elephant" "elephant" "" "The elephant is huge and smells terrible." 0) (Obj 1 1 Unequipable)

    putCon iBag1 (Ent iBag1 "sack" "cloth sack" "" "It's a typical cloth sack, perfect for holding all your treasure. It's red." 0) (Obj 1 1 Unequipable) [iGP2, iGP3] (Con 10)
    putCon iBag2 (Ent iBag2 "sack" "cloth sack" "" "It's a typical cloth sack, perfect for holding all your treasure. It's blue." 0) (Obj 1 1 Unequipable) [iKewpie2] (Con 10)


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
                , _xp = 50 }
    putMob i e [] m
    addToInv [i] iHill
    return i
