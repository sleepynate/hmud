{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld where

import Mud.DataTypes
import Mud.Ids

import Control.Lens (at, to)
import Control.Lens.Operators ((^.), (?=))
import Control.Monad (when)
import Control.Monad.Trans.State
import qualified Data.IntMap as M


ensureSafeId :: Id -> State WorldState ()
ensureSafeId i = do
    ws <- get
    let ks = ws^.typeTbl.to M.keys
    case length ks of 0 -> return ()
                      _ -> when (i `elem` ks) (error $ "Attempted to use key " ++ show i ++ " more than once.")


putObj :: Id -> Ent -> Obj -> State WorldState ()
putObj i e o = do
    ensureSafeId i
    typeTbl.at i ?= ObjType
    entTbl.at i  ?= e
    objTbl.at i  ?= o


putCon :: Id -> Ent -> Obj -> Inv -> Con -> State WorldState ()
putCon i e o xs c = do
    ensureSafeId i
    typeTbl.at i ?= ConType
    entTbl.at i  ?= e
    objTbl.at i  ?= o
    invTbl.at i  ?= xs
    conTbl.at i  ?= c


putWpn :: Id -> Ent -> Obj -> Wpn -> State WorldState ()
putWpn i e o w = do
    ensureSafeId i
    typeTbl.at i ?= WpnType
    entTbl.at i  ?= e
    objTbl.at i  ?= o
    wpnTbl.at i  ?= w


putArm :: Id -> Ent -> Obj -> Arm -> State WorldState ()
putArm i e o a = do
    ensureSafeId i
    typeTbl.at i ?= ArmType
    entTbl.at i  ?= e
    objTbl.at i  ?= o
    armTbl.at i  ?= a


putMob :: Id -> Ent -> Inv -> Mob -> State WorldState ()
putMob i e xs m = do
    ensureSafeId i
    typeTbl.at i ?= MobType
    entTbl.at i  ?= e
    invTbl.at i  ?= xs
    mobTbl.at i  ?= m


putRm :: Id -> Inv -> Room -> State WorldState ()
putRm i xs r = do
    ensureSafeId i
    invTbl.at i ?= xs
    rmTbl.at  i ?= r


-----


initWS :: WorldState
initWS = WorldState initPla (M.fromList []) (M.fromList []) (M.fromList []) (M.fromList []) (M.fromList []) (M.fromList []) (M.fromList []) (M.fromList []) (M.fromList [])


initPla :: Pla
initPla = Pla { _rmId = iHill
              , _race = Human }


createWorld :: State WorldState ()
createWorld = do
    putMob iPla (Ent iPla "" "" "" "" 0) [iKewpie1, iBag1] (Mob Male 10 10 10 10 10 10 0)
    putRm  iHill [iGP1] (Room "The hill" "You stand atop a tall hill." 0 deadEnd deadEnd iCliff deadEnd deadEnd deadEnd)
    putRm  iCliff [iElephant, iBag2] (Room "The cliff" "You have reached the edge of a cliff." 0 deadEnd deadEnd deadEnd iHill deadEnd deadEnd)
    
    putObj iKewpie1 (Ent iKewpie1 "kewpie" "kewpie doll" "" "The red kewpie doll is disgustingly cute." 0) (Obj 1 1)
    putObj iKewpie2 (Ent iKewpie2 "kewpie" "kewpie doll" "" "The orange kewpie doll is disgustingly cute." 0) (Obj 1 1)
    
    putObj iGP1 (Ent iGP1 "guinea" "guinea pig" "" "The yellow guinea pig is charmingly cute." 0) (Obj 1 1)
    putObj iGP2 (Ent iGP2 "guinea" "guinea pig" "" "The green guinea pig is charmingly cute." 0) (Obj 1 1)
    putObj iGP3 (Ent iGP3 "guinea" "guinea pig" "" "The blue guinea pig is charmingly cute." 0) (Obj 1 1)

    putObj iElephant (Ent iElephant "elephant" "elephant" "" "The elephant is huge and smells terrible." 0) (Obj 1 1)

    putCon iBag1 (Ent iBag1 "sack" "cloth sack" "" "It's a typical cloth sack, perfect for holding all your treasure. It's red." 0) (Obj 1 1) [iGP2, iGP3] (Con 10)
    putCon iBag2 (Ent iBag2 "sack" "cloth sack" "" "It's a typical cloth sack, perfect for holding all your treasure. It's blue." 0) (Obj 1 1) [iKewpie2] (Con 10)
