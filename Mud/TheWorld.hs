{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.TheWorld ( initialize
                    , initWS
                    , mkOkapi ) where

import Mud.Convenience
import Mud.Ids
import Mud.StateDataTypes
import Mud.StateHelpers

import Control.Lens (at, ix, to)
import Control.Lens.Operators ((^.), (^?!), (?=))
import Control.Monad (unless, when)
import Control.Monad.Trans.State
import Data.Functor
import Data.List ((\\))
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M


getUnusedId :: MudStack Id
getUnusedId = findAvailKey <$> allKeys


findAvailKey :: Inv -> Id
findAvailKey = head . (\\) [0..]


ensureSafeId :: Id -> MudStack ()
ensureSafeId i = allKeys >>= \ks ->
    unless (null ks) $ when (i `elem` ks) (error $ "Attempted to use key " ++ show i ++ " more than once.")


allKeys :: MudStack Inv
allKeys = gets (^.typeTbl.to IM.keys)


-----


putObj :: Id -> Ent -> Obj -> MudStack ()
putObj i e o = do
    ensureSafeId i
    typeTbl.at i ?= ObjType
    entTbl.at i  ?= e
    objTbl.at i  ?= o


putCloth :: Id -> Ent -> Obj -> Cloth -> MudStack ()
putCloth i e o c = do
    ensureSafeId i
    typeTbl.at i  ?= ClothType
    entTbl.at i   ?= e
    objTbl.at i   ?= o
    clothTbl.at i ?= c


putCon :: Id -> Ent -> Obj -> Inv -> Con -> MudStack ()
putCon i e o is c = do
    ensureSafeId i
    typeTbl.at i ?= ConType
    entTbl.at i  ?= e
    objTbl.at i  ?= o
    invTbl.at i  ?= is
    conTbl.at i  ?= c


putWpn :: Id -> Ent -> Obj -> Wpn -> MudStack ()
putWpn i e o w = do
    ensureSafeId i
    typeTbl.at i ?= WpnType
    entTbl.at i  ?= e
    objTbl.at i  ?= o
    wpnTbl.at i  ?= w


--putArm :: Id -> Ent -> Obj -> Arm -> MudStack ()
--putArm i e o a = do
--    ensureSafeId i
--    typeTbl.at i ?= ArmType
--    entTbl.at i  ?= e
--    objTbl.at i  ?= o
--    armTbl.at i  ?= a


putMob :: Id -> Ent -> Inv -> EqMap -> Mob -> MudStack ()
putMob i e is em m = do
    ensureSafeId i
    typeTbl.at i ?= MobType
    entTbl.at i  ?= e
    invTbl.at i  ?= is
    eqTbl.at i   ?= em
    mobTbl.at i  ?= m


putRm :: Id -> Inv -> Rm -> MudStack ()
putRm i is r = do
    ensureSafeId i
    typeTbl.at i ?= RmType
    invTbl.at i  ?= is
    rmTbl.at i   ?= r


-----


initWS :: WorldState
initWS = WorldState IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty initPla


initPla :: Pla
initPla = Pla { _rmId = iHill
              , _race = Human }


createWorld :: MudStack ()
createWorld = do
    putMob iPla (Ent iPla "" "" "" "" 0) [iKewpie1, iBag1, iClub] (M.fromList [(RHandS, iSword1), (LHandS, iSword2)]) (Mob Male 10 10 10 10 10 10 0 LHand)

    putRm  iHill [iGP1, iLongSword] (Rm "The hill" "You stand atop a tall hill." 0 deadEnd deadEnd iCliff deadEnd deadEnd deadEnd)
    putRm  iCliff [iElephant, iBag2, iBracelet1, iBracelet2, iBracelet3, iBracelet4] (Rm "The cliff" "You have reached the edge of a cliff. \
        \There is a sizable hole in the ground." 0 deadEnd deadEnd deadEnd iHill deadEnd iHole)
    putRm  iHole [iNeck1, iNeck2, iNeck3, iNeck4] (Rm "The hole" "You have climbed into a hole in the ground. There is barely enough room to move around. \
        \It's damp and smells of soil." 0 deadEnd deadEnd deadEnd deadEnd iCliff deadEnd)
    
    putObj iKewpie1 (Ent iKewpie1 "kewpie" "kewpie doll" "" "The red kewpie doll is disgustingly cute." 0) (Obj 1 1)
    putObj iKewpie2 (Ent iKewpie2 "kewpie" "kewpie doll" "" "The orange kewpie doll is disgustingly cute." 0) (Obj 1 1)
    
    putObj iGP1 (Ent iGP1 "guinea" "guinea pig" "" "The yellow guinea pig is charmingly cute." 0) (Obj 1 1)
    putObj iGP2 (Ent iGP2 "guinea" "guinea pig" "" "The green guinea pig is charmingly cute." 0) (Obj 1 1)
    putObj iGP3 (Ent iGP3 "guinea" "guinea pig" "" "The blue guinea pig is charmingly cute." 0) (Obj 1 1)

    putObj iElephant (Ent iElephant "elephant" "elephant" "" "The elephant is huge and smells terrible." 0) (Obj 1 1)

    putCon iBag1 (Ent iBag1 "sack" "cloth sack" "" "It's a typical cloth sack, perfect for holding all your treasure. It's red." 0) (Obj 1 1) [iGP2, iGP3] (Con 10)
    putCon iBag2 (Ent iBag2 "sack" "cloth sack" "" "It's a typical cloth sack, perfect for holding all your treasure. It's blue." 0) (Obj 1 1) [iKewpie2, iRing1, iRing2, iRing3, iRing4] (Con 10)

    putWpn iSword1 (Ent iSword1 "sword" "short sword" "" "It's a sword; short but still sharp! It's silver." 0) (Obj 1 1) (Wpn OneHanded 1 10)
    putWpn iSword2 (Ent iSword2 "sword" "short sword" "" "It's a sword; short but still sharp! It's gold." 0) (Obj 1 1) (Wpn OneHanded 1 10)

    putWpn iClub (Ent iClub "club" "wooden club" "" "It's a crude wooden club; the type a neanderthal might use to great effect." 0) (Obj 1 1) (Wpn OneHanded 1 5)

    putWpn iLongSword (Ent iLongSword "sword" "two-handed long sword" "" "What a big sword! With the right technique it could do a great deal of damage." 0) (Obj 1 1) (Wpn TwoHanded 5 20)

    putCloth iBracelet1 (Ent iBracelet1 "bracelet" "silver bracelet" "" "It's a simple bronze bracelet." 0) (Obj 1 1) WristC
    putCloth iBracelet2 (Ent iBracelet2 "bracelet" "bronze bracelet" "" "It's a simple silver bracelet." 0) (Obj 1 1) WristC
    putCloth iBracelet3 (Ent iBracelet3 "bracelet" "gold bracelet" "" "It's a simple gold bracelet." 0) (Obj 1 1) WristC
    putCloth iBracelet4 (Ent iBracelet4 "bracelet" "platinum bracelet" "" "It's a simple platinum bracelet." 0) (Obj 1 1) WristC

    putCloth iRing1 (Ent iRing1 "ring" "bronze ring" "" "It's a simple bronze ring." 0) (Obj 1 1) FingerC
    putCloth iRing2 (Ent iRing2 "ring" "silver ring" "" "It's a simple silver ring." 0) (Obj 1 1) FingerC
    putCloth iRing3 (Ent iRing3 "ring" "gold ring" "" "It's a simple gold ring." 0) (Obj 1 1) FingerC
    putCloth iRing4 (Ent iRing4 "ring" "platinum ring" "" "It's a simple platinum ring." 0) (Obj 1 1) FingerC

    putCloth iNeck1 (Ent iNeck1 "necklace" "bronze necklace" "" "It's a simple bronze necklace." 0) (Obj 1 1) NeckC
    putCloth iNeck2 (Ent iNeck2 "necklace" "silver necklace" "" "It's a simple silver necklace." 0) (Obj 1 1) NeckC
    putCloth iNeck3 (Ent iNeck3 "necklace" "gold necklace" "" "It's a simple gold necklace." 0) (Obj 1 1) NeckC
    putCloth iNeck4 (Ent iNeck4 "necklace" "platinum necklace" "" "It's a simple platinum necklace." 0) (Obj 1 1) NeckC

-----


initialize :: MudStack ()
initialize = output "Creating the world..." >> createWorld >> 
             output "Sorting all inventories...\n" >> sortAllInvs


sortAllInvs :: MudStack ()
sortAllInvs = gets (^.invTbl.to IM.keys) >>= mapM_ sortEach
  where
    sortEach k = gets (^?!invTbl.ix k) >>= sortInv >>= (invTbl.at k ?=)


-----


mkOkapi :: MudStack Id
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
    putMob i e [] M.empty m
    addToInv [i] iHill -- Will sort the inv.
    return i
