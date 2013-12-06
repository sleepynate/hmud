{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.StateHelpers ( addToInv
                        , findAvailSlot
                        , getCloth
                        , getEnt
                        , getEntBothGramNos
                        , getEntBothGramNosInInv
                        , getEntIds
                        , getEntNamesInInv
                        , getEntSingsInInv
                        , getEntsInInv
                        , getEntsInInvByName
                        , getEntToReadyByName
                        , getEntType
                        , getEq
                        , getEqMap
                        , getInv
                        , getMob
                        , getMobHand
                        , getMobSex
                        , getPlaEq 
                        , getPlaEqMap
                        , getPlaInv
                        , getPlaMobHand
                        , getPlaMobSex
                        , getPlaRm
                        , getPlaRmId
                        , getPlaRmInv
                        , getPlaRmNextRmId 
                        , getWpn
                        , isSlotAvail
                        , makePlurFromBoth
                        , moveInv
                        , procGetEntResPlaInv
                        , procGetEntResRm
                        , remFromInv
                        , ringHelp
                        , sortInv ) where

import Mud.Convenience
import Mud.Ids (deadEnd)
import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.TopLvlDefs

import Control.Applicative
import Control.Lens (_1, at, ix)
import Control.Lens.Operators ((^.), (^?!), (?=))
import Control.Monad.Trans.State
import Data.Char (isDigit)
import Data.List (find)
import Data.List (sortBy)
import Data.Maybe (isNothing)
import Data.Monoid (mappend)
import Data.Text.Read (decimal)
import Data.Text.Strict.Lens (packed, unpacked)
import qualified Data.Map.Strict as M
import qualified Data.Text as T


-- TODO: Add cases for "Mult 1". "Sorry" should have a "n".


getEnt :: Id -> MudStack Ent
getEnt i = gets (^?!entTbl.ix i)


getEntType :: Ent -> MudStack Type
getEntType e = let i = e^.entId
               in gets (^?!typeTbl.ix i)


getEntIds :: [Ent] -> Inv
getEntIds es = [ e^.entId | e <- es ]


getEntsInInv :: Inv -> MudStack [Ent]
getEntsInInv = mapM getEnt


getEntNamesInInv :: Inv -> MudStack [T.Text]
getEntNamesInInv is = getEntsInInv is >>= \es ->
    return [ e^.name | e <- es ]


getEntSingsInInv :: Inv -> MudStack [T.Text]
getEntSingsInInv is = getEntsInInv is >>= \es ->
    return [ e^.sing | e <- es ]


getEntBothGramNos :: Ent -> BothGramNos
getEntBothGramNos e = (e^.sing, e^.plur)


getEntBothGramNosInInv :: Inv -> MudStack [BothGramNos]
getEntBothGramNosInInv is = map getEntBothGramNos <$> getEntsInInv is


makePlurFromBoth :: BothGramNos -> Plur
makePlurFromBoth (s, "") = s <> "s"
makePlurFromBoth (_, p)  = p


-----


getEntsInInvByName :: T.Text -> Inv -> MudStack GetEntResult
getEntsInInvByName searchHame is
  | searchHame == [allChar]^.packed = (Mult (length is) searchHame . Just) <$> getEntsInInv is
  | T.head searchHame == allChar = getMultEnts (maxBound :: Int) (T.tail searchHame) is
  | isDigit (T.head searchHame) = let noText = T.takeWhile isDigit searchHame
                                      noInt  = either undefined (^._1) $ decimal noText
                                      rest   = T.drop (T.length noText) searchHame
                                  in parse rest noInt
  | otherwise = getMultEnts 1 searchHame is
  where
    parse rest noInt
      | T.length rest < 2 = return (Sorry searchHame)
      | otherwise = let delim = T.head rest
                        rest' = T.tail rest
                    in case () of _ | delim == amountChar -> getMultEnts   noInt rest' is -- TODO: Change to a multi-way if.
                                    | delim == indexChar  -> getIndexedEnt noInt rest' is
                                    | otherwise           -> return (Sorry searchHame)


getMultEnts :: Amount -> T.Text -> Inv -> MudStack GetEntResult
getMultEnts a n is
  | a < 1     = return (Sorry n)
  | otherwise = getEntNamesInInv is >>= maybe notFound found . findAbbrev n
  where
    notFound = return (Mult a n Nothing)
    found fullName = (Mult a n . Just . takeMatchingEnts fullName) <$> getEntsInInv is
    takeMatchingEnts fn = take a . filter (\e -> e^.name == fn)


getIndexedEnt :: Index -> T.Text -> Inv -> MudStack GetEntResult
getIndexedEnt x n is
  | x < 1     = return (Sorry n)
  | otherwise = getEntNamesInInv is >>= maybe notFound found . findAbbrev n
  where
    notFound = return (Indexed x n (Left ""))
    found fullName = filter (\e -> e^.name == fullName) <$> getEntsInInv is >>= \matches ->
        if length matches < x
          then let both = getEntBothGramNos . head $ matches
               in return (Indexed x n (Left . makePlurFromBoth $ both))
          else return (Indexed x n (Right $ matches !! (x - 1)))


procGetEntResRm :: GetEntResult -> MudStack (Maybe [Ent]) -- TODO: Can this be deleted? Should it be reworked?
procGetEntResRm ger = case ger of
  Sorry n                 -> output ("You don't see " <> aOrAn n <> " here.")             >> return Nothing
  (Mult 1 n Nothing)      -> output ("You don't see " <> aOrAn n <> " here.")             >> return Nothing
  (Mult _ n Nothing)      -> output ("You don't see any " <> n <> "s here.")              >> return Nothing
  (Mult _ _ (Just es))    -> return (Just es)
  (Indexed _ n (Left "")) -> output ("You don't see any " <> n <> "s here.")              >> return Nothing
  (Indexed x _ (Left p))  -> outputCon [ "You don't see ", showText x, " ", p, " here." ] >> return Nothing
  (Indexed _ _ (Right e)) -> return (Just [e])


procGetEntResPlaInv :: GetEntResult -> MudStack (Maybe [Ent]) -- TODO: Can this be deleted? Should it be reworked?
procGetEntResPlaInv ger = case ger of
  Sorry n                 -> output ("You don't have " <> aOrAn n <> ".")             >> return Nothing
  (Mult 1 n Nothing)      -> output ("You don't have " <> aOrAn n <> ".")             >> return Nothing
  (Mult _ n Nothing)      -> output ("You don't have any " <> n <> "s.")              >> return Nothing
  (Mult _ _ (Just es))    -> return (Just es)
  (Indexed _ n (Left "")) -> output ("You don't have any " <> n <> "s.")              >> return Nothing
  (Indexed x _ (Left p))  -> outputCon [ "You don't have ", showText x, " ", p, "." ] >> return Nothing
  (Indexed _ _ (Right e)) -> return (Just [e])


-----


getEntToReadyByName :: T.Text -> MudStack (Maybe Ent, Maybe RightOrLeft)
getEntToReadyByName searchName
  | slotChar `elem` searchName^.unpacked = let (xs, ys) = T.break (== slotChar) searchName
                                           in if T.length ys == 1 then sorry else findEntToReady xs >>= \me ->
                                               maybe sorry (\rol -> return (me, Just rol)) $ rOrLNamesMap^.at (T.toLower . T.tail $ ys)
  | otherwise = findEntToReady searchName >>= \me ->
      return (me, Nothing)
  where
    sorry = outputCon [ "Please specify ", dblQuote "r", " or ", dblQuote "l", ".\n", ringHelp ] >> return (Nothing, Nothing)


ringHelp :: T.Text
ringHelp = T.concat [ "For rings, specify ", dblQuote "r", " or ", dblQuote "l", " immediately followed by:\n"
                    , dblQuote "i", " for index finger,\n"
                    , dblQuote "m", " for middle finter,\n"
                    , dblQuote "r", " for ring finger,\n"
                    , dblQuote "p", " for pinky finger." ]


findEntToReady :: T.Text -> MudStack (Maybe Ent)
findEntToReady n = getPlaInv >>= getEntsInInvByName n >>= procGetEntResPlaInv >>= \mes ->
      case mes of Just [e]   -> return (Just e)
                  Just (e:_) -> return (Just e) -- TODO: Can this be handled a better way?
                  Nothing    -> return Nothing
                  _          -> undefined


rOrLNamesMap :: M.Map T.Text RightOrLeft
rOrLNamesMap = foldl (\m v -> M.insert (T.toLower . showText $ v) v m) M.empty [R, L, RI, RM, RR, RP, LI, LM, LR, LP]


isSlotAvail :: EqMap -> Slot -> Bool
isSlotAvail em s = isNothing $ em^.at s


findAvailSlot :: EqMap -> [Slot] -> Maybe Slot
findAvailSlot em = find (isSlotAvail em)


-----


getCloth :: Id -> MudStack Cloth
getCloth i = gets (^?!clothTbl.ix i)


-----


getWpn :: Id -> MudStack Wpn
getWpn i = gets (^?!wpnTbl.ix i)


-----


getInv :: Id -> MudStack Inv
getInv i = gets (^?!invTbl.ix i)


getPlaInv :: MudStack Inv
getPlaInv = getInv 0


addToInv :: Inv -> Id -> MudStack ()
addToInv is ti = getInv ti >>= sortInv . (++ is) >>= (invTbl.at ti ?=)


remFromInv :: Inv -> Id -> MudStack ()
remFromInv is fi = getInv fi >>= \fis ->
    invTbl.at fi ?= (deleteAllInList is fis)


moveInv :: Inv -> Id -> Id -> MudStack ()
moveInv [] _  _  = return ()
moveInv is fi ti = remFromInv is fi >> addToInv is ti


sortInv :: Inv -> MudStack Inv
sortInv is = fmap (map (^._1) . sortBy nameThenSing) $ zip3 is <$> getEntNamesInInv is <*> getEntSingsInInv is
  where
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') `mappend` (s `compare` s')


-----


getEqMap :: Id -> MudStack EqMap
getEqMap i = gets (^?!eqTbl.ix i)


getPlaEqMap :: MudStack EqMap
getPlaEqMap = getEqMap 0


getEq :: Id -> MudStack Inv
getEq i = M.elems <$> getEqMap i


getPlaEq :: MudStack Inv
getPlaEq = getEq 0


-----


getMob :: Id -> MudStack Mob
getMob i = gets (^?!mobTbl.ix i)


getMobSex :: Id -> MudStack Sex
getMobSex i = (^.sex) <$> getMob i


getPlaMobSex :: MudStack Sex
getPlaMobSex = getMobSex 0


getMobHand :: Id -> MudStack Hand
getMobHand i = (^.hand) <$> getMob i


getPlaMobHand :: MudStack Hand
getPlaMobHand = getMobHand 0


-----

getPlaRmId :: MudStack Id
getPlaRmId = gets (^.pla.rmId)


getPlaRm :: MudStack Rm
getPlaRm = getPlaRmId >>= \i ->
    gets (^?!rmTbl.ix i)


getPlaRmInv :: MudStack Inv
getPlaRmInv = getPlaRmId >>= \i ->
    gets (^?!invTbl.ix i)


getPlaRmNextRmId :: (Rm -> Id) -> MudStack (Maybe Id)
getPlaRmNextRmId dir = getPlaRm >>= tryId . dir
  where
    tryId nextId | nextId == deadEnd = return Nothing
                 | otherwise = return (Just nextId)
