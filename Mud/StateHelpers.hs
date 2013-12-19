{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.StateHelpers ( addToInv
                        , gerToMes
                        , getCloth
                        , getEnt
                        , getEntBothGramNos
                        , getEntBothGramNosInInv
                        , getEntIds
                        , getEntNamesInInv
                        , getEntSingsInInv
                        , getEntsInInv
                        , getEntsInInvByName
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
                        , makePlurFromBoth
                        , moveInv
                        , procGetEntResPlaInv
                        , procGetEntResRm
                        , remFromInv
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
import Data.List (sortBy)
import Data.Monoid (mappend)
import Data.Text.Read (decimal)
import Data.Text.Strict.Lens (packed)
import qualified Data.Map.Lazy as M
import qualified Data.Text as T


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


gerToMes :: GetEntResult -> MudStack (Maybe [Ent])
gerToMes ger = case ger of
  (Mult    _ _ (Just es)) -> return (Just es)
  (Indexed _ _ (Right e)) -> return (Just [e])
  _                       -> return Nothing


procGetEntResRm :: GetEntResult -> MudStack (Maybe [Ent])
procGetEntResRm ger = case ger of
  Sorry n                 -> output ("You don't see " <> aOrAn n <> " here.")             >> return Nothing
  (Mult 1 n Nothing)      -> output ("You don't see " <> aOrAn n <> " here.")             >> return Nothing
  (Mult _ n Nothing)      -> output ("You don't see any " <> n <> "s here.")              >> return Nothing
  (Mult _ _ (Just es))    -> return (Just es)
  (Indexed _ n (Left "")) -> output ("You don't see any " <> n <> "s here.")              >> return Nothing
  (Indexed x _ (Left p))  -> outputCon [ "You don't see ", showText x, " ", p, " here." ] >> return Nothing
  (Indexed _ _ (Right e)) -> return (Just [e])


procGetEntResPlaInv :: GetEntResult -> MudStack (Maybe [Ent])
procGetEntResPlaInv ger = case ger of
  Sorry n                 -> output ("You don't have " <> aOrAn n <> ".")             >> return Nothing
  (Mult 1 n Nothing)      -> output ("You don't have " <> aOrAn n <> ".")             >> return Nothing
  (Mult _ n Nothing)      -> output ("You don't have any " <> n <> "s.")              >> return Nothing
  (Mult _ _ (Just es))    -> return (Just es)
  (Indexed _ n (Left "")) -> output ("You don't have any " <> n <> "s.")              >> return Nothing
  (Indexed x _ (Left p))  -> outputCon [ "You don't have ", showText x, " ", p, "." ] >> return Nothing
  (Indexed _ _ (Right e)) -> return (Just [e])


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
getPlaRmNextRmId dir = tryId . dir =<< getPlaRm
  where
    tryId nextId | nextId == deadEnd = return Nothing
                 | otherwise = return (Just nextId)
