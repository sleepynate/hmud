{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.StateHelpers where

import Mud.Convenience
import Mud.Ids (deadEnd)
import Mud.MiscDataTypes
import Mud.StateDataTypes

import Control.Applicative
import Control.Lens (_1, at, ix)
import Control.Lens.Operators ((^.), (^?!), (?=))
import Control.Monad.Trans.State
import Data.Char (isDigit)
import Data.List (sortBy)
import Data.Monoid (mappend)
import Data.Text.Read (decimal)
import Data.Text.Strict.Lens (packed, unpacked)
import qualified Data.Map.Strict as M
import qualified Data.Text as T


getEnt :: Id -> MudStack Ent
getEnt i = gets (^?!entTbl.ix i)


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


allChar, amountChar, indexChar, slotChar :: Char
allChar    = '\''
amountChar = '/'
indexChar  = '.'
slotChar   = ':'


getEntsInInvByName :: T.Text -> Inv -> MudStack GetEntResult
getEntsInInvByName searchName is
  | searchName == [allChar]^.packed = (Mult searchName . Just) <$> getEntsInInv is
  | T.head searchName == allChar = getMultEnts (maxBound :: Int) (T.tail searchName) is
  | isDigit (T.head searchName) = let noText = T.takeWhile isDigit searchName
                                      noInt  = either undefined (^._1) $ decimal noText
                                      rest   = T.drop (T.length noText) searchName
                                  in parse rest noInt
  | otherwise = getMultEnts 1 searchName is
  where
    parse rest noInt 
      | T.length rest < 2 = return Sorry
      | otherwise = let delim = T.head rest
                        rest' = T.tail rest
                    in case () of _ | delim == amountChar -> getMultEnts   noInt rest' is -- TODO: Change to a multiway if.
                                    | delim == indexChar  -> getIndexedEnt noInt rest' is
                                    | otherwise -> return Sorry


getMultEnts :: Amount -> T.Text -> Inv -> MudStack GetEntResult
getMultEnts a n is
  | a < 1     = return Sorry
  | otherwise = getEntNamesInInv is >>= maybe notFound found . findAbbrev n
  where
    notFound = return (Mult n Nothing)
    found fullName = (Mult n . Just . takeMatchingEnts fullName) <$> getEntsInInv is
    takeMatchingEnts fn = take a . filter (\e -> e^.name == fn)


getIndexedEnt :: Index -> T.Text -> Inv -> MudStack GetEntResult
getIndexedEnt x n is
  | x < 1     = return Sorry
  | otherwise = getEntNamesInInv is >>= maybe notFound found . findAbbrev n
  where
    notFound = return (Indexed x n (Left ""))
    found fullName = filter (\e -> e^.name == fullName) <$> getEntsInInv is >>= \matches ->
        if length matches < x
          then let both = getEntBothGramNos $ head matches
               in return (Indexed x n (Left $ makePlurFromBoth both))
          else return (Indexed x n (Right $ matches !! (x - 1)))


procGetEntResRm :: T.Text -> GetEntResult -> MudStack (Maybe [Ent])
procGetEntResRm r res = case res of
  Sorry                   -> output ("You don't see " <> aOrAn r <> " here.")             >> return Nothing
  (Mult n Nothing)        -> output ("You don't see any " <> n <> "s here.")              >> return Nothing
  (Mult _ (Just es))      -> return (Just es)
  (Indexed _ n (Left "")) -> output ("You don't see any " <> n <> "s here.")              >> return Nothing
  (Indexed x _ (Left p))  -> outputCon [ "You don't see ", showText x, " ", p, " here." ] >> return Nothing
  (Indexed _ _ (Right e)) -> return (Just [e])


procGetEntResPlaInv :: T.Text -> GetEntResult -> MudStack (Maybe [Ent])
procGetEntResPlaInv r res = case res of
  Sorry                   -> output ("You don't have " <> aOrAn r <> ".")             >> return Nothing
  (Mult n Nothing)        -> output ("You don't have any " <> n <> "s.")              >> return Nothing
  (Mult _ (Just es))      -> return (Just es)
  (Indexed _ n (Left "")) -> output ("You don't have any " <> n <> "s.")              >> return Nothing
  (Indexed x _ (Left p))  -> outputCon [ "You don't have ", showText x, " ", p, "." ] >> return Nothing
  (Indexed _ _ (Right e)) -> return (Just [e])


procGetEntResCon :: ConName -> T.Text -> GetEntResult -> MudStack (Maybe [Ent])
procGetEntResCon cn r res = case res of
  Sorry                   -> outputCon [ "The ", cn, " doesn't contain ", aOrAn r, "." ]            >> return Nothing
  (Mult n Nothing)        -> outputCon [ "The ", cn, " doesn't contain any ", n, "s." ]             >> return Nothing
  (Mult _ (Just es))      -> return (Just es)
  (Indexed _ n (Left "")) -> outputCon [ "The ", cn, " doesn't contain any ", n, "s." ]             >> return Nothing
  (Indexed x _ (Left p))  -> outputCon [ "The ", cn, " doesn't contain ", showText x, " ", p, "." ] >> return Nothing
  (Indexed _ _ (Right e)) -> return (Just [e])


getEntToReadyByName :: T.Text -> MudStack (Maybe Ent, Maybe RightOrLeft)
getEntToReadyByName searchName
  | slotChar `elem` searchName^.unpacked = let (xs, ys) = T.break (== slotChar) searchName
                                           in if T.length ys == 1 then sorry else findEntToReady xs >>= \me ->
                                              case (T.toLower . T.tail $ ys) of "r"   -> return (me, Just R)
                                                                                "l"   -> return (me, Just L)
                                                                                "rif" -> return (me, Just RIF)
                                                                                "rmf" -> return (me, Just RMF)
                                                                                "rrf" -> return (me, Just RRF)
                                                                                "rpf" -> return (me, Just RPF)
                                                                                "lif" -> return (me, Just LIF)
                                                                                "lmf" -> return (me, Just LMF)
                                                                                "lrf" -> return (me, Just LRF)
                                                                                "lpf" -> return (me, Just LPF)
                                                                                _     -> sorry
  | otherwise = findEntToReady searchName >>= \me ->
      return (me, Nothing)
  where
    sorry = outputCon [ "Please specify ", dblQuote slotR, " or ", dblQuote slotL, ".\n", ringHelp ] >> return (Nothing, Nothing)


ringHelp :: T.Text
ringHelp = T.concat [ "For rings, specify ", dblQuote slotR, " or ", dblQuote slotL, " immediately followed by:\n"
                    , dblQuote "if", " for index finger,\n"
                    , dblQuote "mf", " for middle finter,\n"
                    , dblQuote "rf", " for ring finger,\n"
                    , dblQuote "pf", " for pinky finger." ]


slotR, slotL :: T.Text
slotR = (slotChar : "r")^.packed
slotL = (slotChar : "l")^.packed


findEntToReady :: T.Text -> MudStack (Maybe Ent)
findEntToReady searchName = getPlaInv >>= getEntsInInvByName searchName >>= procGetEntResPlaInv searchName >>= \mes ->
    case mes of Just [e]   -> return (Just e)
                Just (e:_) -> return (Just e) -- TODO: Can this be handled a better way?
                Nothing    -> return Nothing
                _          -> undefined


getEntType :: Ent -> MudStack Type
getEntType e = let i = e^.entId
               in gets (^?!typeTbl.ix i)


getCloth :: Id -> MudStack Cloth
getCloth i = gets (^?!clothTbl.ix i)


getWpn :: Id -> MudStack Wpn
getWpn i = gets (^?!wpnTbl.ix i)


getInv :: Id -> MudStack Inv
getInv i = gets (^?!invTbl.ix i)


getPlaInv :: MudStack Inv
getPlaInv = getInv 0


addToInv :: Inv -> Id -> MudStack ()
addToInv is ti = getInv ti >>= sortInv . (++ is) >>= (invTbl.at ti ?=)


sortInv :: Inv -> MudStack Inv
sortInv is = fmap (map (^._1) . sortBy nameThenSing) $ zip3 is <$> getEntNamesInInv is <*> getEntSingsInInv is
  where
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') `mappend` (s `compare` s')


remFromInv :: Inv -> Id -> MudStack ()
remFromInv is fi = getInv fi >>= \fis ->
    invTbl.at fi ?= (deleteAllInList is fis)


moveInv :: Inv -> Id -> Id -> MudStack ()
moveInv [] _  _  = return ()
moveInv is fi ti = remFromInv is fi >> addToInv is ti


getEqMap :: Id -> MudStack EqMap
getEqMap i = gets (^?!eqTbl.ix i)


getPlaEqMap :: MudStack EqMap
getPlaEqMap = getEqMap 0


getEq :: Id -> MudStack Inv
getEq i = M.elems <$> getEqMap i


getPlaEq :: MudStack Inv
getPlaEq = getEq 0


getMob :: Id -> MudStack Mob
getMob i = gets (^?!mobTbl.ix i)


getMobHand :: Id -> MudStack Hand
getMobHand i = (^.hand) <$> getMob i


getPlaMobHand :: MudStack Hand
getPlaMobHand = getMobHand 0


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
