{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.StateHelpers where

import Mud.DataTypes
import Mud.Ids (deadEnd)

import Control.Lens (at)
import Control.Lens.Operators ((^.), (?=))
import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Char (isDigit)
import Data.List (delete, sort)
import Data.Maybe (fromJust)
import Data.Text.Read (decimal)
import Data.Text.Strict.Lens (packed, unpacked)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T


-- ==================================================
-- General purpose convenience methods:


(<>) :: T.Text -> T.Text -> T.Text
(<>) = T.append


showText :: (Show a) => a -> T.Text
showText = T.pack . show


aOrAn :: T.Text -> T.Text
aOrAn "" = undefined
aOrAn t
  | T.head t `elem` ("aeiou"^.unpacked) = "an " <> t
  | otherwise = "a " <> t


findAbbrev :: T.Text -> [T.Text] -> Maybe T.Text
findAbbrev needle hay = if null res then Nothing else Just . head $ res
  where
    res = sort . filter (needle `T.isPrefixOf`) $ hay


deleteAllInList :: (Eq a) => [a] -> [a] -> [a]
deleteAllInList xs ys = foldr (\x ys' -> delete x ys') ys xs


-- ==================================================
-- Convenience methods for dealing with state:


getEnt :: Id -> StateT WorldState IO Ent
getEnt i = do
    ws <- get
    return (fromJust $ ws^.entTbl.at i)


getEntIds :: [Ent] -> Inv
getEntIds es = [ e^.entId | e <- es ]


getEntsInInv :: Inv -> StateT WorldState IO [Ent]
getEntsInInv = mapM getEnt


getEntNamesInInv :: Inv -> StateT WorldState IO [T.Text]
getEntNamesInInv is = do
    es <- getEntsInInv is
    return [ e^.name | e <- es ]


type BothGramNos = (Sing, Plur)


getEntBothGramNos :: Ent -> BothGramNos
getEntBothGramNos e = (e^.sing, e^.plur)


getEntBothGramNosInInv :: Inv -> StateT WorldState IO [BothGramNos]
getEntBothGramNosInInv is = do
    es <- getEntsInInv is
    return (map getEntBothGramNos es)


makePlurFromBoth :: BothGramNos -> Plur
makePlurFromBoth (s, "") = s <> "s"
makePlurFromBoth (_, p)  = p


type Amount = Int
type Index  = Int
type NameSearchedFor = T.Text

data GetEntResult = Mult NameSearchedFor (Maybe [Ent])
                  | Indexed Index NameSearchedFor (Either Plur Ent)
                  | Sorry

allChar, amountChar, indexChar :: Char
allChar    = '\''
amountChar = '/'
indexChar  = '.'


getEntsInInvByName :: T.Text -> Inv -> StateT WorldState IO GetEntResult
getEntsInInvByName searchName is
  | searchName == ([allChar]^.packed) = liftM (Mult searchName . Just) $ getEntsInInv is
  | T.head searchName == allChar = getMultEnts (maxBound :: Int) (T.tail searchName) is
  | isDigit (T.head searchName) = let noText = T.takeWhile isDigit searchName
                                      noInt = case decimal noText of Right (i, _) -> i
                                                                     _ -> undefined
                                      rest = T.drop (T.length noText) searchName
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


getMultEnts :: Amount -> T.Text -> Inv -> StateT WorldState IO GetEntResult
getMultEnts a n is
  | a < 1 = return Sorry
  | otherwise = do
    ens <- getEntNamesInInv is
    case findAbbrev n ens of Nothing -> return (Mult n Nothing)
                             Just fullName -> liftM (Mult n . Just . findMatchingEnts fullName) $ getEntsInInv is
  where
    findMatchingEnts fn = take a . filter (\e -> e^.name == fn)


getIndexedEnt :: Index -> T.Text -> Inv -> StateT WorldState IO GetEntResult
getIndexedEnt x n is
  | x < 1 = return Sorry
  | otherwise = do
    ens <- getEntNamesInInv is
    case findAbbrev n ens of Nothing -> return (Indexed x n . Left $ "")
                             Just fullName -> do
                                 es <- getEntsInInv is
                                 let matches = filter (\e -> e^.name == fullName) es
                                 if length matches < x
                                   then let both = getEntBothGramNos (head matches)
                                        in return (Indexed x n . Left . makePlurFromBoth $ both)
                                   else return (Indexed x n . Right $ matches !! (x - 1))


procGetEntResRm :: T.Text -> GetEntResult -> StateT WorldState IO (Maybe [Ent])
procGetEntResRm r res = case res of
  Sorry                   -> lift $ T.putStrLn ("You don't see " <> aOrAn r <> " here.") >> return Nothing
  (Mult n Nothing)        -> lift $ T.putStrLn ("You don't see any " <> n <> "s here.") >> return Nothing
  (Mult _ (Just es))      -> return (Just es)
  (Indexed _ n (Left "")) -> lift $ T.putStrLn ("You don't see any " <> n <> "s here.") >> return Nothing
  (Indexed x _ (Left p))  -> lift $ T.putStrLn ("You don't see " <> showText x <> " " <> p <> " here.") >> return Nothing
  (Indexed _ _ (Right e)) -> return (Just [e])


procGetEntResPlaInv :: T.Text -> GetEntResult -> StateT WorldState IO (Maybe [Ent])
procGetEntResPlaInv r res = case res of
  Sorry                   -> lift $ T.putStrLn ("You don't have " <> aOrAn r <> ".") >> return Nothing
  (Mult n Nothing)        -> lift $ T.putStrLn ("You don't have any " <> n <> "s.") >> return Nothing
  (Mult _ (Just es))      -> return (Just es)
  (Indexed _ n (Left "")) -> lift $ T.putStrLn ("You don't have any " <> n <> "s.") >> return Nothing
  (Indexed x _ (Left p))  -> lift $ T.putStrLn ("You don't have " <> showText x <> " " <> p <> ".") >> return Nothing
  (Indexed _ _ (Right e)) -> return (Just [e])


type ConName = T.Text


procGetEntResCon :: ConName -> T.Text -> GetEntResult -> StateT WorldState IO (Maybe [Ent])
procGetEntResCon cn r res = case res of
  Sorry                   -> lift $ T.putStrLn ("The " <> cn <> " doesn't contain " <> aOrAn r <> ".") >> return Nothing
  (Mult n Nothing)        -> lift $ T.putStrLn ("The " <> cn <> " doesn't contain any " <> n <> "s.") >> return Nothing
  (Mult _ (Just es))      -> return (Just es)
  (Indexed _ n (Left "")) -> lift $ T.putStrLn ("The " <> cn <> " doesn't contain any " <> n <> "s.") >> return Nothing
  (Indexed x _ (Left p))  -> lift $ T.putStrLn ("The " <> cn <> " doesn't contain " <> showText x <> " " <> p <> ".") >> return Nothing
  (Indexed _ _ (Right e)) -> return (Just [e])


getEntType :: Ent -> StateT WorldState IO Type
getEntType e = do
    ws <- get
    let i = e^.entId
    return (fromJust $ ws^.typeTbl.at i)


getInv :: Id -> StateT WorldState IO Inv
getInv i = do
    ws <- get
    return (fromJust $ ws^.invTbl.at i)


getEqMap :: Id -> StateT WorldState IO EqMap
getEqMap i = do
    ws <- get
    return (fromJust $ ws^.eqTbl.at i)


getEq :: Id -> StateT WorldState IO Inv
getEq i = do
    em <- getEqMap i
    return (M.elems em)


addToInv :: Inv -> Id -> StateT WorldState IO ()
addToInv is to = do
    toIs <- getInv to
    invTbl.at to ?= toIs ++ is


remFromInv :: Inv -> Id -> StateT WorldState IO ()
remFromInv is from = do
    fromIs <- getInv from
    invTbl.at from ?= (deleteAllInList is fromIs)


moveInv :: Inv -> Id -> Id -> StateT WorldState IO ()
moveInv [] _ _ = return ()
moveInv is from to = remFromInv is from >> addToInv is to


getPlaInv :: StateT WorldState IO Inv
getPlaInv = getInv 0


getPlaEq :: StateT WorldState IO Inv
getPlaEq = getEq 0


getPlaRmId :: StateT WorldState IO Id
getPlaRmId = do
    ws <- get
    return (ws^.pla.rmId)


getPlaRm :: StateT WorldState IO Rm
getPlaRm = do
    ws <- get
    i <- getPlaRmId
    return (fromJust $ ws^.rmTbl.at i)


getPlaRmInv :: StateT WorldState IO Inv
getPlaRmInv = do
    ws <- get
    i <- getPlaRmId
    return (fromJust $ ws^.invTbl.at i)


getPlaRmNextRmId :: (Rm -> Id) -> StateT WorldState IO (Maybe Id)
getPlaRmNextRmId dir = getPlaRm >>= tryId . dir
  where
    tryId nextId | nextId == deadEnd = return Nothing
                 | otherwise = return (Just nextId)
