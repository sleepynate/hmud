{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

module Mud.StateHelpers where

import Mud.DataTypes
import Mud.Ids (deadEnd)

import Control.Lens (at, to)
import Control.Lens.Operators ((^.), (?=))
import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Char (isDigit)
import Data.List (delete, sort, sortBy)
import Data.Maybe (fromJust)
import Data.Monoid (mappend)
import Data.Text.Strict.Lens (packed, unpacked)
import Data.Text.Read (decimal)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T


-- ==================================================
-- General purpose convenience methods:


infixr 7 <>
(<>) :: T.Text -> T.Text -> T.Text
(<>) = T.append


showText :: (Show a) => a -> T.Text
showText a = show a ^.packed


aOrAn :: T.Text -> T.Text
aOrAn "" = undefined
aOrAn t
  | T.head t `elem` ("aeiou"^.unpacked) = "an " <> t
  | otherwise = "a " <> t


quote :: T.Text -> T.Text
quote t = T.concat ["\"", t, "\""]


bracketQuote :: T.Text -> T.Text
bracketQuote t = T.concat ["[", t, "]"]


unquote :: T.Text -> T.Text
unquote = T.init . T.tail


bracketPad :: Int -> T.Text -> T.Text
bracketPad x t = bracketQuote t' <> T.replicate p " "
  where
    t' = T.take (x - 3) t
    p  = x - (T.length t') - 2


fstOf3 :: (a, b, c) -> a
fstOf3 (a, _, _) = a


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
    return (ws^.entTbl.at i.to fromJust)


getEntIds :: [Ent] -> Inv
getEntIds es = [ e^.entId | e <- es ]


getEntsInInv :: Inv -> StateT WorldState IO [Ent]
getEntsInInv = mapM getEnt


getEntNamesInInv :: Inv -> StateT WorldState IO [T.Text]
getEntNamesInInv is = do
    es <- getEntsInInv is
    return [ e^.name | e <- es ]


getEntSingsInInv :: Inv -> StateT WorldState IO [T.Text]
getEntSingsInInv is = do
    es <- getEntsInInv is
    return [ e^.sing | e <- es ]


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

allChar, amountChar, indexChar, slotChar :: Char
allChar    = '\''
amountChar = '/'
indexChar  = '.'
slotChar   = ':'


getEntsInInvByName :: T.Text -> Inv -> StateT WorldState IO GetEntResult
getEntsInInvByName searchName is
  | searchName == [allChar]^.packed = liftM (Mult searchName . Just) $ getEntsInInv is
  | T.head searchName == allChar = getMultEnts (maxBound :: Int) (T.tail searchName) is
  | isDigit (T.head searchName) = let noText = T.takeWhile isDigit searchName
                                      noInt = either undefined fst $ decimal noText
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
  | a < 1     = return Sorry
  | otherwise = getEntNamesInInv is >>= maybe notFound found . findAbbrev n
  where
    notFound = return (Mult n Nothing)
    found fullName = liftM (Mult n . Just . takeMatchingEnts fullName) $ getEntsInInv is
    takeMatchingEnts fn = take a . filter (\e -> e^.name == fn)


getIndexedEnt :: Index -> T.Text -> Inv -> StateT WorldState IO GetEntResult
getIndexedEnt x n is
  | x < 1     = return Sorry
  | otherwise = getEntNamesInInv is >>= maybe notFound found . findAbbrev n
  where
    notFound = return (Indexed x n . Left $ "")
    found fullName = do
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


data RightOrLeft = R
                 | L
                 | RIF | RMF | RRF | RPF
                 | LIF | LMF | LRF | LPF


getEntToReadyByName :: T.Text -> StateT WorldState IO (Maybe Ent, Maybe RightOrLeft) -- TODO: Refactor?
getEntToReadyByName searchName
  | slotChar `elem` searchName^.unpacked = do
      let (xs, ys) = T.break (== slotChar) searchName
      if T.length ys == 1 then sorry else do
          me <- findEntToReady xs
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
  | otherwise = do
      me <- findEntToReady searchName
      return (me, Nothing)
  where
    sorry = lift $ T.putStrLn sorryMsg >> return (Nothing, Nothing)
    sorryMsg = T.concat [ "Please specify ", quote slotR, " or ", quote slotL, ".\n", ringHelp ]


ringHelp :: T.Text
ringHelp = T.concat [ "For rings, specify ", quote slotR, " or ", quote slotL, " immediately followed by:\n"
                    , quote "if", " for index finger,\n"
                    , quote "mf", " for middle finter,\n"
                    , quote "rf", " for ring finger,\n"
                    , quote "pf", " for pinky finger." ]


slotR, slotL :: T.Text
slotR = (slotChar : "r")^.packed
slotL = (slotChar : "l")^.packed


findEntToReady :: T.Text -> StateT WorldState IO (Maybe Ent) -- TODO: Refactor?
findEntToReady searchName = do
    mes <- getPlaInv >>= getEntsInInvByName searchName >>= procGetEntResPlaInv searchName
    case mes of Just [e]   -> return (Just e)
                Just (e:_) -> return (Just e)
                Nothing    -> return Nothing
                _ -> undefined


getEntType :: Ent -> StateT WorldState IO Type
getEntType e = do
    ws <- get
    let i = e^.entId
    return (ws^.typeTbl.at i.to fromJust)


getCloth :: Id -> StateT WorldState IO Cloth
getCloth i = do
    ws <- get
    return (ws^.clothTbl.at i.to fromJust)


getWpn :: Id -> StateT WorldState IO Wpn
getWpn i = do
    ws <- get
    return (ws^.wpnTbl.at i.to fromJust)


getInv :: Id -> StateT WorldState IO Inv
getInv i = do
    ws <- get
    return (ws^.invTbl.at i.to fromJust)


getPlaInv :: StateT WorldState IO Inv
getPlaInv = getInv 0


addToInv :: Inv -> Id -> StateT WorldState IO ()
addToInv is ti = do
    tis <- getInv ti
    is' <- sortInv $ tis ++ is
    invTbl.at ti ?= is'


sortInv :: Inv -> StateT WorldState IO Inv
sortInv is = do
    names <- getEntNamesInInv is
    sings <- getEntSingsInInv is
    let ins = zip3 is names sings
    return (map fstOf3 $ sortBy nameThenSing ins)
  where
    nameThenSing (_, n, s) (_, n', s') = (n `compare` n') `mappend` (s `compare` s')


remFromInv :: Inv -> Id -> StateT WorldState IO ()
remFromInv is fi = do
    fis <- getInv fi
    invTbl.at fi ?= (deleteAllInList is fis)


moveInv :: Inv -> Id -> Id -> StateT WorldState IO ()
moveInv [] _ _   = return ()
moveInv is fi ti = remFromInv is fi >> addToInv is ti


getEqMap :: Id -> StateT WorldState IO EqMap
getEqMap i = do
    ws <- get
    return (ws^.eqTbl.at i.to fromJust)


getPlaEqMap :: StateT WorldState IO EqMap
getPlaEqMap = getEqMap 0


getEq :: Id -> StateT WorldState IO Inv
getEq i = do
    em <- getEqMap i
    return (M.elems em)


getPlaEq :: StateT WorldState IO Inv
getPlaEq = getEq 0


getMob :: Id -> StateT WorldState IO Mob
getMob i = do
    ws <- get
    return (ws^.mobTbl.at i.to fromJust)


getMobHand :: Id -> StateT WorldState IO Hand
getMobHand i = do
    m <- getMob i
    return (m^.hand)


getPlaMobHand :: StateT WorldState IO Hand
getPlaMobHand = getMobHand 0


getPlaRmId :: StateT WorldState IO Id
getPlaRmId = do
    ws <- get
    return (ws^.pla.rmId)


getPlaRm :: StateT WorldState IO Rm
getPlaRm = do
    ws <- get
    i <- getPlaRmId
    return (ws^.rmTbl.at i.to fromJust)


getPlaRmInv :: StateT WorldState IO Inv
getPlaRmInv = do
    ws <- get
    i <- getPlaRmId
    return (ws^.invTbl.at i.to fromJust)


getPlaRmNextRmId :: (Rm -> Id) -> StateT WorldState IO (Maybe Id)
getPlaRmNextRmId dir = getPlaRm >>= tryId . dir
  where
    tryId nextId | nextId == deadEnd = return Nothing
                 | otherwise = return (Just nextId)
