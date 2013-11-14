{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Mud.Main (main) where

import Mud.DataTypes
import Mud.StateHelpers
import Mud.TheWorld

import Control.Lens.Operators ((^.), (.=))
import Control.Monad (void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Text.Strict.Lens (packed)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.Readline (readline)
import System.Exit (exitSuccess)


-- ==================================================
-- Top level definitions and convenience methods:


nl, tab :: Char
nl  = '\n'
tab = '\t'


newLine :: IO ()
newLine = putChar $ nl


quote :: T.Text -> T.Text
quote s = "\"" <> s <> "\""


-- ==================================================
-- Structures relating to user commands:


type Input   = (CmdName, Rest)
type CmdName = T.Text
type Rest    = [T.Text]

type Action = Rest -> StateT WorldState IO ()

data Cmd = Cmd { cmdName :: CmdName
               , action  :: Action
               , cmdDesc :: T.Text }

cmdList :: [Cmd]
cmdList = [ Cmd { cmdName = "",  action = const game, cmdDesc = "" }
          , Cmd { cmdName = "?", action = \_ -> lift help, cmdDesc = "Display help." }
          , Cmd { cmdName = "n", action = go "n", cmdDesc = "Go north." }
          , Cmd { cmdName = "s", action = go "s", cmdDesc = "Go south." }
          , Cmd { cmdName = "e", action = go "e", cmdDesc = "Go east." }
          , Cmd { cmdName = "w", action = go "w", cmdDesc = "Go west." }
          , Cmd { cmdName = "u", action = go "u", cmdDesc = "Go up." }
          , Cmd { cmdName = "d", action = go "d", cmdDesc = "Go down." }
          , Cmd { cmdName = "look", action = look, cmdDesc = "Look." }
          , Cmd { cmdName = "inventory", action = inventory, cmdDesc = "Inventory." }
          , Cmd { cmdName = "get", action = getAction, cmdDesc = "Pick items up off the ground." }
          , Cmd { cmdName = "drop", action = dropAction, cmdDesc = "Drop items on the ground." }
          , Cmd { cmdName = "put", action = putAction, cmdDesc = "Put items in a container." }
          , Cmd { cmdName = "remove", action = remove, cmdDesc = "Remove items from a container." }
          , Cmd { cmdName = "okapi", action = okapi, cmdDesc = "Make an okapi." }
          , Cmd { cmdName = "quit", action = \_ -> lift exitSuccess, cmdDesc = "Quit." } ]


-- ==================================================
-- Main and related functions:


main :: IO ()
main = welcomeMsg >> execStateT createWorld initWS >>= evalStateT game


welcomeMsg :: IO ()
welcomeMsg = T.putStrLn "Welcome to the game!\n"


game :: StateT WorldState IO ()
game = do
    ms <- lift . readline $ "> "
    case parseInp (fromJust ms ^.packed) of Nothing  -> game
                                            Just inp -> dispatch inp


parseInp :: T.Text -> Maybe Input
parseInp = splitUp . T.words
  where
    splitUp []     = Nothing
    splitUp [x]    = Just (x, [""])
    splitUp (x:xs) = Just (x, xs)


dispatch :: Input -> StateT WorldState IO ()
dispatch (cn, rest) = case findAction cn of Nothing -> what >> next -- Haha.
                                            Just a  -> a rest >> next
  where
    what = lift . T.putStrLn $ "?"
    next = lift newLine >> game


findAction :: CmdName -> Maybe Action
findAction cn = case findAbbrev cn' cns of
  Nothing -> Nothing
  Just fn -> Just . action . findCmdForFullName $ fn
  where
    cn' = T.toLower cn
    cns = map cmdName cmdList
    findCmdForFullName fn = head . filter ((== fn) . cmdName) $ cmdList


-- ==================================================
-- User commands:


help :: IO ()
help = T.putStrLn . T.init . T.unlines . reverse . T.lines . foldl makeTxtForCmd "" $ cmdList
  where
    makeTxtForCmd txt c = T.concat [cmdName c, [tab]^.packed, cmdDesc c, [nl]^.packed, txt]


go :: T.Text -> Action
go dir [""] = goDispatcher [dir]
go dir rs   = goDispatcher (dir : rs)


goDispatcher :: Action
goDispatcher [r]    = void . tryMove $ r
goDispatcher (r:rs) = tryMove r >> lift newLine >> goDispatcher rs
goDispatcher _ = undefined


tryMove :: T.Text -> StateT WorldState IO ()
tryMove dir = let dir' = T.toLower dir
              in case M.lookup dir' dirMap of
                Nothing -> lift . T.putStrLn . quote $ dir <> " is not a valid direction."
                Just f  -> movePla f
  where
    movePla f = do
        mi <- getPlaRmNextRmId f
        case mi of Nothing -> lift . T.putStrLn $ "You can't go that way."
                   Just i  -> pla.rmId .= i >> look [""]


dirMap :: M.Map T.Text (Room -> Id)
dirMap = M.fromList [("n", north), ("s", south), ("e", east), ("w", west), ("u", up), ("d", down)]


look :: Action
look [""] = do
    r <- getPlaRm
    lift . T.putStrLn . T.concat $ [r^.name, [nl]^.packed, r^.desc]
    getPlaRmInv >>= dispRmInv
look [r] = getPlaRmInv >>= getEntsInInvByName r >>= procGetEntResRm r >>= flip F.forM_ (mapM_ descEnt)
look (r:rs) = look [r] >> look rs
look _ = undefined


dispRmInv :: Inv -> StateT WorldState IO ()
dispRmInv is = do
    ens <- getEntBothGramNosInInv is
    mapM_ descEntInRm . nub . zip (makeCountList ens) $ ens
  where
    descEntInRm (x, (s, _)) | x == 1 = lift . T.putStrLn $ "There is " <> aOrAn s <> " here."
    descEntInRm (x, both) = lift . T.putStrLn $ "There are " <> showText x <> " " <> makePlurFromBoth both <> " here."


makeCountList :: (Eq a) => [a] -> [Int]
makeCountList xs = [ length (filter (==x) xs) | x <- xs ]


descEnt :: Ent -> StateT WorldState IO ()
descEnt e = do
  lift . T.putStrLn $ e^.desc
  t <- getEntType e
  when (t == ConType) $ descEntsInInvForId (e^.entId)


descEntsInInvForId :: Id -> StateT WorldState IO ()
descEntsInInvForId i = do
    ens <- getInv i >>= getEntBothGramNosInInv
    case ens of [] -> empty
                _  -> header >> (mapM_ descEntInInv . nub . zip (makeCountList ens) $ ens)
  where
    empty
      | i == 0 = lift . T.putStrLn $ "You aren't carrying anything."
      | otherwise = do { e <- getEnt i; lift . T.putStrLn $ "The " <> (e^.sing) <> " is empty." }
    header
      | i == 0 = lift . T.putStrLn $ "You are carrying:"
      | otherwise = do { e <- getEnt i; lift . T.putStrLn $ "The " <> (e^.sing) <> " contains:" }
    descEntInInv (x, (s, _)) | x == 1 = lift . T.putStrLn $ "1 " <> s
    descEntInInv (x, both) = lift . T.putStrLn $ showText x <> " " <> makePlurFromBoth both


inventory :: Action
inventory [""] = descEntsInInvForId 0
inventory [r] = getPlaInv >>= getEntsInInvByName r >>= procGetEntResPlaInv r >>= flip F.forM_ (mapM_ descEnt)
inventory (r:rs) = inventory [r] >> inventory rs
inventory _ = undefined


getAction :: Action
getAction [""] = lift . T.putStrLn $ "What do you want to get?"
getAction [r] = do
    mes <- getPlaRmInv >>= getEntsInInvByName r >>= procGetEntResRm r
    case mes of Nothing -> return ()
                Just es -> do
                    i <- getPlaRmId
                    moveInv (getEntIds es) i 0 >> (lift . T.putStrLn $ "Ok.")
getAction (r:rs) = getAction [r] >> getAction rs
getAction _ = undefined


dropAction :: Action
dropAction [""] = lift . T.putStrLn $ "What do you want to drop?"
dropAction [r] = do
    mes <- getPlaInv >>= getEntsInInvByName r >>= procGetEntResPlaInv r
    case mes of Nothing -> return ()
                Just es -> do
                    i <- getPlaRmId
                    moveInv (getEntIds es) 0 i >> (lift . T.putStrLn $ "Ok.")
dropAction (r:rs) = dropAction [r] >> dropAction rs
dropAction _ = undefined


data PutOrRem = Put | Rem


putAction :: Action
putAction [""] = lift . T.putStrLn $ "What do you want to put? And where do you want to put it?"
putAction [_]  = lift . T.putStrLn $ "Where do you want to put it?"
putAction rs   = putRemDispatcher Put rs


remove :: Action
remove [""] = lift . T.putStrLn $ "What do you want to remove? And what do you want to remove it from?"
remove [_]  = lift . T.putStrLn $ "What do you want to remove it from?"
remove rs   = putRemDispatcher Rem rs


rmChar :: Char
rmChar = '-'


putRemDispatcher :: PutOrRem -> Action
putRemDispatcher por (r:rs) = do
    mes <- findCon $ last rs
    case mes of Nothing -> return ()
                Just es -> if length es /= 1
                             then lift . T.putStrLn $ onlyOneMsg
                             else do
                                let e = head es
                                t <- getEntType e
                                if t /= ConType
                                  then void (lift . T.putStrLn $ "The " <> (e^.sing) <> " isn't a container.")
                                  else dispatchToHelper (e^.entId)
  where
    findCon cn
      | T.head cn == rmChar = getPlaRmInv >>= getEntsInInvByName (T.tail cn) >>= procGetEntResRm (T.tail cn)
      | otherwise = getPlaInv >>= getEntsInInvByName cn >>= procGetEntResPlaInv cn
    onlyOneMsg = case por of Put -> "You can only put things into one container at a time."
                             Rem -> "You can only remove things from one container at a time."
    dispatchToHelper i = case por of Put -> putHelper i restWithoutCon 
                                     Rem -> remHelper i restWithoutCon
      where
        restWithoutCon = r : (init rs)
putRemDispatcher _ _ = undefined


putHelper :: Id -> Rest -> StateT WorldState IO ()
putHelper _ [] = return ()
putHelper ci (r:rs) = do
    mes <- getPlaInv >>= getEntsInInvByName r >>= procGetEntResPlaInv r
    case mes of Nothing -> putHelper ci rs
                Just es -> do
                    let is = getEntIds es
                    if ci `elem` is
                      then do
                          e <- getEnt ci
                          lift . T.putStrLn $ "You can't put the " <> (e^.sing) <> " inside itself."
                          let is' = filter (/= ci) is
                          case length is' of 0 -> putHelper ci rs
                                             _ -> moveInv is' 0 ci >> (lift . T.putStrLn $ "Ok.") >> putHelper ci rs
                      else moveInv is 0 ci >> (lift . T.putStrLn $ "Ok.") >> putHelper ci rs


remHelper :: Id -> Rest -> StateT WorldState IO ()
remHelper _ [] = return ()
remHelper ci (r:rs) = do
    e <- getEnt ci
    fromIs <- getInv ci
    if null fromIs
      then lift . T.putStrLn $ "The " <> (e^.sing) <> " appears to be empty."
      else do
          mes <- getEntsInInvByName r fromIs >>= procGetEntResCon (e^.sing) r
          case mes of Nothing -> remHelper ci rs
                      Just es -> moveInv (getEntIds es) ci 0 >> (lift . T.putStrLn $ "Ok.") >> remHelper ci rs


okapi :: Action
okapi _ = do
    i <- mkOkapi
    lift . T.putStrLn $ "Made okapi with id " <> showText i <> "."
