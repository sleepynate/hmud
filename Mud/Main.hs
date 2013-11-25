{-# OPTIONS_GHC -Wall -Werror -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Main (main) where

import Mud.DataTypes
import Mud.StateHelpers
import Mud.TheWorld

import Control.Arrow (first)
import Control.Lens (at, to)
import Control.Lens.Operators ((&), (^.), (?~), (.=), (?=))
import Control.Monad (forM_, when, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Char (toUpper)
import Data.Foldable (traverse_)
import Data.List (delete, nub, sort)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text.Strict.Lens (packed, unpacked)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.Readline (readline)
import System.Directory (getDirectoryContents, getTemporaryDirectory, removeFile, setCurrentDirectory)
import System.Environment (getEnv, getEnvironment, getProgName)
import System.Exit (exitSuccess)
import System.IO
import System.Process (readProcess)


-- ==================================================
-- Top level definitions and convenience methods:


ver :: T.Text
ver = "0.0 2013-10"


mudDir :: FilePath
mudDir = "/Users/stolaruk/Haskell/hmud/Mud/"^.unpacked


helpDir :: FilePath
helpDir = mudDir ++ "help/"


nl, tab :: Char
nl  = '\n'
tab = '\t'


newLine :: IO ()
newLine = putChar nl


ok :: IO ()
ok = T.putStrLn "Ok."


dumpAssocList :: (Show a, Show b) => [(a, b)] -> IO ()
dumpAssocList = mapM_ dump
  where
    dump (a, b) = T.putStrLn $ (unquote . showText $ a) <> " : " <> showText b


mkOrdinal :: Int -> T.Text
mkOrdinal 0  = undefined
mkOrdinal 11 = "11th"
mkOrdinal 12 = "12th"
mkOrdinal 13 = "13th"
mkOrdinal x  = let t = showText x
               in t <> case T.last t of '1' -> "st"
                                        '2' -> "nd"
                                        '3' -> "rd"
                                        _   -> "th"


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
          , Cmd { cmdName = "?", action = \_ -> lift dispCmdList, cmdDesc = "Display this command list." }
          , Cmd { cmdName = "help", action = help, cmdDesc = "Get help on a topic or command." }
          , Cmd { cmdName = "what", action = what, cmdDesc = "Determine what an abbreviation may refer to." }
          , Cmd { cmdName = "n", action = go "n", cmdDesc = "Go north." }
          , Cmd { cmdName = "s", action = go "s", cmdDesc = "Go south." }
          , Cmd { cmdName = "e", action = go "e", cmdDesc = "Go east." }
          , Cmd { cmdName = "w", action = go "w", cmdDesc = "Go west." }
          , Cmd { cmdName = "u", action = go "u", cmdDesc = "Go up." }
          , Cmd { cmdName = "d", action = go "d", cmdDesc = "Go down." }
          , Cmd { cmdName = "look", action = look, cmdDesc = "Look." }
          , Cmd { cmdName = "inv", action = inv, cmdDesc = "Inventory." }
          , Cmd { cmdName = "equip", action = equip, cmdDesc = "Equipment." }
          , Cmd { cmdName = "get", action = getAction, cmdDesc = "Pick items up off the ground." }
          , Cmd { cmdName = "drop", action = dropAction, cmdDesc = "Drop items on the ground." }
          , Cmd { cmdName = "put", action = putAction, cmdDesc = "Put items in a container." }
          , Cmd { cmdName = "remove", action = remove, cmdDesc = "Remove items from a container." }
          , Cmd { cmdName = "ready", action = ready, cmdDesc = "Ready items." }
          , Cmd { cmdName = "unready", action = unready, cmdDesc = "Unready items." }
          , Cmd { cmdName = "okapi", action = okapi, cmdDesc = "Make an okapi." }
          , Cmd { cmdName = "buffer", action = buffCheck, cmdDesc = "Confirm the default buffering mode." }
          , Cmd { cmdName = "env", action = dumpEnv, cmdDesc = "Dump system environment variables." }
          , Cmd { cmdName = "uptime", action = uptime, cmdDesc = "Display system uptime." }
          , Cmd { cmdName = "quit", action = \_ -> lift exitSuccess, cmdDesc = "Quit." } ]


-- ==================================================
-- Main and related functions:


main :: IO ()
main = setCurrentDirectory mudDir >> welcomeMsg >> execStateT createWorld initWS >>= evalStateT game


welcomeMsg :: IO ()
welcomeMsg = do
    un <- getEnv "USER"
    mn <- whatsMyName
    T.putStrLn $ "Hello, " <> un^.packed <> ". Welcome to " <> quote mn <> " ver " <> ver <> ".\n"
  where
    whatsMyName = getProgName >>= \mn -> return $ if mn == "<interactive>" then "why u no compile me?" else mn^.packed


game :: StateT WorldState IO ()
game = do
    ms <- lift . readline $ "> "
    maybe game dispatch $ parseInp (ms^.to fromJust.packed)


parseInp :: T.Text -> Maybe Input
parseInp = splitUp . T.words
  where
    splitUp []     = Nothing
    splitUp [x]    = Just (x, [""])
    splitUp (x:xs) = Just (x, xs)


dispatch :: Input -> StateT WorldState IO ()
dispatch (cn, rest) = maybe (wtf >> next) act $ findAction cn
  where
    wtf   = lift . T.putStrLn $ "?"
    next  = lift newLine >> game
    act a = a rest >> next


findAction :: CmdName -> Maybe Action
findAction cn = fmap (action . findCmdForFullName) $ findAbbrev cn' cns
  where
    cn' = T.toLower cn
    cns = map cmdName cmdList
    findCmdForFullName fn = head . filter ((== fn) . cmdName) $ cmdList


-- ==================================================
-- User commands:


dispCmdList :: IO ()
dispCmdList = T.putStrLn . T.init . T.unlines . reverse . T.lines . foldl makeTxtForCmd "" $ cmdList
  where
    makeTxtForCmd txt c = T.concat [cmdName c, [tab]^.packed, cmdDesc c, [nl]^.packed, txt]


help :: Action
help [""]   = lift . dumpFile $ helpDir ++ "root"
help [r]    = lift . dispHelpTopicByName $ r
help (r:rs) = help [r] >> lift newLine >> help rs
help _ = undefined


dispHelpTopicByName :: T.Text -> IO ()
dispHelpTopicByName r = do
    fns <- getDirectoryContents helpDir
    let tns = map T.pack (tail . tail . sort . delete "root" $ fns)
    maybe sorry dispHelp $ findAbbrev r tns
  where
    sorry = T.putStrLn "No help is available on that topic/command."
    dispHelp = dumpFile . (++) helpDir . T.unpack


dumpFile :: FilePath -> IO ()
dumpFile fn = T.putStr =<< T.readFile fn


data InvType = PlaInv | PlaEq | RmInv deriving Eq


what :: Action
what [""]   = lift . T.putStrLn $ "What abbreviation do you want to look up?"
what [r]    = (lift . whatCmd $ r) >> whatInv PlaInv r >> whatInv PlaEq r >> whatInv RmInv r
what (r:rs) = what [r] >> lift newLine >> what rs
what _ = undefined


whatCmd :: T.Text -> IO ()
whatCmd r = maybe notFound found $ findAbbrev (T.toLower r) (map cmdName cmdList)
  where
    notFound = T.putStrLn $ quote r <> " doesn't refer to any commands."
    found cn = T.putStrLn $ quote r <> " may refer to the " <> quote cn <> " command."


whatInv :: InvType -> T.Text -> StateT WorldState IO ()
whatInv it r = do
    is <- case it of PlaInv -> getPlaInv
                     PlaEq  -> getPlaEq
                     RmInv  -> getPlaRmInv
    ger <- getEntsInInvByName r is
    case ger of
      (Mult n (Just es)) | n == acp -> lift . T.putStrLn $ quote acp <> " may refer to everything " <> target
                         | otherwise ->
                           let e = head es
                               len = length es
                           in if len > 1
                             then lift . T.putStrLn $ quote r <> " may refer to the " <> showText len <> " " <> (makePlurFromBoth . getEntBothGramNos $ e) <> " " <> target
                             else do
                                ens <- getEntNamesInInv is
                                lift . T.putStrLn $ quote r <> " may refer to the " <> checkFirst e ens <> e^.sing <> " " <> target
      (Indexed x _ (Right e)) -> lift . T.putStrLn $ quote r <> " may refer to the " <> mkOrdinal x <> " " <> e^.sing <> " " <> target
      _ -> lift . T.putStrLn $ quote r <> " doesn't refer to anything " <> target
  where
    acp = [allChar]^.packed
    target = case it of PlaInv -> "in your inventory."
                        PlaEq  -> "in your readied equipment."
                        RmInv  -> "in this room."


checkFirst :: Ent -> [T.Text] -> T.Text
checkFirst e ens = if length matches > 1 then "first " else ""
  where
    matches = filter (== e^.name) ens


go :: T.Text -> Action
go dir [""] = goDispatcher [dir]
go dir rs   = goDispatcher (dir : rs)


goDispatcher :: Action
goDispatcher [r]    = tryMove r
goDispatcher (r:rs) = tryMove r >> lift newLine >> goDispatcher rs
goDispatcher _ = undefined


tryMove :: T.Text -> StateT WorldState IO ()
tryMove dir = let dir' = T.toLower dir
              in maybe sorry movePla $ M.lookup dir' dirMap
  where
    sorry     = lift . T.putStrLn . quote $ dir <> " is not a valid direction."
    movePla f = getPlaRmNextRmId f >>= maybe heDont moveHelper
      where
        heDont       = lift . T.putStrLn $ "You can't go that way."
        moveHelper i = pla.rmId .= i >> look [""]


dirMap :: M.Map T.Text (Rm -> Id)
dirMap = M.fromList [("n", north), ("s", south), ("e", east), ("w", west), ("u", up), ("d", down)]


look :: Action
look [""] = do
    r <- getPlaRm
    lift . T.putStrLn . T.concat $ [r^.name, [nl]^.packed, r^.desc]
    getPlaRmInv >>= dispRmInv
look [r]    = getPlaRmInv >>= getEntsInInvByName r >>= procGetEntResRm r >>= traverse_ (mapM_ descEnt)
look (r:rs) = look [r] >> look rs
look _ = undefined


dispRmInv :: Inv -> StateT WorldState IO ()
dispRmInv is = do
    ens <- getEntBothGramNosInInv is
    mapM_ descEntInRm (nub . zip (makeCountList ens) $ ens)
  where
    descEntInRm (x, (s, _)) | x == 1 = lift . T.putStrLn $ "There is " <> aOrAn s <> " here."
    descEntInRm (x, both) = lift . T.putStrLn $ "There are " <> showText x <> " " <> makePlurFromBoth both <> " here."


makeCountList :: (Eq a) => [a] -> [Int]
makeCountList xs = [ length (filter (==x) xs) | x <- xs ]


descEnt :: Ent -> StateT WorldState IO ()
descEnt e = do
    lift . T.putStrLn $ e^.desc
    t <- getEntType e
    when (t == ConType) $ descEntsInInvForId ei
    when (t == MobType) $ descEq ei
  where
    ei = e^.entId


descEntsInInvForId :: Id -> StateT WorldState IO ()
descEntsInInvForId i = do
    ens <- getInv i >>= getEntBothGramNosInInv
    if null ens then none else header >> mapM_ descEntInInv (nub . zip (makeCountList ens) $ ens)
  where
    none
      | i == 0 = lift . T.putStrLn $ "You aren't carrying anything."
      | otherwise = getEnt i >>= \e -> lift . T.putStrLn $ "The " <> e^.sing <> " is empty."
    header
      | i == 0 = lift . T.putStrLn $ "You are carrying:"
      | otherwise = getEnt i >>= \e -> lift . T.putStrLn $ "The " <> e^.sing <> " contains:"
    descEntInInv (x, (s, _)) | x == 1 = lift . T.putStrLn $ "1 " <> s
    descEntInInv (x, both) = lift . T.putStrLn $ showText x <> " " <> makePlurFromBoth both


inv :: Action
inv [""]   = descEntsInInvForId 0
inv [r]    = getPlaInv >>= getEntsInInvByName r >>= procGetEntResPlaInv r >>= traverse_ (mapM_ descEnt)
inv (r:rs) = inv [r] >> inv rs
inv _ = undefined


equip :: Action
equip [""]   = descEq 0
equip [r]    = getPlaEq >>= getEntsInInvByName r >>= procGetEntResPlaInv r >>= traverse_ (mapM_ descEnt)
equip (r:rs) = equip [r] >> equip rs
equip _ = undefined


descEq :: Id -> StateT WorldState IO ()
descEq i = do
    edl <- getEqMap i >>= mkEqDescList . mkSlotNameToIdList . M.toList
    if null edl then none else header >> forM_ edl (lift . T.putStrLn)
  where
    mkSlotNameToIdList = map (first getSlotName)
    getSlotName s = fromJust . M.lookup s $ slotNamesMap
    mkEqDescList = mapM descEqHelper
    none
      | i == 0    = lift . T.putStrLn $ "You don't have anything readied. You're naked!"
      | otherwise = getEnt i >>= \e -> lift . T.putStrLn $ "The " <> e^.sing <> " doesn't have anything readied."
    header
      | i == 0    = lift . T.putStrLn $ "You have readied the following equipment:"
      | otherwise = getEnt i >>= \e -> lift . T.putStrLn $ "The " <> e^.sing <> " has readied the following equipment:"


descEqHelper :: (SlotName, Id) -> StateT WorldState IO T.Text
descEqHelper (sn, i) = do
    e <- getEnt i
    return ("[" <> sn <> "] " <> [tab]^.packed <> e^.sing)


getAction :: Action
getAction [""] = lift . T.putStrLn $ "What do you want to get?"
getAction [r]  = getPlaRmInv >>= getEntsInInvByName r >>= procGetEntResRm r >>= traverse_ shuffleInv
  where
    shuffleInv es = do
        i <- getPlaRmId
        moveInv (getEntIds es) i 0 >> lift ok
getAction (r:rs) = getAction [r] >> getAction rs
getAction _ = undefined


dropAction :: Action
dropAction [""] = lift . T.putStrLn $ "What do you want to drop?"
dropAction [r]  = getPlaInv >>= getEntsInInvByName r >>= procGetEntResPlaInv r >>= traverse_ shuffleInv
  where
    shuffleInv es = do
        i <- getPlaRmId
        moveInv (getEntIds es) 0 i >> lift ok
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
                                  then lift . T.putStrLn $ "The " <> e^.sing <> " isn't a container."
                                  else dispatchToHelper (e^.entId)
  where
    findCon cn
      | T.head cn == rmChar = getPlaRmInv >>= getEntsInInvByName (T.tail cn) >>= procGetEntResRm (T.tail cn)
      | otherwise = getPlaInv >>= getEntsInInvByName cn >>= procGetEntResPlaInv cn
    onlyOneMsg         = case por of Put -> "You can only put things into one container at a time."
                                     Rem -> "You can only remove things from one container at a time."
    dispatchToHelper i = case por of Put -> putHelper i restWithoutCon 
                                     Rem -> remHelper i restWithoutCon
      where
        restWithoutCon = r : (init rs)
putRemDispatcher _ _ = undefined


putHelper :: Id -> Rest -> StateT WorldState IO ()
putHelper _ []      = return ()
putHelper ci (r:rs) = do
    mes <- getPlaInv >>= getEntsInInvByName r >>= procGetEntResPlaInv r
    case mes of Nothing -> next
                Just es -> do
                    let is = getEntIds es
                    if ci `elem` is
                      then do
                          e <- getEnt ci
                          lift . T.putStrLn $ "You can't put the " <> e^.sing <> " inside itself."
                          let is' = filter (/= ci) is
                          if null is'
                            then next
                            else moveInv is' 0 ci >> lift ok >> next
                      else moveInv is 0 ci >> lift ok >> next
  where
    next = putHelper ci rs


remHelper :: Id -> Rest -> StateT WorldState IO ()
remHelper _ []      = return ()
remHelper ci (r:rs) = do
    e <- getEnt ci
    fis <- getInv ci
    if null fis
      then lift . T.putStrLn $ "The " <> e^.sing <> " appears to be empty."
      else getEntsInInvByName r fis >>= procGetEntResCon (e^.sing) r >>= maybe next shuffleInv
  where
    next = remHelper ci rs
    shuffleInv es = moveInv (getEntIds es) ci 0 >> lift ok >> next


ready :: Action
ready [""] = lift . T.putStrLn $ "What do you want to ready?"
ready [r]  = getEntToReady r >>= readyDispatcher
ready (r:rs) = ready [r] >> ready rs
ready _ = undefined


readyDispatcher :: (Maybe Ent, Maybe Slot) -> StateT WorldState IO () -- TODO: Refactor?
readyDispatcher (Nothing, _) = return ()
readyDispatcher (Just e, ms) = do
    let i = e^.entId
    em <- getPlaEqMap
    t <- getEntType e
    case t of WpnType -> readyWpn i e em ms
              _       -> lift . T.putStrLn $ "You can't ready a " <> e^.sing <> "."


readyWpn :: Id -> Ent -> EqMap -> Maybe Slot -> StateT WorldState IO () -- TODO: Refactor?
readyWpn i e em ms
  | isJust (em^.at BothHandsS) = void . lift . T.putStrLn $ "You're already wielding a two-handed weapon."
  | otherwise = do
      ms' <- case ms of Just s  -> getDesigHandSlot em s
                        Nothing -> getAvailHandSlot em
      case ms' of Nothing -> return ()
                  Just s  -> do
                      w <- getWpn i
                      let wt = w^.wpnSub
                      case wt of OneHanded -> do
                                     let em' = em & at s ?~ i
                                     eqTbl.at 0 ?= em'
                                     remFromInv [i] 0
                                     lift . T.putStrLn $ "You wield the " <> e^.sing <> " with your " <> showHandSlot s <> "."
                                 TwoHanded -> do
                                     if areBothHandsAvail
                                      then do
                                          let em' = em & at BothHandsS ?~ i
                                          eqTbl.at 0 ?= em'
                                          remFromInv [i] 0
                                          lift . T.putStrLn $ "You wield the " <> e^.sing <> " with both hands."
                                      else lift . T.putStrLn $ "Both hands are required to weild the " <> e^.sing <> "."
  where
    areBothHandsAvail = isSlotAvail RHandS && isSlotAvail LHandS
    isSlotAvail s = isNothing $ em^.at s


showHandSlot :: Slot -> T.Text
showHandSlot s = case s of RHandS -> "right hand"
                           LHandS -> "left hand"
                           _ -> undefined


getDesigHandSlot :: EqMap -> Slot -> StateT WorldState IO (Maybe Slot) -- TODO: Refactor?
getDesigHandSlot em s = case em^.at s of Nothing -> return (Just s)
                                         Just i  -> getEnt i >>= sorry
  where
    sorry e = lift $ T.putStrLn ("You're already wielding a " <> e^.sing <> " with your " <> showHandSlot s <> ".") >> return Nothing


getAvailHandSlot :: EqMap -> StateT WorldState IO (Maybe Slot) -- TODO: Refactor?
getAvailHandSlot em = do
    h <- getPlaMobHand
    let s = getSlotForHand h
    case em^.at s of Nothing -> return (Just s)
                     Just _  -> let s' = getSlotForOtherHand h
                                in case em^.at s' of Nothing -> return (Just s')
                                                     Just _  -> sorry
  where
    sorry = lift $ T.putStrLn "Your hands are already full." >> return Nothing


getSlotForHand :: Hand -> Slot
getSlotForHand h = case h of RHand -> RHandS
                             LHand -> LHandS
                             _ -> undefined


getSlotForOtherHand :: Hand -> Slot
getSlotForOtherHand h = case h of RHand -> LHandS
                                  LHand -> RHandS
                                  _ -> undefined


unready :: Action
unready [""] = lift . T.putStrLn $ "What do you want to unready?"
unready [r]  = getPlaEq >>= getEntsInInvByName r >>= procGetEntResPlaInv r >>= traverse_ shuffleInv
  where
    shuffleInv es = do
        let is = getEntIds es
        em <- getPlaEqMap
        eqTbl.at 0 ?= M.filter (`notElem` is) em
        addToInv is 0 >> lift ok
unready (r:rs) = unready [r] >> unready rs
unready _ = undefined


okapi :: Action
okapi _ = do
    i <- mkOkapi
    lift . T.putStrLn $ "Made okapi with id " <> showText i <> "."


buffCheck :: Action
buffCheck _ = lift buffCheckHelper
  where
    buffCheckHelper = do
        td <- getTemporaryDirectory
        (fn, h) <- openTempFile td "temp"
        bm <- hGetBuffering h
        T.putStrLn $ "(Default) buffering mode for temp file " <> fn^.packed.to quote <> " is " <> (quote . showText $ bm) <> "."
        hClose h
        removeFile fn


dumpEnv :: Action
dumpEnv [""] = lift $ getEnvironment >>= dumpAssocList
dumpEnv [r]  = lift $ getEnvironment >>= dumpAssocList . filter grepBoth
  where
    grepBoth = \(k, v) -> r `T.isInfixOf` (k^.packed) || r `T.isInfixOf` (v^.packed)
dumpEnv (r:rs) = dumpEnv [r] >> dumpEnv rs
dumpEnv _ = undefined


uptime :: Action
uptime _ = do
    lift (readProcess "/usr/bin/uptime" [] "") >>= lift . T.putStrLn . parse
  where
    parse ut = let (a, b) = span (/= ',') ut
                   a' = unwords . tail . words $ a
                   b' = takeWhile (/= ',') . tail $ b
                   c  = (toUpper . head $ a') : (tail a')
               in c^.packed <> b'^.packed <> "."
