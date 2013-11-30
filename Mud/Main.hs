{-# OPTIONS_GHC -Wall -Werror -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Main (main) where

import Mud.Convenience
import Mud.DataTypes
import Mud.StateHelpers
import Mud.TheWorld

import Control.Arrow (first)
import Control.Lens (at, ix, to)
import Control.Lens.Operators ((&), (^.), (?~), (.=), (?=))
import Control.Monad (forM_, when, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Char (isSpace, toUpper)
import Data.Foldable (traverse_)
import Data.Functor
import Data.List (delete, find, nub, sort)
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
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)


-- TODO: Split into more modules. Trim down Main until it contains just the "main" function and the bare minimum. Make modules export only what they need to export.


-- TODO: Consider what, if anything, should be done about indexing in commands like this:
-- r 1.sw 3.sw
-- ...when you have 3 swords.


-- ==================================================
-- Top level definitions:


ver :: T.Text
ver = "0.0 since 2013-10"


mudDir :: FilePath
mudDir = let home = unsafePerformIO . getEnv $ "HOME"
         in home ++ "/hmud/Mud/"^.unpacked


helpDir :: FilePath
helpDir = mudDir ++ "help/"


-- ==================================================
-- Structures relating to user commands:


type Input   = (CmdName, Rest)
type CmdName = T.Text
type Rest    = [T.Text]

type Action = Rest -> MudStack ()

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
main = setCurrentDirectory mudDir >> welcomeMsg >> 
       execStateT initialize initWS >>= evalStateT game


welcomeMsg :: IO ()
welcomeMsg = do
    un <- getEnv "USER"
    mn <- whatsMyName
    T.putStrLn $ "\nHello, " <> un^.packed <> ". Welcome to " <> dblQuote mn <> " ver " <> ver <> ".\n"
  where
    whatsMyName = getProgName >>= \mn ->
        return $ if mn == "<interactive>" then "why u no compile me?" else mn^.packed


game :: MudStack ()
game = lift (readline "> ") >>= \ms ->
    maybe game dispatch $ splitInp (ms^.to fromJust.packed)


splitInp :: T.Text -> Maybe Input
splitInp = splitUp . T.words
  where
    splitUp []     = Nothing
    splitUp [x]    = Just (x, [""])
    splitUp (x:xs) = Just (x, xs)


dispatch :: Input -> MudStack ()
dispatch (cn, rest) = maybe (wtf >> next) act $ findAction cn
  where
    wtf   = output "?"
    next  = lift newLine >> game
    act a = a rest >> next


findAction :: CmdName -> Maybe Action
findAction cn = action . findCmdForFullName <$> findAbbrev (T.toLower cn) cns
  where
    findCmdForFullName fn = head . filter ((== fn) . cmdName) $ cmdList
    cns = map cmdName cmdList


-- ==================================================
-- User commands:


dispCmdList :: IO ()
dispCmdList = T.putStrLn . T.init . T.unlines . reverse . T.lines . foldl makeTxtForCmd "" $ cmdList
  where
    makeTxtForCmd txt c = T.concat [ cmdName c, "\t", cmdDesc c, "\n", txt ]


help :: Action
help [""]   = lift . dumpFile $ helpDir ++ "root"
help [r]    = lift . dispHelpTopicByName $ r
help (r:rs) = help [r] >> lift newLine >> help rs
help _      = undefined


dispHelpTopicByName :: T.Text -> IO ()
dispHelpTopicByName r = getDirectoryContents helpDir >>= \fns ->
    let fns' = tail . tail . sort . delete "root" $ fns
        tns  = map T.pack fns'
    in maybe sorry dispHelp $ findAbbrev r tns
  where
    sorry = T.putStrLn "No help is available on that topic/command."
    dispHelp = dumpFile . (helpDir ++) . T.unpack


data InvType = PlaInv | PlaEq | RmInv deriving Eq


what :: Action
what [""]   = output "What abbreviation do you want to look up?"
what [r]    = whatCmd >> whatInv PlaInv r >> whatInv PlaEq r >> whatInv RmInv r
  where
    whatCmd = maybe notFound found $ findAbbrev (T.toLower r) (map cmdName cmdList)
      where
        notFound = output $ dblQuote r <> " doesn't refer to any commands."
        found cn = outputCon [ dblQuote r, " may refer to the ", dblQuote cn, " command." ]
what (r:rs) = what [r] >> lift newLine >> what rs
what      _ = undefined


-- TODO: Continue refactoring from here.
whatInv :: InvType -> T.Text -> MudStack ()
whatInv it r = do
    is <- case it of PlaInv -> getPlaInv
                     PlaEq  -> getPlaEq
                     RmInv  -> getPlaRmInv
    ger <- getEntsInInvByName r is
    case ger of
      (Mult n (Just es)) | n == acp  -> output $ dblQuote acp <> " may refer to everything" <> loc
                         | otherwise ->
                           let e = head es
                               len = length es
                           in if len > 1
                             then let ebgns = take len [ getEntBothGramNos e' | e' <- es ]
                                      h = head ebgns
                                      target = if all (== h) ebgns then makePlurFromBoth h else e^.name.to bracketQuote
                                  in outputCon [ dblQuote r, " may refer to the ", showText len, " ", target, loc ]
                             else getEntNamesInInv is >>= \ens ->
                                 outputCon [ dblQuote r, " may refer to the ", checkFirst e ens, e^.sing, loc ]
      (Indexed x _ (Right e)) -> outputCon [ dblQuote r, " may refer to the ", mkOrdinal x, " ", e^.name.to bracketQuote, " ", e^.sing.to parensQuote, loc ]
      _                       -> output $ dblQuote r <> " doesn't refer to anything" <> loc
  where
    acp = [allChar]^.packed
    loc = case it of PlaInv -> " in your inventory."
                     PlaEq  -> " in your readied equipment."
                     RmInv  -> " in this room."


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
goDispatcher _      = undefined


tryMove :: T.Text -> MudStack ()
tryMove dir = let dir' = T.toLower dir
              in maybe sorry movePla $ M.lookup dir' dirMap
  where
    sorry     = output . dblQuote $ dir <> " is not a valid direction."
    movePla f = getPlaRmNextRmId f >>= maybe heDont moveHelper
      where
        heDont       = output "You can't go that way."
        moveHelper i = pla.rmId .= i >> look [""]


dirMap :: M.Map T.Text (Rm -> Id)
dirMap = M.fromList [("n", north), ("s", south), ("e", east), ("w", west), ("u", up), ("d", down)]


look :: Action
look [""]   = do
    r <- getPlaRm
    output $ r^.name <> "\n" <> r^.desc
    getPlaRmInv >>= dispRmInv
look [r]    = getPlaRmInv >>= getEntsInInvByName r >>= procGetEntResRm r >>= traverse_ (mapM_ descEnt)
look (r:rs) = look [r] >> look rs
look _      = undefined


dispRmInv :: Inv -> MudStack ()
dispRmInv is = mkNameCountBothList is >>= mapM_ descEntInRm
  where
    descEntInRm (en, c, (s, _))
      | c == 1 = outputCon [ "There is ", aOrAn s, " here. ", bracketQuote en ]
    descEntInRm (en, c, both) = outputCon [ "There are ", showText c, " ", makePlurFromBoth both, " here. ", bracketQuote en ]


mkNameCountBothList :: Inv -> MudStack [(T.Text, Int, BothGramNos)]
mkNameCountBothList is = do
    ens <- getEntNamesInInv is
    ebgns <- getEntBothGramNosInInv is
    let cs = makeCountList ebgns
    return (nub . zip3 ens cs $ ebgns)


makeCountList :: (Eq a) => [a] -> [Int]
makeCountList xs = [ length (filter (==x) xs) | x <- xs ]


descEnt :: Ent -> MudStack ()
descEnt e = do
    e^.desc.to output
    t <- getEntType e
    when (t == ConType) $ descEntsInInvForId i
    when (t == MobType) $ descEq i
  where
    i = e^.entId


descEntsInInvForId :: Id -> MudStack ()
descEntsInInvForId i = getInv i >>= \is ->
    if null is then none else header >> mkNameCountBothList is >>= mapM_ descEntInInv
  where
    none
      | i == 0 = output "You aren't carrying anything."
      | otherwise = getEnt i >>= \e -> output $ "The " <> e^.sing <> " is empty."
    header
      | i == 0 = output "You are carrying:"
      | otherwise = getEnt i >>= \e -> output $ "The " <> e^.sing <> " contains:"
    descEntInInv (en, c, (s, _))
      | c == 1 = output $ nameCol en <> "1 " <> s
    descEntInInv (en, c, both) = outputCon [ nameCol en, showText c, " ", makePlurFromBoth both ]
    nameCol = bracketPad 11


inv :: Action
inv [""]   = descEntsInInvForId 0
inv [r]    = getPlaInv >>= getEntsInInvByName r >>= procGetEntResPlaInv r >>= traverse_ (mapM_ descEnt)
inv (r:rs) = inv [r] >> inv rs
inv _      = undefined


equip :: Action
equip [""]   = descEq 0
equip [r]    = getPlaEq >>= getEntsInInvByName r >>= procGetEntResPlaInv r >>= traverse_ (mapM_ descEnt)
equip (r:rs) = equip [r] >> equip rs
equip _      = undefined


descEq :: Id -> MudStack ()
descEq i = getEqMap i >>= mkEqDescList . mkSlotNameToIdList . M.toList >>= \edl ->
    if null edl then none else header >> forM_ edl output
  where
    mkSlotNameToIdList = map (first getSlotName)
    getSlotName s = fromJust . M.lookup s $ slotNamesMap
    mkEqDescList = mapM descEqHelper
    descEqHelper (sn, i') = getEnt i' >>= \e ->
        return (T.concat [ parensPad 15 sn, e^.sing, " ", e^.name.to bracketQuote ])
    none
      | i == 0    = output "You don't have anything readied. You're naked!"
      | otherwise = getEnt i >>= \e -> output $ "The " <> e^.sing <> " doesn't have anything readied."
    header
      | i == 0    = output "You have readied the following equipment:"
      | otherwise = getEnt i >>= \e -> output $ "The " <> e^.sing <> " has readied the following equipment:"


getAction :: Action
getAction [""]   = output "What do you want to get?"
getAction [r]    = getPlaRmInv >>= getEntsInInvByName r >>= procGetEntResRm r >>= traverse_ shuffleInv
  where
    shuffleInv es = let is = getEntIds es
                    in getPlaRmId >>= \i ->        
                       moveInv is i 0 >> descGetDrop Get is
getAction (r:rs) = getAction [r] >> getAction rs
getAction _      = undefined


dropAction :: Action
dropAction [""]   = output "What do you want to drop?"
dropAction [r]    = getPlaInv >>= getEntsInInvByName r >>= procGetEntResPlaInv r >>= traverse_ shuffleInv
  where
    shuffleInv es = let is = getEntIds es
                    in getPlaRmId >>= \i ->
                       moveInv is 0 i >> descGetDrop Drop is
dropAction (r:rs) = dropAction [r] >> dropAction rs
dropAction _      = undefined


data GetOrDrop = Get | Drop


descGetDrop :: GetOrDrop -> Inv -> MudStack ()
descGetDrop god is = mkNameCountBothList is >>= mapM_ descGetDropHelper
  where
    descGetDropHelper (_, c, (s, _))
      | c == 1 = outputCon [ "You", verb, aOrAn s, "." ]
    descGetDropHelper (_, c, both) = outputCon [ "You", verb, showText c, " ", makePlurFromBoth both, "." ]
    verb = case god of Get  -> " pick up "
                       Drop -> " drop "


data PutOrRem = Put | Rem


putAction :: Action
putAction [""] = output "What do you want to put? And where do you want to put it?"
putAction [_]  = output "Where do you want to put it?"
putAction rs   = putRemDispatcher Put rs


remove :: Action
remove [""] = output "What do you want to remove? And what do you want to remove it from?"
remove [_]  = output "What do you want to remove it from?"
remove rs   = putRemDispatcher Rem rs


rmChar :: Char
rmChar = '-'


putRemDispatcher :: PutOrRem -> Action
putRemDispatcher por (r:rs) = findCon (last rs) >>= \mes ->
    case mes of Nothing -> return ()
                Just es -> if length es /= 1
                             then output onlyOneMsg
                             else let e = head es
                                  in getEntType e >>= \t ->
                                     if t /= ConType
                                       then output $ "The " <> e^.sing <> " isn't a container."
                                       else e^.entId.to dispatchToHelper
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


putHelper :: Id -> Rest -> MudStack ()
putHelper _ []      = return ()
putHelper ci (r:rs) = getPlaInv >>= getEntsInInvByName r >>= procGetEntResPlaInv r >>= \mes ->
    case mes of Nothing -> next
                Just es -> let is = getEntIds es
                           in getEnt ci >>= \ce ->
                              if ci `elem` is
                                then do
                                    output $ "You can't put the " <> ce^.sing <> " inside itself."
                                    let is' = filter (/= ci) is
                                    if null is'
                                      then next
                                      else moveInv is' 0 ci >> descPutRem Put is' ce >> next
                                else moveInv is 0 ci >> descPutRem Put is ce >> next
  where
    next = putHelper ci rs


remHelper :: Id -> Rest -> MudStack ()
remHelper _ []      = return ()
remHelper ci (r:rs) = do
    ce <- getEnt ci
    fis <- getInv ci
    if null fis
      then output $ "The " <> ce^.sing <> " appears to be empty."
      else getEntsInInvByName r fis >>= procGetEntResCon (ce^.sing) r >>= maybe next (shuffleInv ce)
  where
    next = remHelper ci rs
    shuffleInv ce es = let is = getEntIds es
                       in moveInv is ci 0 >> descPutRem Rem is ce >> next


descPutRem :: PutOrRem -> Inv -> Ent -> MudStack ()
descPutRem por is ce = mkNameCountBothList is >>= mapM_ descPutRemHelper
  where
    descPutRemHelper (_, c, (s, _))
      | c == 1 = outputCon [ "You", verb, aOrAn s, prep, ce^.sing, "." ]
    descPutRemHelper (_, c, both) = outputCon [ "You", verb, showText c, " ", makePlurFromBoth both, prep, ce^.sing, "." ]
    verb = case por of Put -> " put "
                       Rem -> " remove "
    prep = case por of Put -> " in the "
                       Rem -> " from the "


ready :: Action
ready [""]   = output "What do you want to ready?"
ready [r]    = getEntToReadyByName r >>= readyDispatcher
ready (r:rs) = ready [r] >> ready rs
ready _      = undefined


readyDispatcher :: (Maybe Ent, Maybe RightOrLeft) -> MudStack () -- TODO: Refactor?
readyDispatcher (Nothing, _)   = return ()
readyDispatcher (Just e, mrol) = do
    let i = e^.entId
    em <- getPlaEqMap
    t <- getEntType e
    case t of ClothType -> readyCloth i e em mrol
              WpnType   -> readyWpn i e em mrol
              ArmType   -> undefined
              _         -> output $ "You can't ready a " <> e^.sing <> "."


isRingROL :: RightOrLeft -> Bool
isRingROL rol = case rol of R -> False
                            L -> False
                            _ -> True


isSlotAvail :: EqMap -> Slot -> Bool
isSlotAvail em s = isNothing $ em^.at s


findAvailSlot :: EqMap -> [Slot] -> Maybe Slot
findAvailSlot em = find (isSlotAvail em)


readyWpn :: Id -> Ent -> EqMap -> Maybe RightOrLeft -> MudStack () -- TODO: Refactor?
readyWpn i e em mrol
  | isJust (em^.at BothHandsS) = void . output $ "You're already wielding a two-handed weapon."
  | otherwise = do
      ms <- case mrol of Just rol -> getDesigWpnSlot e em rol
                         Nothing  -> getAvailWpnSlot em
      case ms of Nothing -> return ()
                 Just s  -> do
                     w <- getWpn i
                     let wt = w^.wpnSub
                     case wt of OneHanded -> do
                                    let em' = em & at s ?~ i
                                    eqTbl.at 0 ?= em'
                                    remFromInv [i] 0
                                    outputCon [ "You wield the ", e^.sing, " with your ", slotNamesMap^.ix s, "." ]
                                TwoHanded -> do
                                    if all (isSlotAvail em) [RHandS, LHandS]
                                      then do
                                          let em' = em & at BothHandsS ?~ i
                                          eqTbl.at 0 ?= em'
                                          remFromInv [i] 0
                                          output $ "You wield the " <> e^.sing <> " with both hands."
                                      else output $ "Both hands are required to weild the " <> e^.sing <> "."


getDesigWpnSlot :: Ent -> EqMap -> RightOrLeft -> MudStack (Maybe Slot) -- TODO: Refactor?
getDesigWpnSlot e em rol
  | isRingROL rol = sorryNotRing
  | otherwise = let s = case rol of R -> RHandS
                                    L -> LHandS
                                    _ -> undefined
                in case em^.at s of Nothing -> return (Just s)
                                    Just i  -> getEnt i >>= sorry s
  where
    sorry s e'   = outputCon [ "You're already wielding a ", e'^.sing, " with your ", slotNamesMap^.ix s, "." ] >> return Nothing
    sorryNotRing = output ("You can't wield a " <> e^.sing <> " with your finger!") >> return Nothing


getAvailWpnSlot :: EqMap -> MudStack (Maybe Slot) -- TODO: Refactor?
getAvailWpnSlot em = do
    h <- getPlaMobHand
    let s = getSlotForHand h
    case em^.at s of Nothing -> return (Just s)
                     Just _  -> let s' = getSlotForOtherHand h
                                in case em^.at s' of Nothing -> return (Just s')
                                                     Just _  -> sorry
  where
    getSlotForHand h      = case h of RHand -> RHandS
                                      LHand -> LHandS
                                      _     -> undefined
    getSlotForOtherHand h = case h of RHand -> LHandS
                                      LHand -> RHandS
                                      _     -> undefined
    sorry = output "You're already wielding two weapons." >> return Nothing


readyCloth :: Id -> Ent -> EqMap -> Maybe RightOrLeft -> MudStack () -- TODO: Refactor?
readyCloth i e em mrol = do
    ms <- case mrol of Just rol -> getDesigClothSlot i e em rol
                       Nothing  -> undefined
    case ms of Nothing -> return ()
               Just s  -> do
                   let em' = em & at s ?~ i
                   eqTbl.at 0 ?= em'
                   remFromInv [i] 0
                   readiedMsg s
  where
    readiedMsg s = do
        c <- getCloth i
        case c of FingerC -> outputCon [ "You wear the ", e^.sing, " on your ", slotNamesMap^.ix s, "." ]
                  WristC  -> outputCon [ "You wear the ", e^.sing, " on your ", slotNamesMap^.ix s, "." ]
                  _       -> undefined


getDesigClothSlot :: Id -> Ent -> EqMap -> RightOrLeft -> MudStack (Maybe Slot) -- TODO: Refactor?
getDesigClothSlot i e em rol = getCloth i >>= \c ->
    case c of FingerC -> if not . isRingROL $ rol
                           then sorryIsRing
                           else let rs = case rol of RIF -> RIndexFS
                                                     RMF -> RMidFS
                                                     RRF -> RRingFS
                                                     RPF -> RPinkyFS
                                                     LIF -> LIndexFS
                                                     LMF -> LMidFS
                                                     LRF -> LRingFS
                                                     LPF -> LPinkyFS
                                                     _   -> undefined
                                in case em^.at rs of Nothing -> return (Just rs)
                                                     Just i' -> getEnt i' >>= sorry rs
              WristC  -> if isRingROL rol
                           then sorryNotRing
                           else let mws = case rol of R -> findAvailSlot em [RWrist1S, RWrist2S, RWrist3S]
                                                      L -> findAvailSlot em [LWrist1S, LWrist2S, LWrist3S]
                                                      _ -> undefined
                                in if isNothing mws then sorryWrist else return mws
              _       -> undefined
  where
    sorry s e'   = outputCon [ "You're already wearing a ", e'^.sing, " on your ", slotNamesMap^.ix s, "." ] >> return Nothing
    sorryIsRing  = output ringHelp >> return Nothing
    sorryNotRing = output ("You can't wear a " <> e^.sing <> " on your finger!") >> return Nothing
    sorryWrist   = output "You can't wear any more accessories on your wrist."   >> return Nothing


unready :: Action
unready [""]   = output "What do you want to unready?"
unready [r]    = getPlaEq >>= getEntsInInvByName r >>= procGetEntResPlaInv r >>= traverse_ shuffleInv
  where
    shuffleInv es = do
        let is = getEntIds es
        em <- getPlaEqMap
        eqTbl.at 0 ?= M.filter (`notElem` is) em
        addToInv is 0 >> descUnready is
unready (r:rs) = unready [r] >> unready rs
unready _      = undefined


descUnready :: Inv -> MudStack ()
descUnready is = mkNameCountBothList is >>= mapM_ descUnreadyHelper
  where
    descUnreadyHelper (_, c, (s, _))
      | c == 1 = output $ "You unready the " <> s <> "." -- TODO: Use a word that's better than "unready" (case-by-case)?
    descUnreadyHelper (_, c, both) = outputCon [ "You unready ", showText c, " ", makePlurFromBoth both, "." ]


okapi :: Action
okapi _ = mkOkapi >>= \i ->
    output $ "Made okapi with id " <> showText i <> "."


buffCheck :: Action
buffCheck _ = lift buffCheckHelper
  where
    buffCheckHelper = do
        td <- getTemporaryDirectory
        (fn, h) <- openTempFile td "temp"
        bm <- hGetBuffering h
        T.putStrLn . T.concat $ [ "(Default) buffering mode for temp file ", fn^.packed.to dblQuote, " is ", dblQuote . showText $ bm, "." ]
        hClose h
        removeFile fn


dumpEnv :: Action
dumpEnv [""] = lift $ getEnvironment >>= dumpAssocList
dumpEnv [r]  = lift $ getEnvironment >>= dumpAssocList . filter grepBoth
  where
    grepBoth = \(k, v) -> r `T.isInfixOf` (k^.packed) || r `T.isInfixOf` (v^.packed)
dumpEnv (r:rs) = dumpEnv [r] >> dumpEnv rs
dumpEnv _    = undefined


uptime :: Action
uptime _ = lift (readProcess "/usr/bin/uptime" [] "") >>= output . parse
  where
    parse ut = let (a, b) = span (/= ',') ut
                   a' = unwords . tail . words $ a
                   b' = dropWhile isSpace . takeWhile (/= ',') . tail $ b
                   c  = (toUpper . head $ a') : (tail a')
               in T.concat [ c^.packed, " ", b'^.packed, "." ]
