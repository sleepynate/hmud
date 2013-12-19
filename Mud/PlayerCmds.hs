{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.PlayerCmds (game) where

import Mud.Convenience
import Mud.MiscDataTypes
import Mud.StateDataTypes
import Mud.StateHelpers
import Mud.TheWorld
import Mud.TopLvlDefs

import Control.Arrow (first)
import Control.Lens (_1, _2, at, dropping, folded, to)
import Control.Lens.Operators ((&), (^.), (^..), (?~), (.=), (?=))
import Control.Monad ((>=>), forM_, mplus, when)
import Control.Monad.Trans.Class (lift)
import Data.Char (isSpace, toUpper)
import Data.Foldable (traverse_)
import Data.Functor
import Data.List (delete, find, foldl', nub, nubBy, sort)
import Data.Maybe (fromJust, isNothing)
import Data.Text.Strict.Lens (packed, unpacked)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.Readline (readline)
import System.Directory (getDirectoryContents, getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment)
import System.Exit (exitSuccess)
import System.IO
import System.Process (readProcess)


-- TODO: Make a command to re-enter the last command. Command history?


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


-----


dispCmdList :: IO ()
dispCmdList = T.putStrLn . T.init . T.unlines . reverse . T.lines . foldl' makeTxtForCmd "" $ cmdList
  where
    makeTxtForCmd txt c = T.concat [ cmdName c, "\t", cmdDesc c, "\n", txt ]


-----


help :: Action
help [""]   = lift . dumpFile $ helpDir ++ "root"
help [r]    = lift . dispHelpTopicByName $ r
help (r:rs) = help [r] >> lift newLine >> help rs
help _      = undefined


dispHelpTopicByName :: T.Text -> IO ()
dispHelpTopicByName r = getDirectoryContents helpDir >>= \fns ->
    let fns' = tail . tail . sort . delete "root" $ fns
        tns  = fns'^..folded.packed
    in maybe sorry dispHelp $ findAbbrev r tns
  where
    sorry = T.putStrLn "No help is available on that topic/command."
    dispHelp = dumpFile . (helpDir ++) . T.unpack


-----


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


whatInv :: InvType -> T.Text -> MudStack ()
whatInv it r = do
    is <- getLocInv
    ger <- getEntsInInvByName r is
    case ger of
      (Mult _ n (Just es)) | n == acp  -> output $ dblQuote acp <> " may refer to everything" <> locName
                           | otherwise ->
                             let e   = head es
                                 len = length es
                             in if len > 1
                               then let ebgns  = take len [ getEntBothGramNos e' | e' <- es ]
                                        h      = head ebgns
                                        target = if all (== h) ebgns then makePlurFromBoth h else e^.name.to bracketQuote <> "s"
                                    in outputCon [ dblQuote r, " may refer to the ", showText len, " ", target, locName ]
                               else getEntNamesInInv is >>= \ens ->
                                   outputCon [ dblQuote r, " may refer to the ", checkFirst e ens, e^.sing, locName ]
      (Indexed x _ (Right e)) -> outputCon [ dblQuote r, " may refer to the ", mkOrdinal x, " ", e^.name.to bracketQuote, " ", e^.sing.to parensQuote, locName ]
      _                       -> output $ dblQuote r <> " doesn't refer to anything" <> locName
  where
    getLocInv = case it of PlaInv -> getPlaInv
                           PlaEq  -> getPlaEq
                           RmInv  -> getPlaRmInv
    acp       = [allChar]^.packed
    locName   = case it of PlaInv -> " in your inventory."
                           PlaEq  -> " in your readied equipment."
                           RmInv  -> " in this room."
    checkFirst e ens = let matches = filter (== e^.name) ens
                       in if length matches > 1 then "first " else ""


-----


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


-----


look :: Action
look [""]   = getPlaRm >>= \r ->
    output (r^.name <> "\n" <> r^.desc) >> getPlaRmInv >>= dispRmInv
look [r]    = getPlaRmInv >>= getEntsInInvByName r >>= procGetEntResRm >>= traverse_ (mapM_ descEnt) -- TODO: Put info about "traverse_" in the style guide?
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
      | i == 0 = dudeYourHandsAreEmpty
      | otherwise = getEnt i >>= \e -> output $ "The " <> e^.sing <> " is empty."
    header
      | i == 0 = output "You are carrying:"
      | otherwise = getEnt i >>= \e -> output $ "The " <> e^.sing <> " contains:"
    descEntInInv (en, c, (s, _))
      | c == 1 = output $ nameCol en <> "1 " <> s
    descEntInInv (en, c, both) = outputCon [ nameCol en, showText c, " ", makePlurFromBoth both ]
    nameCol = bracketPad 11


dudeYourHandsAreEmpty :: MudStack ()
dudeYourHandsAreEmpty = output "You aren't carrying anything."


-----


inv :: Action
inv [""]   = descEntsInInvForId 0
inv [r]    = getPlaInv >>= getEntsInInvByName r >>= procGetEntResPlaInv >>= traverse_ (mapM_ descEnt)
inv (r:rs) = inv [r] >> inv rs
inv _      = undefined


-----


equip :: Action
equip [""]   = descEq 0
equip [r]    = getPlaEq >>= getEntsInInvByName r >>= procGetEntResPlaInv >>= traverse_ (mapM_ descEnt)
equip (r:rs) = equip [r] >> equip rs
equip _      = undefined


descEq :: Id -> MudStack ()
descEq i = (mkEqDescList . mkSlotNameToIdList . M.toList =<< getEqMap i) >>= \edl ->
    if null edl then none else header >> forM_ edl output
  where
    mkSlotNameToIdList    = map (first showText)
    mkEqDescList          = mapM descEqHelper
    descEqHelper (sn, i') = getEnt i' >>= \e ->
        return (T.concat [ parensPad 16 sn, e^.sing, " ", e^.name.to bracketQuote ])
    none
      | i == 0    = dudeYoureNaked
      | otherwise = getEnt i >>= \e -> output $ "The " <> e^.sing <> " doesn't have anything readied."
    header
      | i == 0    = output "You have readied the following equipment:"
      | otherwise = getEnt i >>= \e -> output $ "The " <> e^.sing <> " has readied the following equipment:"


dudeYoureNaked :: MudStack ()
dudeYoureNaked = output "You don't have anything readied. You're naked!"


-----


getAction :: Action
getAction [""] = output "What do you want to get?"
getAction (rs) = do
    is <- getPlaRmInv
    gers <- mapM (\r -> getEntsInInvByName r is) rs
    mesList <- mapM gerToMes gers
    let misList = pruneDupIds [] $ (fmap . fmap . fmap) (^.entId) mesList
    mapM_ procGerMisForGet $ zip gers misList


pruneDupIds :: Inv -> [Maybe Inv] -> [Maybe Inv]
pruneDupIds _       []               = []
pruneDupIds uniques (Nothing : rest) = Nothing : pruneDupIds uniques rest
pruneDupIds uniques (Just is : rest) = let is' = deleteAllInList uniques is
                                       in Just is' : pruneDupIds (is' ++ uniques) rest


procGerMisForGet :: (GetEntResult, Maybe Inv) -> MudStack ()
procGerMisForGet (_,                     Just []) = return ()
procGerMisForGet (Sorry n,               Nothing) = output $ "You don't see " <> aOrAn n <> " here."
procGerMisForGet (Mult 1 n Nothing,      Nothing) = output $ "You don't see " <> aOrAn n <> " here."
procGerMisForGet (Mult _ n Nothing,      Nothing) = output $ "You don't see any " <> n <> "s here."
procGerMisForGet (Mult _ _ (Just _),     Just is) = shuffleInvGet is
procGerMisForGet (Indexed _ n (Left ""), Nothing) = output $ "You don't see any " <> n <> "s here."
procGerMisForGet (Indexed x _ (Left p),  Nothing) = outputCon [ "You don't see ", showText x, " ", p, " here." ]
procGerMisForGet (Indexed _ _ (Right _), Just is) = shuffleInvGet is
procGerMisForGet _                                = undefined


shuffleInvGet :: Inv -> MudStack ()
shuffleInvGet is = getPlaRmId >>= \i ->
    moveInv is i 0 >> descGetDrop Get is


descGetDrop :: GetOrDrop -> Inv -> MudStack ()
descGetDrop god is = mkNameCountBothList is >>= mapM_ descGetDropHelper
  where
    descGetDropHelper (_, c, (s, _))
      | c == 1 = outputCon [ "You", verb, "the ", s, "." ]
    descGetDropHelper (_, c, both) = outputCon [ "You", verb, showText c, makePlurFromBoth both, "." ]
    verb      = case god of Get  -> " pick up "
                            Drop -> " drop "


-----


dropAction :: Action
dropAction [""] = output "What do you want to drop?"
dropAction (rs) = getPlaInv >>= \is ->
    if null is then dudeYourHandsAreEmpty else do
        gers <- mapM (\r -> getEntsInInvByName r is) rs
        mesList <- mapM gerToMes gers
        let misList = pruneDupIds [] $ (fmap . fmap . fmap) (^.entId) mesList
        mapM_ procGerMisForDrop $ zip gers misList


procGerMisForDrop :: (GetEntResult, Maybe Inv) -> MudStack ()
procGerMisForDrop (_,                     Just []) = return ()
procGerMisForDrop (Sorry n,               Nothing) = output $ "You don't have " <> aOrAn n <> "."
procGerMisForDrop (Mult 1 n Nothing,      Nothing) = output $ "You don't have " <> aOrAn n <> "."
procGerMisForDrop (Mult _ n Nothing,      Nothing) = output $ "You don't have any " <> n <> "s."
procGerMisForDrop (Mult _ _ (Just _),     Just is) = shuffleInvDrop is
procGerMisForDrop (Indexed _ n (Left ""), Nothing) = output $ "You don't have any " <> n <> "s."
procGerMisForDrop (Indexed x _ (Left p),  Nothing) = outputCon [ "You don't have ", showText x, " ", p, "." ]
procGerMisForDrop (Indexed _ _ (Right _), Just is) = shuffleInvDrop is
procGerMisForDrop _                                = undefined


shuffleInvDrop :: Inv -> MudStack ()
shuffleInvDrop is = getPlaRmId >>= \i ->
    moveInv is 0 i >> descGetDrop Drop is


-----


putAction :: Action
putAction [""] = output "What do you want to put? And where do you want to put it?"
putAction [_]  = output "Where do you want to put it?"
putAction rs   = getPlaInv >>= \is ->
    if null is then dudeYourHandsAreEmpty else putRemDispatcher Put rs


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
      | T.head cn == rmChar = getPlaRmInv >>= getEntsInInvByName (T.tail cn) >>= procGetEntResRm
      | otherwise = getPlaInv >>= getEntsInInvByName cn >>= procGetEntResPlaInv
    onlyOneMsg         = case por of Put -> "You can only put things into one container at a time."
                                     Rem -> "You can only remove things from one container at a time."
    dispatchToHelper i = case por of Put -> putHelper i restWithoutCon 
                                     Rem -> remHelper i restWithoutCon
      where
        restWithoutCon = r : (init rs)
putRemDispatcher _ _ = undefined


putHelper :: Id -> Rest -> MudStack ()
putHelper _  []   = return ()
putHelper ci (rs) = do
    is <- getPlaInv
    gers <- mapM (\r -> getEntsInInvByName r is) rs
    mesList <- mapM gerToMes gers
    let misList = pruneDupIds [] $ (fmap . fmap . fmap) (^.entId) mesList
    mapM_ (procGerMisForPut ci) $ zip gers misList


procGerMisForPut :: Id -> (GetEntResult, Maybe Inv) -> MudStack ()
procGerMisForPut _  (_,                     Just []) = return ()
procGerMisForPut _  (Sorry n,               Nothing) = output $ "You don't have " <> aOrAn n <> "."
procGerMisForPut _  (Mult 1 n Nothing,      Nothing) = output $ "You don't have " <> aOrAn n <> "."
procGerMisForPut _  (Mult _ n Nothing,      Nothing) = output $ "You don't have any " <> n <> "s."
procGerMisForPut ci (Mult _ _ (Just _),     Just is) = shuffleInvPut ci is
procGerMisForPut _  (Indexed _ n (Left ""), Nothing) = output $ "You don't have any " <> n <> "s."
procGerMisForPut _  (Indexed x _ (Left p),  Nothing) = outputCon [ "You don't have ", showText x, " ", p, "." ]
procGerMisForPut ci (Indexed _ _ (Right _), Just is) = shuffleInvPut ci is
procGerMisForPut _  _                                = undefined


shuffleInvPut :: Id -> Inv -> MudStack ()
shuffleInvPut ci is = do
    cn <- (^.sing) <$> getEnt ci
    is' <- checkImplosion cn
    moveInv is' 0 ci >> descPutRem Put is' cn
  where
    checkImplosion cn = if ci `elem` is
                          then output ("You can't put the " <> cn <> " inside itself.") >> return (filter (/= ci) is)
                          else return is


descPutRem :: PutOrRem -> Inv -> ConName -> MudStack ()
descPutRem por is cn = mkNameCountBothList is >>= mapM_ descPutRemHelper
  where
    descPutRemHelper (_, c, _)
      | c == 1                    = outputCon [ "You", verb, " the ", prep, cn, "." ]
    descPutRemHelper (_, c, both) = outputCon [ "You", verb, showText c, " ", makePlurFromBoth both, prep, cn, "." ]
    verb = case por of Put -> " put "
                       Rem -> " remove "
    prep = case por of Put -> " in the "
                       Rem -> " from the "


-----


remove :: Action
remove [""] = output "What do you want to remove? And what do you want to remove it from?"
remove [_]  = output "What do you want to remove it from?"
remove rs   = putRemDispatcher Rem rs


remHelper :: Id -> Rest -> MudStack ()
remHelper _  []   = return ()
remHelper ci (rs) = do
    cn <- (^.sing) <$> getEnt ci
    is <- getInv ci
    if null is then output $ "The " <> cn <> " appears to be empty." else do
        gers <- mapM (\r -> getEntsInInvByName r is) rs
        mesList <- mapM gerToMes gers
        let misList = pruneDupIds [] $ (fmap . fmap . fmap) (^.entId) mesList
        mapM_ (procGerMisForRem ci cn) $ zip gers misList


procGerMisForRem :: Id -> ConName -> (GetEntResult, Maybe Inv) -> MudStack ()
procGerMisForRem _  _  (_,                     Just []) = return ()
procGerMisForRem _  cn (Sorry n,               Nothing) = outputCon [ "The ", cn, " doesn't contain ", aOrAn n, "." ]
procGerMisForRem _  cn (Mult 1 n Nothing,      Nothing) = outputCon [ "The ", cn, " doesn't contain ", aOrAn n, "." ]
procGerMisForRem _  cn (Mult _ n Nothing,      Nothing) = outputCon [ "The ", cn, " doesn't contain any ", n, "s." ] 
procGerMisForRem ci cn (Mult _ _ (Just _),     Just is) = shuffleInvRem ci cn is
procGerMisForRem _  cn (Indexed _ n (Left ""), Nothing) = outputCon [ "The ", cn, " doesn't contain any ", n, "s." ] 
procGerMisForRem _  cn (Indexed x _ (Left p),  Nothing) = outputCon [ "The ", cn, " doesn't contain ", showText x, " ", p, "." ]
procGerMisForRem ci cn (Indexed _ _ (Right _), Just is) = shuffleInvRem ci cn is
procGerMisForRem _  _  _                                = undefined


shuffleInvRem :: Id -> ConName -> Inv -> MudStack ()
shuffleInvRem ci cn is = moveInv is ci 0 >> descPutRem Rem is cn


-----


ready :: Action
ready [""] = output "What do you want to ready?"
ready (rs) = getPlaInv >>= \is ->
    if null is then dudeYourHandsAreEmpty else do
        res <- mapM (\r -> getEntsToReadyByName r is) rs
        let gers  = res^..folded._1
        let mrols = res^..folded._2
        mesList <- mapM gerToMes gers
        let misList = pruneDupIds [] $ (fmap . fmap . fmap) (^.entId) mesList
        mapM_ procGerMisMrolForReady $ zip3 gers misList mrols


getEntsToReadyByName :: T.Text -> Inv -> MudStack (GetEntResult, Maybe RightOrLeft)
getEntsToReadyByName searchName is
  | slotChar `elem` searchName^.unpacked = let (xs, ys) = T.break (== slotChar) searchName
                                           in if T.length ys == 1 then sorry else do
                                               ger <- getEntsInInvByName xs is
                                               let try = reads (ys^..unpacked.dropping 1 (folded.to toUpper)) :: [(RightOrLeft, String)]
                                               case try of [(rol, _)] -> return (ger, Just rol)
                                                           _          -> sorry
  | otherwise = getEntsInInvByName searchName is >>= \ger -> return (ger, Nothing)
  where
    sorry = return (Sorry searchName, Nothing)


procGerMisMrolForReady :: (GetEntResult, Maybe Inv, Maybe RightOrLeft) -> MudStack ()
procGerMisMrolForReady (_,                     Just [], _)    = return ()
procGerMisMrolForReady (Sorry n,               Nothing, _)    = sorryCantReady n
procGerMisMrolForReady (Mult 1 n Nothing,      Nothing, _)    = output $ "You don't have " <> aOrAn n <> "."
procGerMisMrolForReady (Mult _ n Nothing,      Nothing, _)    = output $ "You don't have any " <> n <> "s."
procGerMisMrolForReady (Mult _ _ (Just _),     Just is, mrol) = readyDispatcher mrol is
procGerMisMrolForReady (Indexed _ n (Left ""), Nothing, _)    = output $ "You don't have any " <> n <> "s."
procGerMisMrolForReady (Indexed x _ (Left p),  Nothing, _)    = outputCon [ "You don't have ", showText x, " ", p, "." ]
procGerMisMrolForReady (Indexed _ _ (Right _), Just is, mrol) = readyDispatcher mrol is
procGerMisMrolForReady _                                      = undefined


sorryCantReady :: T.Text -> MudStack ()
sorryCantReady n
  | slotChar `elem` n^.unpacked = outputCon [ "Please specify ", dblQuote "r", " or ", dblQuote "l", ".\n", ringHelp ]
  | otherwise = output $ "You don't have " <> aOrAn n <> "."


ringHelp :: T.Text
ringHelp = T.concat [ "For rings, specify ", dblQuote "r", " or ", dblQuote "l", " immediately followed by:\n"
                    , dblQuote "i", " for index finger,\n"
                    , dblQuote "m", " for middle finter,\n"
                    , dblQuote "r", " for ring finger,\n"
                    , dblQuote "p", " for pinky finger." ]


readyDispatcher :: Maybe RightOrLeft -> Inv -> MudStack ()
readyDispatcher mrol = mapM_ dispatchByType
  where
    dispatchByType i = do
        e <- getEnt i
        em <- getPlaEqMap
        t <- getEntType e
        case t of ClothType -> getCloth i >>= \c -> readyCloth i e c em mrol
                  WpnType   -> readyWpn i e em mrol
                  ArmType   -> undefined -- TODO
                  _         -> output $ "You can't ready a " <> e^.sing <> "."


-- Helpers for the entity type-specific ready functions:


moveReadiedItem :: Id -> EqMap -> Slot -> MudStack ()
moveReadiedItem i em s = eqTbl.at 0 ?= (em & at s ?~ i) >> remFromInv [i] 0


otherHand :: Hand -> Hand
otherHand RHand = LHand
otherHand LHand = RHand
otherHand _     = undefined


isRingROL :: RightOrLeft -> Bool
isRingROL rol = case rol of R -> False
                            L -> False
                            _ -> True


neckSlots, rWristSlots, lWristSlots :: [Slot]
neckSlots   = [Neck1S   .. Neck3S]
rWristSlots = [RWrist1S .. RWrist3S]
lWristSlots = [LWrist1S .. LWrist3S]


isSlotAvail :: EqMap -> Slot -> Bool
isSlotAvail em s = isNothing $ em^.at s


findAvailSlot :: EqMap -> [Slot] -> Maybe Slot
findAvailSlot em = find (isSlotAvail em)


sorryFullClothSlots :: Cloth -> MudStack ()
sorryFullClothSlots c = output $ "You can't wear any more accessories on your " <> loc <> "."
  where
    loc = case c of NeckC   -> "neck"
                    WristC  -> "wrists"
                    FingerC -> "fingers"
                    _       -> undefined -- TODO


sorryFullClothSlotsOneSide :: Slot -> MudStack ()
sorryFullClothSlotsOneSide s = output $ "You can't wear any more accessories on your " <> showText s <> "."


-- Ready clothing:


readyCloth :: Int -> Ent -> Cloth -> EqMap -> Maybe RightOrLeft -> MudStack ()
readyCloth i e c em mrol = maybe (getAvailClothSlot c em) (getDesigClothSlot e c em) mrol >>= \ms ->
    maybe (return ()) (\s -> moveReadiedItem i em s >> readiedMsg s) ms
  where
    readiedMsg s = case c of NeckC   -> wearNeckMsg
                             WristC  -> wearGenericMsg
                             FingerC -> wearRingMsg
                             _       -> undefined -- TODO
      where
        wearNeckMsg    = output $ "You put on the " <> e^.sing <> "."
        wearRingMsg    = outputCon [ "You slide the ", e^.sing, " on your ", showText s, "." ]
        wearGenericMsg = outputCon [ "You wear the ", e^.sing, " on your ", showText s, "." ]


getDesigClothSlot :: Ent -> Cloth -> EqMap -> RightOrLeft -> MudStack (Maybe Slot)
getDesigClothSlot e c em rol = case c of NeckC   -> sorryNotForHand
                                         WristC  | isRingROL rol -> sorryNotRing
                                                 | otherwise     -> maybe sorryFullWrist (return . Just) desigWristSlot
                                         FingerC | not . isRingROL $ rol -> sorryNotRingROL
                                                 | otherwise             -> maybe (return (Just desigRingSlot))
                                                                                  (getEnt >=> sorry desigRingSlot)
                                                                                  (em^.at desigRingSlot)
                                         _       -> undefined -- TODO
  where
    sorryNotForHand = output ("You can't wear a " <> e^.sing <> " on your hand!")   >> return Nothing
    sorryNotRing    = output ("You can't wear a " <> e^.sing <> " on your finger!") >> return Nothing
    sorryFullWrist  = sorryFullClothSlotsOneSide s >> return Nothing
      where s = case rol of R -> head rWristSlots
                            L -> head lWristSlots
                            _ -> undefined
    desigWristSlot  = case rol of R -> findAvailSlot em rWristSlots
                                  L -> findAvailSlot em lWristSlots
                                  _ -> undefined
    sorryNotRingROL = output ringHelp >> return Nothing
    desigRingSlot   = case rol of RI -> RIndexFS
                                  RM -> RMidFS
                                  RR -> RRingFS
                                  RP -> RPinkyFS
                                  LI -> LIndexFS
                                  LM -> LMidFS
                                  LR -> LRingFS
                                  LP -> LPinkyFS
                                  _  -> undefined
    sorry s e'      = outputCon [ "You're already wearing a ", e'^.sing, " on your ", showText s, "." ] >> return Nothing


getAvailClothSlot :: Cloth -> EqMap -> MudStack (Maybe Slot)
getAvailClothSlot c em = getPlaMobHand >>= \h ->
    case c of NeckC   -> procMaybe $ findAvailSlot em neckSlots
              WristC  -> procMaybe $ getWristSlotForHand h `mplus` (getWristSlotForHand . otherHand $ h)
              FingerC -> getRingSlotForHand h >>= procMaybe
              _       -> undefined -- TODO
  where
    procMaybe             = maybe (sorryFullClothSlots c >> return Nothing) (return . Just)
    getWristSlotForHand h = case h of RHand -> findAvailSlot em lWristSlots
                                      LHand -> findAvailSlot em rWristSlots
                                      _     -> undefined
    getRingSlotForHand h  = getPlaMobSex >>= \s ->
        return $ case s of Male   -> case h of RHand -> findAvailSlot em [LRingFS, LIndexFS, RRingFS, RIndexFS, LMidFS, RMidFS, LPinkyFS, RPinkyFS]
                                               LHand -> findAvailSlot em [RRingFS, RIndexFS, LRingFS, LIndexFS, RMidFS, LMidFS, RPinkyFS, LPinkyFS]
                                               _     -> undefined
                           Female -> case h of RHand -> findAvailSlot em [LRingFS, LIndexFS, RRingFS, RIndexFS, LPinkyFS, RPinkyFS, LMidFS, RMidFS]
                                               LHand -> findAvailSlot em [RRingFS, RIndexFS, LRingFS, LIndexFS, RPinkyFS, LPinkyFS, RMidFS, LMidFS]
                                               _     -> undefined
                           _      -> undefined


-- Ready weapons:


readyWpn :: Id -> Ent -> EqMap -> Maybe RightOrLeft -> MudStack ()
readyWpn i e em mrol
  | not . isSlotAvail em $ BothHandsS = output "You're already wielding a two-handed weapon."
  | otherwise = maybe (getAvailWpnSlot em) (getDesigWpnSlot e em) mrol >>= \ms ->
                    maybe (return ()) (\s -> getWpn i >>= readyHelper s) ms
  where
    readyHelper s w = case w^.wpnSub of OneHanded -> moveReadiedItem i em s >> outputCon [ "You wield the ", e^.sing, " with your ", showText s, "." ]
                                        TwoHanded -> if all (isSlotAvail em) [RHandS, LHandS]
                                                       then moveReadiedItem i em BothHandsS >> output ("You wield the " <> e^.sing <> " with both hands.")
                                                       else output $ "Both hands are required to weild the " <> e^.sing <> "."


getDesigWpnSlot :: Ent -> EqMap -> RightOrLeft -> MudStack (Maybe Slot)
getDesigWpnSlot e em rol
  | isRingROL rol = sorryNotRing
  | otherwise     = maybe (return (Just desigSlot)) (getEnt >=> sorry) $ em^.at desigSlot
  where
    sorryNotRing = output ("You can't wield a " <> e^.sing <> " with your finger!") >> return Nothing
    sorry e'     = outputCon [ "You're already wielding a ", e'^.sing, " with your ", showText desigSlot, "." ] >> return Nothing
    desigSlot    = case rol of R -> RHandS
                               L -> LHandS
                               _ -> undefined


getAvailWpnSlot :: EqMap -> MudStack (Maybe Slot)
getAvailWpnSlot em = getPlaMobHand >>= \h ->
    maybe sorry (return . Just) (findAvailSlot em . map getSlotForHand $ [h, otherHand h])
  where
    getSlotForHand h = case h of RHand -> RHandS
                                 LHand -> LHandS
                                 _     -> undefined
    sorry = output "You're already wielding two weapons." >> return Nothing


-----


unready :: Action
unready [""] = output "What do you want to unready?"
unready rs   = getPlaEq >>= \is ->
    if null is then dudeYoureNaked else do
        gers <- mapM (\r -> getEntsInInvByName r is) rs
        mesList <- mapM gerToMes gers
        let misList = pruneDupIds [] $ (fmap . fmap . fmap) (^.entId) mesList
        mapM_ procGerMisForUnready $ zip gers misList


procGerMisForUnready :: (GetEntResult, Maybe Inv) -> MudStack ()
procGerMisForUnready (_,                     Just []) = return ()
procGerMisForUnready (Sorry n,               Nothing) = output $ "You don't have " <> aOrAn n <> " among your readied equipment."
procGerMisForUnready (Mult 1 n Nothing,      Nothing) = output $ "You don't have " <> aOrAn n <> " among your readied equipment."
procGerMisForUnready (Mult _ n Nothing,      Nothing) = output $ "You don't have any " <> n <> "s among your readied equipment."
procGerMisForUnready (Mult _ _ (Just _),     Just is) = shuffleInvUnready is
procGerMisForUnready (Indexed _ n (Left ""), Nothing) = output $ "You don't have any " <> n <> "s among your readied equipment."
procGerMisForUnready (Indexed x _ (Left p),  Nothing) = outputCon [ "You don't have ", showText x, " ", p, " readied." ]
procGerMisForUnready (Indexed _ _ (Right _), Just is) = shuffleInvUnready is
procGerMisForUnready _                                = undefined


shuffleInvUnready :: Inv -> MudStack ()
shuffleInvUnready is = M.filter (`notElem` is) <$> getPlaEqMap >>= (eqTbl.at 0 ?=) >> addToInv is 0 >> descUnready is


descUnready :: Inv -> MudStack ()
descUnready is = mkIdCountBothList is >>= mapM_ descUnreadyHelper
  where
    descUnreadyHelper (i, c, both@(s, _)) = verb i >>= \v ->
        outputCon $ if c == 1
          then [ "You ", v, "the ", s, "." ]
          else [ "You ", v, showText c, " ", makePlurFromBoth both, "." ]
    verb i = getEnt i >>= getEntType >>= \t ->
        case t of ClothType -> getCloth i >>= \c ->
                                   case c of WristC  -> return unwearGenericVerb
                                             FingerC -> return unwearGenericVerb
                                             _       -> undefined -- TODO
                  WpnType   -> return "stop wielding "
                  _         -> undefined -- TODO
      where
        unwearGenericVerb = "take off "


mkIdCountBothList :: Inv -> MudStack [(Id, Int, BothGramNos)]
mkIdCountBothList is = getEntBothGramNosInInv is >>= \ebgns ->
    let cs = makeCountList ebgns
    in return (nubBy equalCountsAndBoths . zip3 is cs $ ebgns)
      where
        equalCountsAndBoths (_, c, b) (_, c', b') = c == c' && b == b'


-----


okapi :: Action
okapi _ = mkOkapi >>= \i ->
    output $ "Made okapi with id " <> showText i <> "."


-----


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


-----


dumpEnv :: Action
dumpEnv [""] = lift $ getEnvironment >>= dumpAssocList
dumpEnv [r]  = lift $ dumpAssocList . filter grepPair =<< getEnvironment
  where
    grepPair (k, v) = r `T.isInfixOf` (k^.packed) || r `T.isInfixOf` (v^.packed)
dumpEnv (r:rs) = dumpEnv [r] >> dumpEnv rs
dumpEnv _      = undefined


-----


uptime :: Action
uptime _ = output . parse =<< (lift . readProcess "/usr/bin/uptime" [] $ "")
  where
    parse ut = let (a, b) = span (/= ',') ut
                   a' = unwords . tail . words $ a
                   b' = dropWhile isSpace . takeWhile (/= ',') . tail $ b
                   c  = (toUpper . head $ a') : (tail a')
               in T.concat [ c^.packed, " ", b'^.packed, "." ]
