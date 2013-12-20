{-# OPTIONS_GHC -funbox-strict-fields -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}

module Mud.Convenience ( (<>)
                       , aOrAn
                       , bracketPad
                       , bracketQuote
                       , dblQuote
                       , deleteAllInList
                       , dumpAssocList
                       , dumpFile
                       , findAbbrev
                       , makeCountList
                       , mkOrdinal
                       , newLine
                       , output
                       , outputCon
                       , parensPad
                       , parensQuote
                       , showText
                       , unquote ) where

import Mud.StateDataTypes

import Control.Lens (_1, _2, folded, to)
import Control.Lens.Operators ((^.), (^..))
import Control.Monad.Trans.Class (lift)
import Data.List (delete, foldl', sort)
import Data.Text.Strict.Lens (unpacked)
import qualified Data.Text as T
import qualified Data.Text.IO as T


newLine :: IO ()
newLine = putChar '\n'


output :: T.Text -> MudStack ()
output = lift . T.putStrLn


outputCon :: [T.Text] -> MudStack () -- Prefer over "output" when there would be more than two "<>"s.
outputCon = output . T.concat


infixl 7 <>
(<>) :: T.Text -> T.Text -> T.Text
(<>) = T.append


showText :: (Show a) => a -> T.Text
showText = T.pack . show


aOrAn :: T.Text -> T.Text
aOrAn "" = undefined
aOrAn t
  | T.head t `elem` "aeiou"^.unpacked = "an " <> t
  | otherwise = "a " <> t


quoteWith :: (T.Text, T.Text) -> T.Text -> T.Text
quoteWith (a, b) t = T.concat [ a, t, b ]


dblQuote :: T.Text -> T.Text
dblQuote = quoteWith ("\"", "\"")


bracketQuote :: T.Text -> T.Text
bracketQuote = quoteWith ("[", "]")


parensQuote :: T.Text -> T.Text
parensQuote = quoteWith ("(", ")")


unquote :: T.Text -> T.Text
unquote = T.init . T.tail


quoteWithAndPad :: (T.Text, T.Text) -> Int -> T.Text -> T.Text
quoteWithAndPad q x t = quoteWith q t' <> T.replicate p " "
  where
    t' = T.take (x - l - 1) t
    l  = sum $ [q^._1, q^._2]^..folded.to T.length
    p  = x - T.length t' - 2


bracketPad :: Int -> T.Text -> T.Text
bracketPad = quoteWithAndPad ("[", "]")


parensPad :: Int -> T.Text -> T.Text
parensPad = quoteWithAndPad ("(", ")")


findAbbrev :: T.Text -> [T.Text] -> Maybe T.Text
findAbbrev needle hay = if null res then Nothing else Just . head $ res
  where
    res = sort . filter (needle `T.isPrefixOf`) $ hay


deleteAllInList :: (Eq a) => [a] -> [a] -> [a]
deleteAllInList delThese fromThese = foldl' (\acc x -> delete x acc) fromThese delThese


dumpFile :: FilePath -> IO ()
dumpFile fn = T.putStr =<< T.readFile fn


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


makeCountList :: (Eq a) => [a] -> [Int]
makeCountList xs = [ length (filter (==x) xs) | x <- xs ]
