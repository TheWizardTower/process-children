{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( getChildrenOp
    ) where

import           Control.Monad         (filterM, join, mapM)
import           Data.ByteString       ()
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 (lines, pack, unpack, words)
import           Data.List             (foldl')
import qualified Data.Map              as M
import qualified Data.Set              as S
import           Prelude               (Bool (..), IO, Int, Maybe (..), String,
                                        filter, fmap, head, length, print, read,
                                        return, show, (!!), ($), (++), (/=),
                                        (>>=))
import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        listDirectory)
import           System.IO             (IOMode (..), withFile)
import           Text.Regex            (Regex, matchRegex, mkRegex)
import           Text.Regex.Base       ()

pidRegex :: Regex
pidRegex = mkRegex "^/proc/([0-9]+)$"

matchRegexBool :: Regex -> String -> Bool
matchRegexBool regex str = case matchRegex regex str of
  Nothing -> False
  Just _  -> True

appendMatchSubstrToList :: Regex -> [Int] -> String -> [Int]
appendMatchSubstrToList regex list str =
  case matchRegex regex str of
    Nothing           -> list
    Just subexprMatch -> (list ++ [(read (head subexprMatch) :: Int)])

getPidInts :: [Int] -> String -> [Int]
getPidInts list pidstr = appendMatchSubstrToList pidRegex list pidstr

getPIDPaths :: IO [Int]
getPIDPaths = do
  contents <- listDirectory "/proc"
  procDirectories <- filterM doesDirectoryExist (fmap ("/proc/" ++) contents)
  let pidDirectories = filter (matchRegexBool pidRegex) procDirectories
      pidIntList = foldl' getPidInts [] pidDirectories
  return pidIntList

ppidRegex :: Regex
ppidRegex = mkRegex "PPid:\t([0-9]+)"

getPPidInts :: [Int] -> B.ByteString -> [Int]
getPPidInts list pidstr = appendMatchSubstrToList ppidRegex list (unpack pidstr)

-- This is hideous and shameful. My honor is disgraced.
getPIDParent :: Int -> IO (Maybe (Int, Int))
getPIDParent pid = do
  let statsPath = "/proc/" ++ show pid ++ "/status"
  statsFileExists <- doesFileExist statsPath
  if statsFileExists then do
    contents <- withFile statsPath ReadMode B.hGetContents
    let contentLines = lines contents
        statsContents = foldl' (getPPidInts) [] contentLines
    return $ Just (pid, head statsContents)
  else
    return Nothing

getParentPidList :: IO [Maybe (Int, Int)]
getParentPidList = getPIDPaths >>= (\a -> mapM getPIDParent a)

buildPIDChildrenMap :: [Maybe (Int, Int)] -> M.Map Int (S.Set Int)
buildPIDChildrenMap list =
  foldl' (\map maybePair ->
    case maybePair of
      Nothing -> map
      Just (pid, pidParent) -> M.insertWith
        (S.union)
       pidParent
       (S.singleton pid)
       map
  ) (M.fromList []) list

getChildren :: (M.Map Int (S.Set Int)) -> Int -> [Int]
getChildren map pid = do
  let childrenSetMaybe = M.lookup pid map
  case childrenSetMaybe of
    Nothing  -> []
    Just set -> let list = S.toList set in
        list ++ (join $ fmap (getChildren map) list)

getChildrenOp :: Int -> IO [Int]
getChildrenOp pidRoot = do
  ppid_list <- getParentPidList
  let map = buildPIDChildrenMap ppid_list
      pidList = getChildren map pidRoot
  return pidList

-- pidPaths <- getPIDPaths
-- pidParentPairs <- mapM getPIDParent pidPaths
