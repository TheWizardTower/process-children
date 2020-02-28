{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( getPIDPaths
    , getPIDParent
    , getParentPidList
    , buildPIDParentMap
    ) where

import           Control.Monad         (filterM, mapM)
import           Data.ByteString       ()
import qualified Data.ByteString       as B
import           Data.ByteString.Char8 (lines, pack, unpack, words)
import           Data.List             (foldl')
import qualified Data.Map              as M
import           Prelude               (Bool, IO, Int, Maybe (..), filter, fmap,
                                        head, length, read, return, show, (!!),
                                        ($), (++), (/=), (>>=))
import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        listDirectory)
import           System.IO             (IOMode (..), withFile)
import           Text.Regex.Base       ()
import           Text.Regex.PCRE       ((=~))

getPIDPaths :: IO [Int]
getPIDPaths = do
  contents <- listDirectory "/proc"
  procDirectories <- filterM doesDirectoryExist contents
  let pidDirectories = filter (\a -> a =~ "^[0-9]$" :: Bool) procDirectories
  let pidPaths = fmap (\a -> read a :: Int) pidDirectories
  return pidPaths


-- This is hideous and shameful. My honor is disgraced.
getPIDParent :: Int -> IO (Maybe (Int, Int))
getPIDParent pid = do
  let statsPath = "/proc/" ++ show pid ++ "/status"
  statsFileExists <- doesFileExist statsPath
  if statsFileExists then do
    contents <- withFile statsPath ReadMode B.hGetContents
    let contentLines = lines contents
        statsContents = filter (B.isPrefixOf (pack "PPID: ")) contentLines
        statsPPidTokens = fmap words statsContents
    if length statsContents /= 1 then
      return Nothing
    else
      if length (head statsPPidTokens) /= 2 then
        return Nothing
     else do
        let wordList = statsPPidTokens !! 0
            ppid = wordList !! 1
            ppid_int = read (unpack ppid) :: Int
        return $ Just (pid, ppid_int)
  else
    return Nothing

getParentPidList :: IO [Maybe (Int, Int) ]
getParentPidList = getPIDPaths >>= (\a -> mapM getPIDParent a)

buildPIDParentMap :: [Maybe (Int, Int)] -> IO (M.Map Int Int)
buildPIDParentMap list = do
  let pid_map = foldl' (\map maybePair -> case maybePair of
                           Nothing -> map
                           Just (pid, pidParent) -> M.insert pid pidParent map) (M.fromList []) list
  return pid_map

-- pidPaths <- getPIDPaths
-- pidParentPairs <- mapM getPIDParent pidPaths
