{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where


import           Control.Monad       (mapM_)
import           Data.ByteString     ()
import           Options.Applicative (Parser, auto, execParser, fullDesc,
                                      header, help, helper, info, long, metavar,
                                      option, progDesc, short, switch,
                                      (<$>), (<**>), (<*>))
import           Prelude             (Bool, IO, Int, print, (<>), (>>=))

import           Lib                 (getChildren, getChildrenAndParent)

data CommandArguments = CommandArguments
    { rootPidNumber          :: Int
    , includeParentProcesses :: Bool
    }

cliParser :: Parser CommandArguments
cliParser = CommandArguments
  <$> option auto
   ( long "pid"
  <> short 'p'
  <> metavar "PID"
  <> help "Process ID of the process you want to get the children of."
   ) <*> switch
   ( long "include-parents"
  <> short 'P'
  <> help "Show parent pids as well as child ones."
   )

main :: IO ()
main = do
  let opts = info (cliParser <**> helper)
        ( fullDesc
        <> progDesc "This program takes in a PID and returns the PIDs of that process' children."
        <> header "process-children -- Return the PIDs of children processes in an easily parse-able form."
        )
  args <- execParser opts
  if includeParentProcesses args then
    getChildrenAndParent (rootPidNumber args) >>= mapM_ print
  else
    getChildren (rootPidNumber args) >>= mapM_ print
