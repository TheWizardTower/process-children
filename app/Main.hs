{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where


import           Control.Monad       (mapM_)
import           Data.ByteString     ()
import           Options.Applicative (Parser, auto, execParser, fullDesc,
                                      header, help, helper, info, long, metavar,
                                      option, progDesc, short, value, (<$>),
                                      (<**>), (<*>))
import           Prelude             (Bool (False), IO, Int, print, ($), (<>))

import           Lib                 (getChildrenOp)

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
   ) <*> option auto
   ( long "include-parents"
  <> short 'P'
  <> help "Show parent pids as well as child ones."
  <> metavar "PARENT"
  <> value False
   )

main :: IO ()
main = do
  let opts = info (cliParser <**> helper)
        ( fullDesc
        <> progDesc "This program takes in a PID and returns the PIDs of that process' children."
        <> header "process-children -- Return the PIDs of children processes in an easily parse-able form."
        )
  args <- execParser opts
  pid_list <- getChildrenOp $ rootPidNumber args
  mapM_ print pid_list
