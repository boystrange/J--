-- This file is part of J--

-- J-- is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- J-- is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with J--. If not, see <http://www.gnu.org/licenses/>.

-- Copyright 2022 Luca Padovani

-- |This module parses the command-line arguments and invokes the type checker.
module Main (main) where

import Atoms
import Type
import Language
import SourceLanguage
import Render
import Exceptions (MyException)
import Parser
import qualified Checker
import qualified Compiler
import qualified Jasmin
import qualified Optimizer

import System.Console.GetOpt
import System.IO (stdout, stderr, hFlush, hPutStrLn, openFile, IOMode(..))
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getProgName, getArgs)
import Control.Monad (forM_, unless, when)
import Control.Exception (catch)
import qualified Data.Version
import Data.Time (getCurrentTime, diffUTCTime)
import System.FilePath.Posix (takeFileName, takeBaseName)

-- |Version of the program.
version :: Data.Version.Version
version = Data.Version.makeVersion [1, 0]

-- |Entry point.
main :: IO ()
main = do
  progName <- getProgName
  (args, file) <- getArgs >>= parse progName
  source <- if file == "-" then getContents else readFile file
  let cls = if file == "-" then "default" else takeBaseName file
  case parseProgram file source of
    Left msg -> printWarning msg
    Right (methods, stmts) -> do
      let no_opt = NoOpt `elem` args
      let stmt = foldr Seq Skip stmts
      let main = Method VoidType (Id Somewhere "main") [(Id Somewhere "_args", ArrayType StringType)] stmt
      methods <- Checker.checkClass cls (main : methods)
      methods' <- (if no_opt then id else Optimizer.optimizeMethods) <$> Compiler.compileClass methods
      Jasmin.outputClass cls methods'

-- |Representation of supported flags.
data Flag = Verbose  -- -v --verbose
          | Version  -- -V --version
          | NoOpt    -- -o
          | Logging  --    --log
          | Help     --    --help
            deriving (Eq, Ord)

-- |List of supported flags.
flags :: [OptDescr Flag]
flags =
   [ Option []  ["log"]      (NoArg Logging)     "Log type checking time"
   , Option "v" ["verbose"]  (NoArg Verbose)     "Print type checking and running activities"
   , Option "V" ["version"]  (NoArg Version)     "Print version information"
   , Option "o" []           (NoArg NoOpt)       "Disable code optimization"
   , Option "h" ["help"]     (NoArg Help)        "Print this help message" ]

-- |The information displayed when the verbose option is specified.
versionInfo :: String -> String
versionInfo progName =
  "J-- " ++ Data.Version.showVersion version ++ " Copyright © 2022 Luca Padovani\n"
  ++ "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  ++ "This is free software: you are free to change and redistribute it.\n"
  ++ "There is NO WARRANTY, to the extent permitted by law."

-- |Parse command-line arguments.
parse :: String -> [String] -> IO ([Flag], String)
parse progName argv =
  case getOpt Permute flags argv of
    (args, files, []) -> do
      when (Version `elem` args)
        (do hPutStrLn stderr (versionInfo progName)
            exitWith ExitSuccess)
      when (null files || length files > 1 || Help `elem` args)
        (do hPutStrLn stderr (usageInfo header flags)
            exitWith ExitSuccess)
      return (args, head files)
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header flags)
      exitWith (ExitFailure 1)
  where
    header = "Usage: " ++ progName ++ " [options] [FILE]"
