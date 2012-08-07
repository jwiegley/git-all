#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

-- git-all.hs, version 1.0 (2012-08-06)
--
-- by John Wiegley <johnw@newartisans.com>
--
-- A utility for determining which Git repositories need to actions to be
-- taken within them.

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Loops
import Data.Char
import Data.Function
import Data.Maybe
import Filesystem.Path (FilePath, directory, filename)
import GHC.Conc
import System.Console.CmdArgs
import System.Directory
import System.Environment
import System.Exit
import System.IO hiding (FilePath)
import System.IO.Storage
import System.Log.Logger
import System.Process
import System.Time
import qualified Data.List as L
import qualified Control.Exception as C

import Shelly hiding (FilePath)
import Data.Text.Lazy as T
import Text.Regex.Posix
import Prelude hiding (FilePath, drop, replicate, lines, take, length, unlines, map)
default (T.Text)

version       = "1.0.0"
copyright     = "2012"
gitAllSummary = "git-all v" ++ version ++ ", (C) John Wiegley " ++ copyright

data GitAll = GitAll
    { fetch     :: Bool
    , fetchonly :: Bool
    , errors    :: Bool
    , pulls     :: Bool
    , untracked :: Bool
    , verbose   :: Bool
    , debug     :: Bool
    , dirs      :: [String]
    }
    deriving (Data, Typeable, Show, Eq)

gitAll = GitAll
    { fetch     = def &= name "f" &= help "Fetch from all remotes"
    , fetchonly = def &= name "F" &= help "Only fetch, nothing else"
    , errors    = def &= name "E" &= help "Only show Git failures"
    , pulls     = def &= name "p" &= help "Include NEED PULL sections"
    , untracked = def &= name "U" &= help "Display untracked files as possible changes"
    , verbose   = def &= name "v" &= help "Report progress verbosely"
    , debug     = def &= name "D" &= help "Report debug information"

    , dirs      = def &= opt "." &= args &= typ "DIR..."
    } &=
    summary gitAllSummary &=
    program "git-all" &=
    helpArg [explicit, name "h"] &=
    help "Report the status of all Git repositories within a directory tree"


-- "Impure" aspects of this utility:
--
-- 1. Walking the filesystem to find *.git/config files.
--
-- 2. Reading the config files to discover what type of remote it has
--    (regular, git-svn, git-bzr, gc-update, etc).
--
-- 3. Calling out to git to perform the actual update

main :: IO ()
main = do
  -- use 2 cores; this utility must be linked with -threaded
  GHC.Conc.setNumCapabilities 2

  -- process command-line options
  opts <- cmdArgs gitAll
  when (verbose opts) $ updateGlobalLogger "git-all" (setLevel INFO)
  when (debug opts)   $ updateGlobalLogger "git-all" (setLevel DEBUG)

  -- Do a find in all directories (in sequence) in a separate thread, so that
  -- the list of directories is accumulated while we work on them
  c <- newChan
  forkIO $ findDirectories c ".git" (L.map (fromText . pack) (dirs opts))

  -- While readChan keeps returning `Just x', call `checkGitDirectory opts x'.
  -- Once it returns Nothing, findDirectories is done and we can stop
  whileJust_ (readChan c) (checkGitDirectory opts)

-- Primary logic

checkGitDirectory :: GitAll -> FilePath -> IO ()
checkGitDirectory opts dir = do
  debugM "git-all" $ "Scanning " ++ (asText dir)

  when (fetch opts || fetchonly opts) $ do
    gitFetch dir

  unless (fetchonly opts) $ do
    branches <- gitLocalBranches dir
    mapM_ (gitPushOrPull dir) branches

    gitStatus dir (fetch opts) (untracked opts)

-- Git command wrappers

gitStatus :: FilePath -> Bool -> Bool -> IO ()
gitStatus dir fetch untracked = do
  changes <-
    git dir "status"
        [ "--porcelain"
        , append "--untracked-files=" (if untracked then "normal" else "no") ]

  putStr $ unpack $ topTen "STATUS" (directory dir) changes "=="

gitFetch :: FilePath -> IO ()
gitFetch dir = do
  url    <- gitConfig dir "svn-remote.svn.url"
  output <-
    if isNothing url
    then do
      git dir "fetch" [ "-q", "--progress", "--all", "--prune", "--tags" ]

    else do
      out <- git dir "svn" [ "fetch" ]
      -- jww (2012-08-06): Why doesn't =~ work on Data.Text.Lazy.Text?
      let pat = unpack "^(W: |This may take a while|Checked through|$)"
      return $ unlines $ L.filter (not . (=~ pat) . unpack) (lines out)

  putStr $ unpack $ topTen "FETCH" (directory dir) output "=="

gitPushOrPull :: FilePath -> Text -> IO ()
gitPushOrPull dir branch = do
  -- jww (2012-08-07): NYI.  The Ruby code to be ported:
  -- remote = g.config["branch.#{branch.name}.remote"] || git_svn_repo(g)
  -- if remote
  --   remote_sha =
  --     if git_svn_repo(g)
  --       g.revparse 'trunk' rescue g.revparse 'git-svn'
  --     else
  --       g.revparse File.join(remote, branch.name)
  --     end
  --
  --   if branch.gcommit.sha != remote_sha
  --     git_log = "git --git-dir='#{@git_dir}' log --no-merges --oneline"
  --
  --     out = `#{git_log} #{remote_sha}..#{branch.gcommit.sha}`
  --     top_ten('NEED PUSH', "#{working_dir}\##{branch.name}",
  --         out.split("\n"), true)
  --
  --     if $params[:pulls].value
  --       out = `#{git_log} #{branch.gcommit.sha}..#{remote_sha}`
  --       top_ten('NEED PULL', "#{working_dir}\##{branch.name}",
  --           out.split("\n"), true)
  --     end
  --   end
  -- end
  putStrLn $ "Push or pull " ++ (asText dir) ++ "#" ++ (unpack branch)

gitLocalBranches :: FilePath -> IO [Text]
gitLocalBranches dir = do
  output <- git dir "for-each-ref" ["refs/heads/"]
  return $ parseGitRefs (lines output)

  -- Each line is of the form: "<HASH> commit refs/heads/<NAME>"
  where parseGitRefs (x:xs) = drop 11 (T.words x !! 2) : parseGitRefs xs
        parseGitRefs [] = []

-- Utility functions

gitConfig :: FilePath -> Text -> IO (Maybe Text)
gitConfig dir option =
  shelly $ silently $ errExit False $ do
    text <- doGit dir "config" [option]
    code <- lastExitCode
    if code == 0
      then return (Just text)
      else return Nothing

doGit :: FilePath -> Text -> [Text] -> Sh Text
doGit dir com args = do
  let gitDir  = T.append "--git-dir=" (toTextIgnore dir)
  let workDir = T.append "--work-tree=" (toTextIgnore (directory dir))
  run "git" $ [gitDir, workDir, com] ++ args

git :: FilePath -> Text -> [Text] -> IO Text
git dir com args = do
  shelly $ silently $ errExit False $ do
    text <- doGit dir com args
    code <- lastExitCode
    unless (code == 0) $ do
      err <- lastStderr
      liftIO $ putStr $ unpack $ topTen "FAILED" (directory dir) err "##"
    return text

topTen :: Text -> FilePath -> Text -> Text -> Text
topTen _ _ "" _ = ""
topTen category path content marker =
  T.concat $ [ "\n", marker, " ", category, " ", marker, " "
             , (toTextIgnore path), "\n"
             , (unlines (L.map (take 80) (L.take 10 ls))) ]
          ++ (let len = L.length (L.drop 10 ls) in
              case len of
                0 -> []
                _ -> [ "... (and " , (pack (show len)) , " more)\n" ])
  where ls = lines content

findDirectories :: Chan (Maybe FilePath) -> FilePath -> [FilePath] -> IO ()
findDirectories c name dirs =
  forM_ dirs $ \dir -> do
    xs <- shelly $ findByName name dir
    mapM_ (writeChan c . Just) xs
    writeChan c Nothing

findByName :: FilePath -> FilePath -> Sh [FilePath]
findByName name = findWhen $ return . (name ==) . filename

asText = unpack . toTextIgnore

-- git-all.hs ends here
