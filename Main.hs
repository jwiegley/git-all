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
-- A utility for determining which Git repositories need actions to be taken
-- within them.

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Loops
import qualified Data.List as L
import Data.Maybe
import Data.Text.Lazy as T
import Filesystem.Path (directory, filename)
import GHC.Conc
import Shelly
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Log.Logger
import Text.Regex.Posix

import Prelude
  hiding (FilePath, drop, replicate, lines, take, length, unlines, map)
default (T.Text)

version       = "1.0.1"
copyright     = "2012"
gitAllSummary = "git-all v" ++ version ++ ", (C) John Wiegley " ++ copyright

data GitAll = GitAll
    { pulls     :: Bool
    , untracked :: Bool
    , verbose   :: Bool
    , debug     :: Bool
    , arguments :: [String] }
    deriving (Data, Typeable, Show, Eq)

gitAll = GitAll
    { pulls     = def &= name "p" &= help "Include NEED PULL sections"
    , untracked = def &= name "U" &= help "Display untracked files as possible changes"
    , verbose   = def &= name "v" &= help "Report progress verbosely"
    , debug     = def &= name "D" &= help "Report debug information"
    , arguments = def &= args &= typ "fetch | status" } &=
    summary gitAllSummary &=
    program "git-all" &=
    help "Fetch or report status of all Git repositories under the current directory"

main :: IO ()
main = do
  -- use 2 cores; this utility must be linked with -threaded
  GHC.Conc.setNumCapabilities 2

  -- process command-line options
  args <- getArgs
  opts <- (if L.null args then withArgs ["--help"] else id) (cmdArgs gitAll)

  when (verbose opts) $ updateGlobalLogger "git-all" (setLevel INFO)
  when (debug opts)   $ updateGlobalLogger "git-all" (setLevel DEBUG)

  -- Do a find in all directories (in sequence) in a separate thread, so
  -- that the list of directories is accumulated while we work on them
  c <- newChan
  forkIO $ findDirectories c ((".git" ==) . filename) ["."]

  -- While readChan keeps returning `Just x', call `checkGitDirectory opts
  -- x'.  Once it returns Nothing, findDirectories is done and we can stop
  whileJust_ (readChan c) $ checkGitDirectory opts

-- Primary logic

checkGitDirectory :: GitAll -> FilePath -> IO ()
checkGitDirectory opts dir = do
  debugM "git-all" $ "Scanning " ++ asText dir

  url <- gitMaybe dir "config" ["svn-remote.svn.url"]

  case L.head (arguments opts) of
    "fetch" ->
      gitFetch dir url

    "status" -> do
      branches <- gitLocalBranches dir
      mapM_ (gitPushOrPull dir url (pulls opts)) branches

      gitStatus dir (untracked opts)

-- Git command wrappers

dirAsFile = fromText . T.init . toTextIgnore . directory

gitStatus :: FilePath -> Bool -> IO ()
gitStatus dir untracked = do
  changes <-
    git dir "status"
        [ "--porcelain"
        , append "--untracked-files=" (if untracked then "normal" else "no") ]

  putStr $ unpack $ topTen "STATUS" (dirAsFile dir) changes "=="

gitFetch :: FilePath -> Maybe Text -> IO ()
gitFetch dir url = do
  output <-
    if isNothing url
    then
      git dir "fetch" ["-q", "--progress", "--all", "--prune", "--tags"]

    else do
      out <- git dir "svn" ["fetch"]
      -- jww (2012-08-06): Why doesn't =~ work on Data.Text.Lazy.Text?
      let pat = unpack "^(W: |This may take a while|Checked through|$)"
      return $ unlines $ L.filter (not . (=~ pat) . unpack) (lines out)

  putStr $ unpack $ topTen "FETCH" (dirAsFile dir) output "=="

type CommitId = Text
type BranchInfo = (CommitId, Text)

gitLocalBranches :: FilePath -> IO [BranchInfo]
gitLocalBranches dir = do
  output <- git dir "for-each-ref" ["refs/heads/"]
  return $ parseGitRefs (lines output)

  -- Each line is of the form: "<HASH> commit refs/heads/<NAME>"
  where parseGitRefs (x:xs) =
          (L.head words', drop 11 (words' !! 2)) : parseGitRefs xs
          where words' = T.words x
        parseGitRefs [] = []

gitPushOrPull :: FilePath -> Maybe Text -> Bool -> BranchInfo -> IO ()
gitPushOrPull dir url pulls branch = do
  remote <-
    gitMaybe dir "config" [T.concat ["branch.", snd branch, ".remote"]]

  when (isJust remote || isJust url) $ do
    let branchName = T.concat [fromJust remote, "/", snd branch]
    remoteSha <- if isJust remote
                 then
                   gitMaybe dir "rev-parse" [branchName]
                 else do
                   trunk <- gitMaybe dir "rev-parse" ["trunk"]
                   case trunk of
                     Nothing -> gitMaybe dir "rev-parse" ["git-svn"]
                     Just _  -> return trunk

    when (isJust remoteSha && fst branch /= fromJust remoteSha) $ do
      let logArgs     = ["--no-merges", "--oneline"]
          branchLabel = T.concat [toTextIgnore (dirAsFile dir), "#", snd branch]
          sha         = fromJust remoteSha

      pushLog <- git dir "log" (   logArgs
                                ++ [T.concat [sha, "..", fst branch], "--"])
      putStr $ unpack $ topTen "NEED PUSH" (fromText branchLabel) pushLog "=="

      when pulls $ do
        pullLog <- git dir "log" (   logArgs
                                  ++ [T.concat [fst branch, "..", sha], "--"])
        putStr $ unpack $ topTen "NEED PULL" (fromText branchLabel) pullLog "=="

-- Utility functions

doGit :: FilePath -> Text -> [Text] -> Sh Text
doGit dir com args = do
  let gitDir  = T.append "--git-dir=" (toTextIgnore dir)
  let workDir = T.append "--work-tree=" (toTextIgnore (dirAsFile dir))
  run "git" $ [gitDir, workDir, com] ++ args

git :: FilePath -> Text -> [Text] -> IO Text
git dir com args =
  shelly $ silently $ errExit False $ do
    text <- doGit dir com args
    code <- lastExitCode
    unless (code == 0) $ do
      err <- lastStderr
      liftIO $ putStr $ unpack $ topTen "FAILED" (dirAsFile dir) err "##"
    return text

gitMaybe :: FilePath -> Text -> [Text] -> IO (Maybe Text)
gitMaybe dir cmd args =
  shelly $ silently $ errExit False $ do
    text <- doGit dir cmd args
    code <- lastExitCode
    if code == 0
      then return . Just . L.head . lines $ text
      else return Nothing

topTen :: Text -> FilePath -> Text -> Text -> Text
topTen _ _ "" _ = ""
topTen category path content marker =
  T.concat $ [ "\n", marker, " ", category, " ", marker, " "
             , toTextIgnore path, "\n"
             -- , unlines (L.take 10 ls)
             , unlines (L.map (take 80) (L.take 10 ls)) ]
          ++ (let len = L.length (L.drop 10 ls) in
              case len of
                0 -> []
                _ -> ["... (and " , pack (show len) , " more)\n"])
  where ls = lines content

findDirectories :: Chan (Maybe FilePath) -> (FilePath -> Bool) -> [FilePath] -> IO ()
findDirectories c pred dirs = do
  -- This is a bit dense, so here's the breakdown:
  --
  -- 1. Given a list of `dirs', call test_d to find which are really there
  -- 2. Bind the function after >>= to this list, which maps over each
  --    directory, calling findFold on each one
  -- 3. The fold calls writeChan for each directory matching our predicate
  -- 4. When it's all done, write Nothing to the channel to let the caller
  --    know we're all done.

  shelly $ filterM test_d dirs >>=
    mapM_ (findFold (\_ path -> when (pred path) $
                                liftIO . writeChan c . Just $ path) ())
  writeChan c Nothing

asText = unpack . toTextIgnore

-- git-all.hs ends here
