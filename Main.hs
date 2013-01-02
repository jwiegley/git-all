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

import           Control.Concurrent
import           Control.Concurrent.ParallelIO
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.List as L
import           Data.Maybe
import           Data.Text.Lazy as T
import           Filesystem (listDirectory)
import           Filesystem.Path (directory, filename)
import           GHC.Conc
import           Prelude hiding (FilePath, catch)
import           Shelly
import           System.Console.CmdArgs
import           System.Environment (getArgs, withArgs)
import           System.Log.Logger
import           System.Posix.Files
import           Text.Regex.Posix

default (T.Text)

version :: String
version       = "1.3.0"

copyright :: String
copyright     = "2012"

gitAllSummary :: String
gitAllSummary = "git-all v" ++ version ++ ", (C) John Wiegley " ++ copyright

data GitAll = GitAll { jobs      :: Int
                     , pulls     :: Bool
                     , untracked :: Bool
                     , verbose   :: Bool
                     , debug     :: Bool
                     , arguments :: [String] }
    deriving (Data, Typeable, Show, Eq)

gitAll :: GitAll
gitAll = GitAll
    { jobs      = def &= name "j" &= typ "INT"
                      &= help "Run INT concurrent finds at once (default: 2)"
    , pulls     = def &= name "p"
                      &= help "Include NEED PULL sections"
    , untracked = def &= name "U"
                      &= help "Display untracked files as possible changes"
    , verbose   = def &= name "v"
                      &= help "Report progress verbosely"
    , debug     = def &= name "D"
                      &= help "Report debug information"
    , arguments = def &= args &= typ "fetch | status" &= opt (T.unpack "status") } &=
    summary gitAllSummary &=
    program "git-all" &=
    help "Fetch or get status of all Git repos under the given directories"

main :: IO ()
main = do
  -- process command-line options
  opts <- cmdArgs gitAll
  _ <- GHC.Conc.setNumCapabilities $ case jobs opts of 0 -> 4; x -> x

  when (verbose opts) $ updateGlobalLogger "git-all" (setLevel INFO)
  when (debug opts)   $ updateGlobalLogger "git-all" (setLevel DEBUG)

  -- Do a find in all directories (in sequence) in a separate operating system
  -- thread, so that the list of directories is accumulated while we work on
  -- them
  c    <- newChan
  _    <- forkOS $ do
    parallel_ $ L.map (findDirs c 0 ((".git" ==) . filename))
                      (let xs = L.tail (arguments opts)
                       in if L.null xs
                          then ["."]
                          else L.map (fromText . pack) xs)
    writeChan c Nothing
  dirs <- getChanContents c

  -- While readChan keeps returning `Just x', call `checkGitDirectory opts
  -- x'.  Once it returns Nothing, findDirs is done and we can stop
  parallel_ $ L.map (runGitCmd opts >=> putStr . unpack)
                    (catMaybes' dirs)
  stopGlobalPool

  where runGitCmd :: GitAll -> FilePath -> IO Text
        runGitCmd opts = flip execStateT "" . checkGitDirectory opts

        catMaybes' :: [Maybe a] -> [a]
        catMaybes' [] = []
        catMaybes' (x:xs) = case x of
          Nothing -> []
          Just y  -> y : catMaybes' xs

-- Primary logic

type IOState = StateT Text IO

putStrM :: Text -> IOState ()
putStrM = modify . T.append

toString :: FilePath -> String
toString = unpack . toTextIgnore

checkGitDirectory :: GitAll -> FilePath -> IOState ()
checkGitDirectory opts dir = do
  liftIO $ debugM "git-all" $ "Scanning " ++ toString dir

  url <- gitMaybe dir "config" ["svn-remote.svn.url"]

  case L.head (arguments opts) of
    "fetch"  ->
      gitFetch dir url

    "status" -> do
      mapM_ (gitPushOrPull dir url (pulls opts)) =<< gitLocalBranches dir
      gitStatus dir (untracked opts)

    unknown  -> putStrM $ T.concat [ "Unknown command: ", T.pack unknown, "\n" ]

-- Git command wrappers

dirAsFile :: FilePath -> FilePath
dirAsFile = fromText . T.init . toTextIgnore . directory

gitStatus :: FilePath -> Bool -> IOState ()
gitStatus dir showUntracked = do
  changes <-
    git dir "status" [ "--porcelain"
                     , append "--untracked-files="
                              (if showUntracked then "normal" else "no") ]

  putStrM $ topTen "STATUS" (dirAsFile dir) changes "=="

gitFetch :: FilePath -> Maybe Text -> IOState ()
gitFetch dir url = do
  output <-
    if isNothing url
    then git dir "fetch" ["-q", "--progress", "--all", "--prune", "--tags"]
    else do
      out <- git dir "svn" ["fetch"]
      -- jww (2012-08-06): Why doesn't =~ work on Data.Text.Lazy.Text?
      let pat = unpack "^(W: |This may take a while|Checked through|$)"
      return $ T.unlines $ L.filter (not . (=~ pat) . unpack) (T.lines out)

  putStrM $ topTen "FETCH" (dirAsFile dir) output "=="

type CommitId = Text
type BranchInfo = (CommitId, Text)

gitLocalBranches :: FilePath -> IOState [BranchInfo]
gitLocalBranches dir = do
  output <- git dir "for-each-ref" ["refs/heads/"]
  return $ parseGitRefs (T.lines output)

  -- Each line is of the form: "<HASH> commit refs/heads/<NAME>"
  where parseGitRefs []     = []
        parseGitRefs (x:xs) =
          (L.head words', T.drop 11 (words' !! 2)) : parseGitRefs xs
          where words' = T.words x

gitPushOrPull :: FilePath -> Maybe Text -> Bool -> BranchInfo -> IOState ()
gitPushOrPull dir url doPulls branch = do
  remote <- gitMaybe dir "config" [T.concat ["branch.", snd branch, ".remote"]]
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

      pushLog <- git dir "log" (  logArgs
                               ++ [T.concat [sha, "..", fst branch], "--"])
      putStrM $ topTen "NEED PUSH" (fromText branchLabel) pushLog "=="

      when doPulls $ do
        pullLog <- git dir "log" (  logArgs
                                 ++ [T.concat [fst branch, "..", sha], "--"])
        putStrM $ topTen "NEED PULL" (fromText branchLabel) pullLog "=="

-- Utility functions

doGit :: FilePath -> Text -> [Text] -> Sh Text
doGit dir com gitArgs = do
  let gitDir  = T.append "--git-dir=" (toTextIgnore dir)
  let workDir = T.append "--work-tree=" (toTextIgnore (dirAsFile dir))
  run "git" $ [gitDir, workDir, com] ++ gitArgs

git :: FilePath -> Text -> [Text] -> IOState Text
git dir com gitArgs = do
  result <-
    shelly $ silently $ errExit False $ do
      text <- doGit dir com gitArgs
      code <- lastExitCode
      if code == 0
        then return $ Right text
        else return . Left =<< lastStderr
  case result of
    Left err   -> do putStrM $ topTen "FAILED" (dirAsFile dir) err "##"
                     return ""
    Right text -> return text

gitMaybe :: FilePath -> Text -> [Text] -> IOState (Maybe Text)
gitMaybe dir gitCmd gitArgs =
  shelly $ silently $ errExit False $ do
    text <- doGit dir gitCmd gitArgs
    code <- lastExitCode
    if code == 0
      then return . Just . L.head . T.lines $ text
      else return Nothing

topTen :: Text -> FilePath -> Text -> Text -> Text
topTen _ _ "" _ = ""
topTen category pathname content marker =
  T.concat $ [ "\n", marker, " ", category, " ", marker, " "
             , toTextIgnore pathname, "\n"
             -- , unlines (L.take 10 ls)
             , T.unlines (L.map (T.take 80) (L.take 10 ls')) ]
          ++ (let len = L.length (L.drop 10 ls') in
              case len of
                0 -> []
                _ -> ["... (and " , pack (show len) , " more)\n"])
  where ls' = T.lines content

findDirsW :: Chan (Maybe FilePath) -> Int -> (FilePath -> Bool) -> FilePath
          -> IO ()
findDirsW c curDepth findPred p =
  catch (findDirs c curDepth findPred p)
        (\e -> (e :: IOException) `seq` return ())

findDirs :: Chan (Maybe FilePath) -> Int -> (FilePath -> Bool) -> FilePath
         -> IO ()
findDirs c curDepth findPred p = do
  status <- if curDepth == 0
            then getFileStatus (toString p)
            else getSymbolicLinkStatus (toString p)

  when (isDirectory status &&
       filename p `notElem` ["CVS", ".svn", ".deps", "_darcs", "CMakeFiles"]) $
    if findPred p
      then writeChan c (Just p)
      else do
        files <- listDirectory p
        let func = findDirsW c (curDepth + 1) findPred
        if curDepth == 0
          then parallel_ $ L.map func files
          else mapM_ func files

-- git-all.hs ends here
