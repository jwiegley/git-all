{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

-- git-all.hs
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
#if MIN_VERSION_shelly(1, 0, 0)
import           Data.Text as T
#else
import           Data.Text.Lazy as T
#endif
import           GHC.Conc
import           Shelly hiding (cmd)
import           System.Console.CmdArgs
import           System.Directory
import           System.FilePath.Posix hiding ((</>))
import           System.Log.Logger
import           System.Posix.Files
import           Text.Regex.Posix

default (T.Text)

version :: String
version = "1.5.0"

copyright :: String
copyright = "2012-2013"

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
                      &= help "Run INT concurrent finds at once (default: 4)"
    , pulls     = def &= name "p"
                      &= help "Include NEED PULL sections"
    , untracked = def &= name "U"
                      &= help "Display untracked files as possible changes"
    , verbose   = def &= name "v"
                      &= help "Report progress verbosely"
    , debug     = def &= name "D"
                      &= help "Report debug information"
    , arguments = def &= args &= typ "fetch | status"
                      &= opt (T.unpack "status") }
    &= summary gitAllSummary
    &= program "git-all"
    &= help "Fetch or get status of all Git repos under the given directories"

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
  c    <- newChan :: IO (Chan (Maybe (FilePath, FilePath)))
  _    <- forkOS $ do
    parallel_ $ L.map (findDirsContaining c 0)
                      (let xs = L.tail (arguments opts)
                       in if L.null xs then ["."] else xs)
    writeChan c Nothing
  dirs <- getChanContents c

  -- While readChan keeps returning `Just x', call `checkGitDirectory opts
  -- x'.  Once it returns Nothing, findDirs is done and we can stop
  mapM_ (runGitCmd opts >=> putStr . unpack)
        (catMaybes' dirs)
  stopGlobalPool

  where
    runGitCmd :: GitAll -> (FilePath, FilePath) -> IO Text
    runGitCmd opts entry = do
      debugM "git-all" $ "Running command for " ++ show entry
      execStateT (uncurry (checkGitDirectory opts) entry) ""

    catMaybes' :: [Maybe a] -> [a]
    catMaybes' [] = []
    catMaybes' (x:xs) = case x of
      Nothing -> []
      Just y  -> y : catMaybes' xs

-- Primary logic

type IOState = StateT Text IO

putStrM :: Text -> IOState ()
putStrM = modify . T.append

checkGitDirectory :: GitAll -> FilePath -> FilePath -> IOState ()
checkGitDirectory opts gitDir workTree = do
  liftIO $ debugM "git-all" $
      "Scanning " ++ gitDir ++ " => " ++ workTree

  url <- gitMaybe gitDir workTree "config" ["svn-remote.svn.url"]

  case L.head (arguments opts) of
    "fetch"  ->
      gitFetch gitDir workTree url

    "status" -> do
      mapM_ (gitPushOrPull gitDir workTree url (pulls opts))
          =<< gitLocalBranches gitDir workTree
      gitStatus gitDir workTree (untracked opts)

    cmd -> gitCommand gitDir workTree (T.pack cmd)

-- Git command wrappers

gitStatus :: FilePath -> FilePath -> Bool -> IOState ()
gitStatus gitDir workTree showUntracked = do
  changes <-
    git gitDir workTree "status"
        [ "--porcelain"
        , append "--untracked-files="
            (if showUntracked then "normal" else "no") ]

  putStrM $ topTen "STATUS" workTree changes "=="

gitFetch :: FilePath -> FilePath -> Maybe Text -> IOState ()
gitFetch gitDir workTree url = do
  output <-
    if isNothing url
    then git gitDir workTree "fetch"
             ["-q", "--progress", "--all", "--prune", "--tags"]
    else do
      out <- git gitDir workTree "svn" ["fetch"]
      -- jww (2012-08-06): Why doesn't =~ work on Data.Text.Lazy.Text?
      let pat = unpack "^(W: |This may take a while|Checked through|$)"
      return $ T.unlines $ L.filter (not . (=~ pat) . unpack) (T.lines out)

  putStrM $ topTen "FETCH" workTree output "=="

gitCommand :: FilePath -> FilePath -> Text -> IOState ()
gitCommand gitDir workTree cmd = do
  output <- git gitDir workTree cmd []
  putStrM $ topTen ("CMD[" <> cmd <> "]") workTree output "=="

type CommitId = Text
type BranchInfo = (CommitId, Text)

gitLocalBranches :: FilePath -> FilePath -> IOState [BranchInfo]
gitLocalBranches gitDir workTree = do
  output <- git gitDir workTree "for-each-ref" ["refs/heads/"]
  return $ parseGitRefs (T.lines output)

  -- Each line is of the form: "<HASH> commit refs/heads/<NAME>"
  where parseGitRefs []     = []
        parseGitRefs (x:xs) =
          (L.head words', T.drop 11 (words' !! 2)) : parseGitRefs xs
          where words' = T.words x

gitPushOrPull :: FilePath -> FilePath -> Maybe Text -> Bool -> BranchInfo -> IOState ()
gitPushOrPull gitDir workTree url doPulls branch = do
  remote <- gitMaybe gitDir workTree "config"
      [T.concat ["branch.", snd branch, ".remote"]]
  when (isJust remote || isJust url) $ do
    let branchName = T.concat [fromJust remote, "/", snd branch]
    remoteSha <- if isJust remote
                 then
                   gitMaybe gitDir workTree "rev-parse" [branchName]
                 else do
                   trunk <- gitMaybe gitDir workTree "rev-parse" ["trunk"]
                   case trunk of
                     Nothing -> gitMaybe gitDir workTree "rev-parse" ["git-svn"]
                     Just _  -> return trunk

    when (isJust remoteSha && fst branch /= fromJust remoteSha) $ do
      let logArgs     = ["--no-merges", "--oneline"]
          branchLabel = T.concat [toTextIgnore gitDir, "#", snd branch]
          sha         = fromJust remoteSha

      pushLog <- git gitDir workTree "log"
          (logArgs ++ [T.concat [sha, "..", fst branch], "--"])
      putStrM $ topTen "NEED PUSH" (fromText branchLabel) pushLog "=="

      when doPulls $ do
        pullLog <- git gitDir workTree "log"
            (logArgs ++ [T.concat [fst branch, "..", sha], "--"])
        putStrM $ topTen "NEED PULL" (fromText branchLabel) pullLog "=="

-- Utility functions

doGit :: FilePath -> FilePath -> Text -> [Text] -> Sh Text
doGit gitDir workTree com gitArgs = do
    liftIO $ debugM "git-all" $ "git " ++ show args_
    run "git" args_
  where
    args_ = "--git-dir":pack gitDir:"--work-tree":pack workTree:com:gitArgs

git :: FilePath -> FilePath -> Text -> [Text] -> IOState Text
git gitDir workTree com gitArgs = do
  result <-
    shelly $ silently $ errExit False $ do
      text <- doGit gitDir workTree com gitArgs
      code <- lastExitCode
      if code == 0
        then return $ Right text
        else Left <$> lastStderr
  case result of
    Left err   -> do
        putStrM $ topTen "FAILED" (workTree <> " <= " <> gitDir) err "##"
        return ""
    Right text -> return text

gitMaybe :: FilePath -> FilePath -> Text -> [Text] -> IOState (Maybe Text)
gitMaybe gitDir workTree gitCmd gitArgs =
  shelly $ silently $ errExit False $ do
    text <- doGit gitDir workTree gitCmd gitArgs
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

findDirsContainingW
    :: Chan (Maybe (FilePath, FilePath))
    -> Int
    -> FilePath
    -> IO ()
findDirsContainingW c curDepth p =
  catch (findDirsContaining c curDepth p)
        (\e -> (e :: IOException) `seq` return ())

findDirsContaining
    :: Chan (Maybe (FilePath, FilePath))
    -> Int
    -> FilePath
    -> IO ()
findDirsContaining c curDepth p = do
  status <- if curDepth == 0
            then getFileStatus p
            else getSymbolicLinkStatus p

  debugM "git-all" $ "p = " ++ show p
  debugM "git-all" $ "takeFileName p = " ++ show (takeFileName p)
  when (takeFileName p == ".git") $ do
    debugM "git-all" "is .git"
    when (isRegularFile status) $ do
      debugM "git-all" ".git is regular file"
      t <- readFile p
      when ("gitdir: " `L.isPrefixOf` t) $
        writeChan c (Just (takeDirectory p </> fromText (T.strip (T.pack (L.drop 8 t))),
                           takeDirectory p))
    when (isDirectory status) $ do
      debugM "git-all" ".git is directory"
      writeChan c (Just (p, takeDirectory p))

  when (isDirectory status &&
        takeFileName p `notElem` ["CVS", ".svn", ".deps", "_darcs", "CMakeFiles"]) $ do
    debugM "git-all" $ p ++ " is not a special directory, recursing"
    files <- listDirectory p
    let func = findDirsContainingW c (curDepth + 1)
    if curDepth == 0
      then parallel_ $ L.map func files
      else mapM_ func files

-- git-all.hs ends here
