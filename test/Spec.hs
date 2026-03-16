module Main where

import System.Exit

-- Minimal test suite. Unit tests for pure functions (topTen, parseGitRefs,
-- etc.) can be added once they're extracted to a library module.

main :: IO ()
main = do
  putStrLn "git-all test suite"
  putStrLn "  (no unit tests yet -- single-module executable)"
  exitSuccess
