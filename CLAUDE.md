# git-all Haskell Project Guide

This guide provides essential context for working with the git-all codebase - a concurrent Git repository scanner written in Haskell.

## Build Commands

### Nix (Preferred for Development)
```bash
# Modern flake-based development (GHC 9.10)
nix develop                    # Enter dev shell with HLS
nix build                      # Build the executable
nix run                        # Run directly
nix run -- -j8 status ~/repos  # Run with arguments

# Legacy Nix (GHC 8.8.2)
nix-build                      # Build
./result/bin/git-all           # Run from result
```

### Cabal
```bash
cabal v2-build                 # Build
cabal v2-run git-all -- -j8 .  # Run with arguments
cabal v2-install               # Install to ~/.cabal/bin

# Development with ghcid (if installed)
ghcid --command="cabal repl"
```

### Common Usage
```bash
git-all                        # Status of current directory
git-all -j8 status .           # 8 parallel workers
git-all -p status ~/projects   # Include pull requirements
git-all -U status .            # Show untracked files
git-all fetch ~/projects       # Fetch all repos
```

## Architecture Overview

### Single-Module Monolith
All code resides in `Main.hs` (~300 lines). While unconventional, this works for a focused utility.

### Concurrent Producer-Consumer Pattern
```haskell
-- Key insight: Uses channels for thread communication
c <- newChan :: IO (Chan (Maybe (FilePath, FilePath)))
_ <- forkOS $ do
  parallel_ $ L.map (findDirsContaining c 0) directories
  writeChan c Nothing  -- Sentinel for completion
dirs <- getChanContents c  -- Lazy consumption
```

**Concurrency Strategy:**
- OS thread (`forkOS`) for directory discovery - can block on I/O
- `parallel_` (from parallel-io) for top-level directory scanning
- Sequential processing below depth 0 to avoid thread explosion
- Controlled by `-j` flag (default: 4 capabilities)

### State Management Pattern
```haskell
type IOState = StateT Text IO
```
Uses `StateT` instead of `WriterT` to avoid space leaks. Each repository accumulates its output independently via explicit `T.append`.

### Fail-Soft Error Philosophy
```haskell
git :: FilePath -> FilePath -> Text -> [Text] -> IOState Text
-- Returns empty text on failure, continues scanning
case result of
  Left err -> do
    putStrM $ topTen "FAILED" ... err "##"
    return ""  -- Continue processing other repos
```
Git command failures don't abort the scan - errors are reported inline with "##" markers.

## Key Patterns & Conventions

### Shelly Version Compatibility
```haskell
#if MIN_VERSION_shelly(1, 0, 0)
import Data.Text as T          -- Strict Text
#else
import Data.Text.Lazy as T      -- Lazy Text
#endif
```
Handles both shelly 0.x and 1.x+ at compile time via CPP.

### Git Detection Logic
- Regular `.git` directory → standard repository
- `.git` file with `gitdir:` prefix → submodule or worktree
- Returns tuple: `(gitDir, workTree)`

### Directory Traversal Rules
- Skips: CVS, .svn, _darcs, .deps, CMakeFiles
- Uses `getSymbolicLinkStatus` at depth > 0 for symlink safety
- Only parallelizes top-level directories

### Output Truncation
The `topTen` function shows first 10 lines (80 chars each) plus count of remaining lines to prevent output flooding.

## Project Structure
```
git-all.cabal         # Version 1.8.0, executable only, no tests
Main.hs               # Version constant: "1.5.0" (discrepancy)
flake.nix             # GHC 9.10, haskell.nix, includes HLS
```

## Notable Design Decisions

### Why StateT over WriterT?
Avoids lazy accumulation space leaks. StateT with explicit `T.append` provides better memory control.

### Parallelism Control
```haskell
GHC.Conc.setNumCapabilities $ case jobs opts of 0 -> 4; x -> x
```
Sets runtime capabilities for optimal parallelism.

### Git-SVN Support
Explicitly handles git-svn repositories with special fetch logic and output filtering.

## Development Notes

### Missing Test Suite
No formal tests exist. For concurrent code, consider:
- QuickCheck for property testing
- STM for safer shared state (currently uses channels)
- async library patterns for better exception handling

### Profiling Support (Commented Out)
```nix
# In flake.nix, lines 36-39
# modules = [{
#   enableLibraryProfiling = true;
#   enableProfiling = true;
# }];
```

### Thread Safety Considerations
- Channel communication is thread-safe
- No shared mutable state between repositories
- Each repo processed independently
- Potential issue: No rate limiting on git process spawning

## Common Modifications

### Adding New Git Commands
1. Add command-line flag in `GitAll` data type (line ~50)
2. Add command logic similar to `gitStatus`/`gitFetch` pattern
3. Use `git` wrapper for error handling consistency

### Changing Parallelism Behavior
- Modify `findDirsContaining` recursion logic (line ~298)
- Adjust `setNumCapabilities` default (line 80)
- Consider rate limiting for git command spawning

### Output Format Changes
- Modify `topTen` function for truncation logic
- Adjust markers: "==" for success, "##" for errors
- Change `IOState` accumulation in `putStrM`
