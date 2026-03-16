# git-all

I manage a lot of Git repositories -- a few hundred at last count, spread
across several directory trees. Checking each one by hand to see what's dirty,
what needs pushing, or what's fallen behind got old fast.

`git-all` scans a directory tree, finds every Git repo (including submodules
and worktrees), and runs a Git command in each one -- reporting only the repos
that need attention. Clean repos stay silent, so you only see what matters.

## Installation

### Nix

```bash
nix run github:jwiegley/git-all -- status ~/projects
```

### Cabal

```bash
cabal install
```

Make sure `~/.cabal/bin` is on your PATH.

## Usage

```bash
git-all                        # Status of repos under .
git-all status ~/projects      # Status under given directories
git-all -j8 status .           # 8 parallel workers
git-all -p status .            # Show "NEED PULL" sections
git-all -U status .            # Include untracked files
git-all fetch ~/projects       # Fetch all repos
git-all log ~/projects         # Run any git command
```

The output uses `==` markers for normal results and `##` for errors. Only the
first ten lines per repo are shown, with a count of any remaining.

## Building from source

```bash
nix develop                    # Enter dev shell with all tools
cabal build                    # Build
cabal test                     # Run tests
nix flake check                # Run all checks (build, lint, format)
```

### Build targets

```bash
nix run .#format -- Main.hs    # Format code with fourmolu
nix run .#lint -- Main.hs      # Lint with hlint
```

## How it works

A producer thread walks the directory tree, writing `(gitDir, workTree)` pairs
to a channel as it discovers `.git` directories. The main thread consumes the
channel lazily and runs the requested Git command in each repository. Top-level
directories are scanned in parallel (controlled by `-j`); subdirectories are
scanned sequentially to avoid thread explosion.

## License

BSD-3-Clause -- see [LICENSE.md](LICENSE.md).
