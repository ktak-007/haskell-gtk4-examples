# Haskell gtk4 examples
The set of gtk4 Haskell examples by Stanislav Ktak Smirnov

# Structure
* Every directory is the separate and self-sufficient project to build.
* It is assumed you are using the Nix package manager: https://nixos.org/download/

# Build project

There are two variants. The first, in the project directory:
1. `cd <project_dir>`
2. `nix-shell`
3. `cabal build`
4. `cabal run`

The second, in the root directory:
1. `nix-shell`
2. `cabal build <project_name>`
3. `cabal run <project_name>`
The projects names you can see as a directories with projects or you can look at cabal.project file.
