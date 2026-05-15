default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ ARGS }}

# run nix build
build:
    nix build .#oke --log-format internal-json -v |& nom --json
