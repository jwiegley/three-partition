# Solving the 3-Partition Problem with Z3

Recently a friend of mine was tasked with a partitioning problem: given a
bag of integers, split them into groups of three such that every group has the
same sum. It's known as the
[3-partition problem](https://en.wikipedia.org/wiki/3-partition_problem), and
it's NP-hard in the strong sense -- meaning there's no known trick to make it
fast as the input grows.

I've always found these kinds of problems to be a great fit for SMT solvers,
so I wrote a small Haskell tool that hands the whole thing off to Z3 via
[SBV](https://hackage.haskell.org/package/sbv). The solver assigns each
integer to one of _n/3_ groups, constrains every group to have exactly 3
elements, computes symbolic sums, and then uses SBV's `optimize
Lexicographic` to minimize the maximum group sum. If a valid partition exists,
Z3 will find it.

One slightly tricky bit: SBV's `SatModel` instance needs to know how many
variables to parse out of the solver's result, but that depends on the input
size which isn't known at compile time. I used `Data.Reflection` to inject the
length at the type level, which lets the `parseCVs` implementation pull out
exactly the right number of assignments and sums.

## Building

The project uses [haskell.nix](https://input-output-hk.github.io/haskell.nix/)
with GHC 9.10. Enter the dev shell and build with cabal:

```bash
nix develop
cabal build
```

You'll need Z3 available at runtime -- the Nix shell provides it automatically.

## Running

```bash
cabal run three-partition
```

The default input is hardcoded in `app/Main.hs`. Edit the list there or wire
up your own input. The output shows the partition groups and their sums:

```
[[88,87,0],[82,63,39],[95,88,10],[100,61,12]]
[175,184,193,173]
```

## Development

Formatting uses [fourmolu](https://github.com/fourmolu/fourmolu), linting
uses [hlint](https://github.com/ndmitchell/hlint), and there's a
[lefthook](https://github.com/evilmartians/lefthook) config that runs both as
pre-commit hooks along with `nix flake check` and `cabal test`:

```bash
lefthook install        # set up git hooks
fourmolu -i src/ app/   # format in place
hlint src/ app/         # lint
cabal test              # run the test suite
```

The `nix flake check` target runs the build, format check, and lint check all
at once -- it's the same thing CI would run.

## License

BSD-3-Clause. See [LICENSE.md](LICENSE.md).
