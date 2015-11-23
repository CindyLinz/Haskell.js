# Build

```bash
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
$ cabal install
```

# Run

```bash
$ .cabal-sandbox/bin/show < sample/hello_world.hs
# take a look on the syntax tree
```

```bash
$ .cabal-sandbox/bin/trans < sample/hello_world.hs | node
# run the sample
```
