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

```bash
$ .cabal-sandbox/bin/desugar-template DeIf deIf 0 0 > src/DeIf.hs
# generate desugar template source (後面兩個數字分別是 mode (0:normal, 1:annotated) 與想要帶的額外參數數量)
```
