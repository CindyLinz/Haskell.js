# Build

```bash
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
```

# Run

```bash
$ dist/build/show/show < sample/hello_world.hs
# take a look on the syntax tree
```

```bash
$ dist/build/trans/trans < sample/hello_world.hs | node
# run the sample
```

```bash
$ dist/build/desugar-template/desugar-template DeIf deIf 0 0 > src/DeIf.hs
# generate desugar template source (後面兩個數字分別是 mode (0:normal, 1:annotated) 與想要帶的額外參數數量)
```
