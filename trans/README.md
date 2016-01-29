# Build

```bash
$ stack build
```

# Run

```bash
$ stack exec show < sample/hello_world.hs
# take a look on the syntax tree
```

```bash
$ stack exec trans < sample/hello_world.hs | node
# run the sample
```

```bash
$ stack exec trans -- -d < sample/hello_world.hs
# or
# stack exec trans -- --desugar < sample/hello_world.hs
# see the desugarred basic haskell source
```

```bash
$ stack exec desugar-template DeIf deIf 0 0 > src/DeIf.hs
# generate desugar template source (後面兩個數字分別是 mode (0:normal, 1:annotated) 與想要帶的額外參數數量)
```
