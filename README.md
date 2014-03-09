# bpl

A compiler for CS331.

## Running BPL

The `Main` module contains the code for running the compiler (at the
latest stage of development). To run it, use:

```
$ runhaskell Main.hs <filename>
```

BPL can also be compiled to native code by GHC.

If `<filename>` is not given, `Main` will default to reading from
`"parser_test.bpl"`.
