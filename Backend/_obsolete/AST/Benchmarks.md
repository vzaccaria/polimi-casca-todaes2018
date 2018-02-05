
# Benchmarking the AST

Output of: `runhaskell Backend/AST/EvalAstReprBench.hs`

## commit be521ad
```
benchmarking evalA4 inv4/1
time                 144.2 μs   (141.2 μs .. 148.0 μs)
                     0.996 R²   (0.990 R² .. 0.999 R²)
mean                 145.4 μs   (143.0 μs .. 151.6 μs)
std dev              12.69 μs   (6.034 μs .. 25.48 μs)
variance introduced by outliers: 76% (severely inflated)

benchmarking evalA4 inv4/4
time                 3.802 s    (3.663 s .. 3.888 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.813 s    (3.787 s .. 3.824 s)
std dev              22.21 ms   (0.0 s .. 23.90 ms)
variance introduced by outliers: 19% (moderately inflated)
```

## commit 310e42e
```
benchmarking evalA4 inv4/1
time                 142.4 μs   (140.8 μs .. 144.0 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 142.3 μs   (141.0 μs .. 143.7 μs)
std dev              4.541 μs   (3.721 μs .. 5.790 μs)
variance introduced by outliers: 30% (moderately inflated)

benchmarking evalA4 inv4/4
time                 3.796 s    (3.647 s .. 4.044 s)
                     1.000 R²   (NaN R² .. 1.000 R²)
mean                 3.746 s    (3.708 s .. 3.774 s)
std dev              42.54 ms   (0.0 s .. 48.52 ms)
variance introduced by outliers: 19% (moderately inflated)
```

