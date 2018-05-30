This repository contains the research artifacts associated with the
manuscript:

> CASCA: a Design Automation Approach for Designing Hardware
> Countermeasures Against Side Channel Attacks Lorenzo Delledonne,
> Vittorio Zaccaria, Ruggero Susella, Guido Bertoni, Filippo Melzani
> submitted to ACM TODAES in 2018

This is made made publically available so that

-   any interested party may audit it
-   replication experiments can be performed
-   robustness of the original results can be verified, and
-   others can build directly upon the previous work through reuse and
    repurposing.

This work is licensed under a Creative Commons Attribution-ShareAlike
4.0 International License.

You should have received a copy of the license along with this work. If
not, see <http://creativecommons.org/licenses/by-sa/4.0/> or the
`LICENSE.md` file in this repo.

## Prerequisites

| Tool  | Version    | Suggested install method                      |
|-------|------------|-----------------------------------------------|
| Z3    | &gt; 4.7   | from [source](https://github.com/Z3Prover/z3) |
| Stack | &gt; 1.3.2 | OS package manager                            |

## Contents

This repository contains:

-   the definition of the CASCA abstract syntax
    (`Language/Operators.hs`)

-   the following semantic interpreters for the abstract syntax:
    -   `Backend/CLaSH`: structural RTL description, using CλaSH, for
        generating both synthesizable RTL spec. and Testbench (in VHDL
        or Verilog)
    -   `Backend/SBV`: symbolic description of the circuits, used by an
        SMT (Satisfiability Modulo Theories) solver to verify the code
        against a formal spec.
    -   `Backend/MaskProp`: symbolic description of a heuristics for
        checking the masking properties of the specs.
-   the specification of the following digital circuits done with the
    CASCA DSL:

    -   `Primitives/MaskedSBOX.hs`: a **masked** AES S-Box inversion,
        implemented in $GF(((2^2)^2)^2)$, based on Wolkerstorfer's
        composite field inversion (see papers:
        [\[1\]](https://www.iacr.org/archive/fse2005/35570401/35570401.pdf),
        [\[2\]](https://eprint.iacr.org/2004/134)).
    -   `Primitives/MaskedSBOX.hs`: An **unmasked** AES S-Box inversion,
        implemented in a $GF((2^4)^2)$ composite field (see paper
        [\[3\]](https://dl.acm.org/citation.cfm?id=680932)).
    -   `Primitives/Papers/Keccak.hs`: The original three shares Keccak
        Chi implemention (see, for example,
        [\[4\]](https://link.springer.com/chapter/10.1007/978-3-319-08302-5_13), p. 190)

-   A supplemental file with a short tutorial on tagless final
    programming in Haskell.

## Setting up

1.  Install prerequisite tools (`stack` and `z3`)

2.  Install CλaSH, SBV and other useful libraries.

    ``` bash
    stack setup
    stack install
    ```

## Playing with this repo

### Checking correctness of CASCA circuits

To functionally check the primitives against correctness rules (see for
example `Backend/SBV/Eval/EvalMasked.hs`) you can use the following
targets:

``` bash
make prove-unmasked # OR
make prove-masked   # OR
make prove-keccak
```

### Checking masking correctness of CASCA circuits

To check the "naive masking" properties of the specs by using a custom
symbolic heuristics (see `Backend/MaskProp/Eval/EvalMasked.hs`):

``` bash
make check-naive-masking
```

**Note**: some of the tests will "correctly" fail as the tested
specifications are not secure!

### Generating hardware specifications

To generate the RTL spec in Verilog through CLaSH

``` bash
make syn-unmasked   # OR
make syn-masked
make syn-keccak
```

***Important note:*** at the moment there's a slight problem with
compiling multiple CLaSH specs, as they will all result in files with
the same prefix (*"Backend"*). For this reason, it's better to remove
(or move somewhere else) the existing output directory **before**
performing another CLaSH compile:

``` bash
rm -Rf verilog/
```

## License

CASCA: a Design Automation Approach for Designing Hardware
Countermeasures Against Side Channel Attacks (c) by Lorenzo Delledonne,
Vittorio Zaccaria, Filippo Melzani, Guido Bertoni, Ruggero Susella

This work is licensed under a Creative Commons Attribution-ShareAlike
4.0 International License.

You should have received a copy of the license along with this work. If
not, see <http://creativecommons.org/licenses/by-sa/4.0/>.
