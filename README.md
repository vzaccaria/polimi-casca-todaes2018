> CASCA: a Design Automation Approach for Designing Hardware Countermeasures Against Side Channel Attacks

## Prerequisites

| Tool  | Version  | Suggested install method                      |
| ----- | -------- | -----                                         |
| Z3    | > 4.7    | from [source](https://github.com/Z3Prover/z3) |
| Stack | > 1.3.2  | OS package manager                            |

## Primitives

-   `Oswald MS-IAIK`: A **masked** AES S-Box inversion, implemented in
    $GF(((2^2)^2)^2)$, based on Wolkerstorfer's composite field
    inversion. Papers:
    [1](https://www.iacr.org/archive/fse2005/35570401/35570401.pdf),
    [2](https://eprint.iacr.org/2004/134).

-   `Wolkerstorfer S-IAIK`: An **unmasked** AES S-Box inversion,
    implemented in a $GF((2^4)^2)$ composite field. Paper:
    [1](https://dl.acm.org/citation.cfm?id=680932).

## Content

-   `Backend`: Different *interpretations* of the same abstract
    polymorphic specification (MaskedSBOX, UmaskedSBOX).
    -   `CLaSH`: structural RTL description, using CλaSH, for generating
        both synthesizable RTL spec. and Testbench (in VHDL, Verilog,
        ...).
    -   `SBV`: symbolic description of the circuits, used by an SMT
        (Satisfiability Modulo Theories) solver to verify the code
        against a formal spec.
    -   `MaskProp`: symbolic description of a heuristics for checking
        the masking properties of the specs.
    -   `Trace`: high-level probing machinery for side-channel analysis.
-   `Language`: Syntax definition of our abstract polymorphic language.
-   `Primitives`: Abstract polymorphic specs of *UnmaskedSBOX*,
    *MaskedSBOX* and all the mathematical primitives needed by
    computations in Composite Galois Fields.

## How To (local)

1.  To use locally, first of all you will need a working [Haskell Tool
    Stack](https://docs.haskellstack.org/en/stable/README/).

2.  Install CλaSH, SBV and other useful libraries.

    ``` bash
    stack setup --resolver lts-9.10
    stack install clash-ghc --resolver lts-9.10
    stack install clash-prelude --resolver lts-9.10
    stack install tasty tasty-hunit tasty-quickcheck --resolver lts-9.10
    stack install sbv --resolver lts-9.10
    ```

3.  To perform a quick check on both masked and unmasked SBOX
    polymorphic primitives:

    ``` bash
    make prove-unmasked
    make prove-masked
    ```

4.  To functionally check against some formal symbolic rules, by using
    the SMT solver *Microsoft Ζ3* (via SBV):

    ``` bash
    make prove-unmasked
    make prove-masked
    ```

5.  To check the "naive masking" properties of the specs by using a
    custom symbolic heuristics:

    ``` bash
    make check-naive-masking
    ```

6.  To generate the RTL spec in Verilog, by using the CλaSH
    *interpretation*:

    ``` bash
    make syn-unmasked   # OR
    make syn-masked
    ```

    It'll take less than 10 minutes. You'll find the results in
    `./verilog`.

    ***Important note:*** at the moment there's a slight problem with
    compiling multiple CLaSH specs, as they will all result in files
    with the same prefix (*"Backend"*). For this reason, it's better to
    remove (or move somewhere else) the existing output directory
    **before** performing another CLaSH compile:

    ``` bash
    rm -Rf verilog/
    ```

7.  To generate high-level traces (probes are placed on 4-bit
    operations):

    ``` bash
    make generate-inputs
    make probe
    ```

    ***Note:*** Probing is done via UNSAFE IO functions, this should
    change in a future version.

## License

```
CASCA: a Design Automation Approach for Designing Hardware Countermeasures Against Side Channel Attacks (c) by Lorenzo Delledonne, Vittorio Zaccaria, Filippo Melzani, Guido Bertoni, Ruggero Susella

This work is licensed under a
Creative Commons Attribution-ShareAlike 4.0 International License.

You should have received a copy of the license along with this
work. If not, see <http://creativecommons.org/licenses/by-sa/4.0/>.
```
