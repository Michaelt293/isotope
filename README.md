[![Build Status](https://travis-ci.org/Michaelt293/isotope.svg?branch=master)](https://travis-ci.org/Michaelt293/isotope)

![alt tag](https://github.com/Michaelt293/isotope/blob/master/isotope_jpeg.jpg)

WARNING: Isotope is currently undergoing major changes in preparation for Hackage. Consequently, this code repository may be broken or be out of line with it's documentation.

* [Isotopic, integer, monoisotopic, nominal and average masses](#isotopic-integer-monoisotopic-nominal-and-average-masses)
* [Comparison to other chemistry libraries](#comparison-to-other-chemistry-libraries)
    * [Radium](#radium)
    * [Ouch](#ouch)
* [Future directions](#future-directions)
* [Contributions](#contributions)
* [Authors](#authors)
* [License](#license)
* [References](#references)

Isotope is a chemistry library for calculating masses of elements and molecules. The main focus of the Isotope library is mass spectrometry, an area where the masses and relative abundances of isotopes and isotopologues is of central importance.

## Isotopic, integer, monoisotopic, nominal and average masses

In mass spectrometry and general chemistry, there are several different ways in which to describe mass. This can lead to some confusion since more than one term can exist to describe the same unit of measurement. In the Isotope library, the following conventions are used:

Mass | Description
--- | ---
Isotopic mass | The mass of an isotope for an element.
Integer mass | The mass of an isotope rounded to the nearest integer value.
Monoisotopic mass | The mass of the most abundant isotope for an element or the sum of the masses of the most abundant isotope of each element for a molecule.
Nominal mass | The integer mass of the most abundant isotope for an element or the sum of integer mass of the most abundant isotope of each element for a molecule.
Average mass | The average mass of an element or molecule based on naturally-occurring abundances. In the isotopes library, average mass is used in place of atomic mass and molecular mass.

For more detailed discussion regarding the concept of mass in mass spectrometry, please refer to "Molecular Weight and the Nominal Mass, Monoisotopic Mass and Average Molar Mass" by Prof. O. David Sparkman [1].

## Chemical and molecular formulae

In the Isotope library, a distinction between chemical and molecular formulae is made.

When using GHCi, it is possible to set a default type to either `ChemicalFormula` or `MolecularFormula`. This means that explicit type annotations (i.e., `"CH4" :: ChemicalFormula`) are not required.
```
GHCi> default(ChemicalFormula)
GHCi> "CH4"
ElementSymbolMap {getSymbolMap = fromList [(H,4),(C,1)]}
```

## Design

The Isotope library designed with type safety and flexibility in mind. Key features will be described below.

### Element symbols

In Isotope, element symbols are represented by the enumeration type, `ElementSymbol`, i.e. `data ElementSymbol = H | He | Li | Be .....`. This is advantageous over the use of strings to represent element symbols (i.e. `type ElementSymbol = String`) since it increases type safety. Isotope provides a range of functions which accepts an `ElementSymbol` as input. For example:
```haskell
>>> monoisotopicMass C
12.0
```
Moreover, values of type `ElementSymbol` can be used as keys within maps (see `ElementSymbolMap` section) as an intuitive way to mark elements to their properties. Isotope library presently contains information on the isotopic masses and relative abundances for all elements from Hydrogen to Bismuth and Thorium and Uranium.

### `ElementSymbolMap`

`ElementSymbolMap` is a newtype wrapper for `Map ElementSymbol a`, that is, a mapping from an `ElementSymbol` to a type variable `a`. Presently within the Isotope library, the `ElementSymbolMap` type is used in two places; the `elements` map and `ChemicalFormula` type (see below).

### `Element` and `Isotope`

TODO

### `elements` map

TODO

### `Mass` type class

TODO

### `ChemicalFormula`

TODO

## Comparison to other chemistry libraries
In addition to Isotope, there are two other open-source chemistry libraries written in Haskell; Radium [2] and Ouch [3].

### Radium

Radium is a Haskell library written by klangner and has been uploaded to Hackage [3]. Radium is also under active development with the last commit occurring 15 hours before the time of writing (27/03/16). In comparison to Isotope, the main difference is that Radium does not provide information on the masses and relative abundances of elemental isotopes. Consequently, Radium could not be used for calculating the monoisotopic masses of molecules and ions. Conversely, Radium provides information on electron negativities, ionisation energies and electron configurations and support for SMILES strings which are not provided by Isotope.

### Ouch

Ouch is a chemistry informatics toolkit written in Haskell by Orion Jankowski (Note: Ouch is not on Hackage). Unfortunately, Ouch is no longer under active development with the last commit occurring three years ago. Similar to Radium, Ouch does not provide information on the masses and relative abundances of elemental isotopes making it unsuitable for mass spectrometry. On the otherhand, Ouch provides support for SMILES strings and molecular structure. More information on Ouch can be found on Orion Jankowski's blog [4].

## Future directions

Isotopic profiles currently cannot be calculated for chemical formulae. This is a major limitation since isotopic profiles are important in mass spectrometry. Therefore, this functionality should be added to Isotope in a future version. Since calculating isotopic profiles is computationally expensive, this feature could be introduced using Rust if performance is an issue using pure Haskell code. (Rust is a modern systems programming language with a strong type system and memory safety [6].)

To increase compile-time checks, refinement types could be introduced using LiquidHaskell [7]. For example, not all elements have naturally-occurring isotopes and such elements therefore do not have an average mass. Using LiquidHaskell, the function `averageMass` could be refined to only accept elements with naturally-occurring isotopes. If such a direction is taken, two separate libraries may be maintained; one using LiquidHaskell and the other using only conventional Haskell code.

## Contributions

Please report bugs and feature suggestions using the issue tracker (https://github.com/Michaelt293/isotope/issues). Pull requests are welcome and will be merged provided they improve Isotope.

## Author

The Isotope library is authored and maintained by Michael Thomas. Isotope logo was designed by Ben Jerrems.

## License

Copyright © 2015–2016 Michael Thomas

## References

[1] http://www.sepscience.com/Information/Archive/MS-Solutions/646-/MS-Solutions-8-Confusion-Resulting-from-Molecular-Weight-and-the-Nominal-Mass-Monoisotopic-Mass-and-Average-Molar-Mass

[2] https://github.com/klangner/radium

[3] https://hackage.haskell.org/package/radium

[4] https://github.com/odj/Ouch

[5] http://www.pharmash.com/tags/OUCH.html

[6] https://www.rust-lang.org/

[7] https://ucsd-progsys.github.io/liquidhaskell-tutorial/01-intro.html
