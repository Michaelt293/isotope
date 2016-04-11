[![Build Status](https://travis-ci.org/Michaelt293/isotope.svg?branch=master)](https://travis-ci.org/Michaelt293/isotope)

![alt tag](https://github.com/Michaelt293/isotope/blob/master/isotope_jpeg.jpg)

WARNING: Isotope is currently undergoing major changes in preparation for Hackage. Consequently, this code repository may be broken or be out of line with its documentation.

* [Design](#design)
    * [Isotopic, integer, monoisotopic, nominal and average masses](#isotopic-integer-monoisotopic-nominal-and-average-masses)
    * [Element symbols](#element-symbols)
    * [Molecular and condensed formulae](#molecular-and-condensed-formule)
    * [ChemicalMass type class](#chemicalmass-type-class)
* [Comparison to other chemistry libraries](#comparison-to-other-chemistry-libraries)
    * [Radium](#radium)
    * [Ouch](#ouch)
* [Future directions](#future-directions)
* [Contributions](#contributions)
* [Author](#author)
* [License](#license)
* [References](#references)

Isotope is a chemistry library for calculating masses of elements and molecules. The main focus of the Isotope library is mass spectrometry, an area where the masses and relative abundances of isotopes and isotopologues is of central importance.

## Design

The Isotope library designed with type safety and flexibility in mind. Key features will be described below.

### Isotopic, integer, monoisotopic, nominal and average masses

In mass spectrometry and general chemistry, there are several different ways in which to describe mass. This can lead to some confusion since more than one term can exist to describe the same unit of measurement. In the Isotope library, the following conventions are used:

Mass | Description
--- | ---
Isotopic mass | The mass of an isotope for an element.
Integer mass | The mass of an isotope rounded to the nearest integer value.
Monoisotopic mass | The mass of the most abundant isotope for an element or the sum of the masses of the most abundant isotope of each element for a molecule.
Nominal mass | The integer mass of the most abundant isotope for an element or the sum of integer masses of the most abundant isotope of each element for a molecule.
Average mass | The average mass of an element or molecule based on naturally-occurring abundances. In the isotopes library, average mass is used in place of atomic mass and molecular mass.

For more detailed discussion regarding the concept of mass in mass spectrometry, please refer to "Molecular Weight and the Nominal Mass, Monoisotopic Mass and Average Molar Mass" by Prof. O. David Sparkman [1].


### Element symbols

In Isotope, element symbols are represented by the enumeration type, `ElementSymbol`, i.e. `data ElementSymbol = H | He | Li | Be .....`. This is advantageous over the use of strings to represent element symbols (i.e. `type ElementSymbol = String`) since it increases type safety. Moreover, values of type `ElementSymbol` can be used as keys within maps (see `ElementSymbolMap` in Haddock documentation) as an intuitive way to map elements to their properties. Isotope presently contains information on the isotopic masses and relative abundances for all elements from Hydrogen to Bismuth and Thorium and Uranium.

### Molecular and condensed formulae

In the Isotope library, a distinction between molecular and condensed formulae is made. Molecular formulae contain the total number of atoms for each element of a molecule. Conversely, condensed formulae give information on the connectivity of atoms within molecules. For example, the molecule trimethylamine has a molecular formula of C3H9N and a condensed formula of N(CH3)3. Here the molecular formula indicates trimethyelamine has a total of 3 carbon atoms, 9 hydrogen atoms and 1 nitrogen whereas the condensed formula indicates trimethylamine has 3 methyl groups bonded to a central nitrogen.

`MolecularFormula` and `CondensedFormula` are instances of `IsString`. Therefore, shorthand notation can be used when working with molecular and condensed molecular formulae in addition to the constructor `mkElementSymbolMap`.
```haskell
GHCi> ("CH4" :: MolecularFormula) == mkMolecularFormula [(C,1),(H,4)]
True
```
If molecular formula strings are used in source code, errors will not be detected at compile-time and will result in run-time errors. Therefore, it is preferable to use the smart constructor `mkMolecularFormula` or to use GHCi to convert molecular formula strings to `MolecularFormula` values which can then be copied and pasted into source code. When using GHCi, it is possible to set a default type to either `MolecularFormula` or `CondensedFormula` for convenience. This means that explicit type annotations (i.e., `"CH4" :: MolecularFormula`) are not required.
```haskell
GHCi> default(MolecularFormula)
GHCi> "CH4"
ElementSymbolMap {getSymbolMap = fromList [(H,4),(C,1)]}
```
The Isotope library comes with three operators for working with molecular formulae; `|+|`, `|-|` and `|*|`. These operators have the same fixity and associativity as `+`, `-` and `*`, respectively. This allows us to the `|+|`, `|-|` and `|*|` operators in an intuitive manner (i.e., like basic arithmetic). For example, we could define the molecule formula of propane in terms of its building blocks; that is, 2 methyl groups and 1 methylene group.
```haskell
GHCi> let methyl = "CH3" :: MolecularFormula
GHCi> let methylene = "CH2" :: MolecularFormula
GHCi> let propane = (2 :: Int) |*| methyl |+| methylene
GHCi> propane
ElementSymbolMap {getSymbolMap = fromList [(H,8),(C,3)]}
```
We could then go one step further and define propene to be propane minus molecular hydrogen.
```haskell
GHCi> let propene = propane |-| ("H2" :: MolecularFormula)
GHCi> propene
ElementSymbolMap {getSymbolMap = fromList [(H,6),(C,3)]}
```

### `ChemicalMass` type class

The `ChemicalMass` type class has four methods; `getElementalComposition`, `monoisotopicMass`, `nominalMass` and `averageMass`, where `getElementalComposition` is the minimal complete definition. `ElementSymbol`,`MolecularFormula` and `CondensedFormula` are instances of `ChemicalMass`. This provides a uniform approach to working with elements, molecular formulae and condensed formulae.
```haskell
GHCi> nominalMass C
12
GHCi> averageMass ("CH4" :: MolecularFormula)
16.042498912958358
GHCi> monoisotopicMass ("N(CH3)3" :: CondensedFormula)
59.073499294499996
```
It should be noted that Isotope also provides a range of addition functions which accepts an `ElementSymbol` as input. For example, to get the masses of all isotopes for titanium, we simply have to pass the element symbol `Ti` to the function `isotopicMasses`.
```haskell
GHCi> isotopicMasses Ti
[45.95262772,46.95175879,47.94794198,48.94786568,49.94478689]
```

## Comparison to other chemistry libraries
In addition to Isotope, there are two other open-source chemistry libraries written in Haskell; Radium [2] and Ouch [3].

### Radium

Radium is a Haskell library written by klangner and has been uploaded to Hackage [3]. Radium is also under active development with the last commit occurring 15 hours before the time of writing (27/03/16). In comparison to Isotope, the main difference is that Radium does not provide information on the masses and relative abundances of elemental isotopes. Consequently, Radium could not be used for calculating the monoisotopic masses of molecules and ions. Conversely, Radium provides information on electron negativities, ionisation energies and electron configurations and support for SMILES strings which are not provided by Isotope.

### Ouch

Ouch is a chemistry informatics toolkit written in Haskell by Orion Jankowski (Note: Ouch is not on Hackage). Unfortunately, Ouch is no longer under active development with the last commit occurring three years ago. Similar to Radium, Ouch does not provide information on the masses and relative abundances of elemental isotopes making it unsuitable for mass spectrometry. On the otherhand, Ouch provides support for SMILES strings and molecular structure. More information on Ouch can be found on Orion Jankowski's blog [4].

## Future directions

Isotopic profiles currently cannot be calculated for molecular formulae. This is a major limitation since isotopic profiles are important in mass spectrometry. Therefore, this functionality should be added to Isotope in a future version. Since calculating isotopic profiles is computationally expensive, this feature could be introduced using Rust if performance is an issue using pure Haskell code. (Rust is a modern systems programming language with a strong type system and memory safety [6].)

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
