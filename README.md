[![Build Status](https://travis-ci.org/Michaelt293/isotope.svg?branch=master)](https://travis-ci.org/Michaelt293/isotope)
[![Gitter](https://badges.gitter.im/Michaelt293/isotope.svg)](https://gitter.im/Michaelt293/isotope)

![alt tag](https://github.com/Michaelt293/isotope/blob/master/isotope_jpeg.jpg)

* [Design](#design)
    * [Isotopic, integer, monoisotopic, nominal and average masses](#isotopic-integer-monoisotopic-nominal-and-average-masses)
    * [Element symbols](#element-symbols)
    * [Elemental composition and molecular, condensed and empirical formulae](#elemental-composition-and-molecular-condensed-and-empirical-formulae)
    * [ElementalComposition, MolecularFormula, CondensedFormula, EmpiricalFormula quasiquoters](#elementalcomposition-molecularformula-condensedformula-empiricalformula-quasiquoters)
    * [Conversion between ElementalComposition, MolecularFormula, CondensedFormula and EmpiricalFormula data types](#conversion-between-elementalcomposition-molecularformula-condensedformula-and-empiricalformula-data-types)
    * [Operators for working with formulae and masses](#operators-for-working-with-formulae-and-masses)
    * [ToElementalComposition type class](#elementalcomposition-type-class)
    * [Behaviour of ElementalComposition, MolecularFormula, CondensedFormula and EmpiricalFormula data types](#behaviour-of-elementalcomposition-molecularformula-condensedformula-and-empiricalformula-data-types)
    * [Additional functions accepting an ElementSymbol as input](#additional-functions-accepting-an-elementsymbol-as-input)
    * [Representing ions in Isotope](#representing-ions-in-isotope)
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
Average mass | The average mass of an element or molecule based on naturally-occurring isotopic abundances. In the Isotope library, average mass is used in place of atomic mass and molecular mass.

For more detailed discussion regarding the concept of mass in mass spectrometry, please refer to "Molecular Weight and the Nominal Mass, Monoisotopic Mass and Average Molar Mass" by Prof. O. David Sparkman [1].


### Element symbols

In Isotope, element symbols are represented by the enumeration type, `ElementSymbol`, i.e. `data ElementSymbol = H | He | Li | Be .....`. This is advantageous over the use of strings to represent element symbols (i.e. `type ElementSymbol = String`) since it increases type safety. Moreover, values of type `ElementSymbol` can be used as keys within maps as an intuitive way to map elements to their properties. Isotope presently contains information on the isotopic masses and relative abundances for all elements from Hydrogen to Bismuth and Thorium and Uranium (excluding Technetium and promethium).

### Elemental composition and molecular, condensed and empirical formulae

In the Isotope library, a distinction between elemental composition and molecular, condensed and empirical formulae is made. Molecular formulae contain the total number of atoms for each element of a molecule while condensed formulae give information on the connectivity of atoms within molecules. For example, the molecule trimethylamine has a molecular formula of C3H9N and a condensed formula of N(CH3)3. Here the molecular formula indicates trimethyelamine has a total of 3 carbon atoms, 9 hydrogen atoms and 1 nitrogen whereas the condensed formula indicates trimethylamine has 3 methyl groups bonded to a central nitrogen. Conversely, an empirical formula is the simplest integer ratio for the atoms of a compound. For example, the molecular formula of benzene is C6H6 whereas the empirical formula of benzene is simply CH. Molecular, condensed and empirical formulae may all be considered to have an elemental composition. That is, the total number of atoms for each element for a formulae.

In Isotope, `CondensedFormula` is defined as a recursive data type. This allows deep nesting within `CondensedFormula`. For example, triisopropylamine may be expressed as `[con|N(CH(CH3)2)3|]`. This nesting can therefore be used to convey greater structural detail.

### `ElementalComposition`, `MolecularFormula`, `CondensedFormula`, `EmpiricalFormula` quasiquoters

The quasiquoters, `ele`, `mol`, `con` and `emp` are provided for `ElementalComposition`, `MolecularFormula`, `CondensedFormula`, `EmpiricalFormula` and  data types, respectively. This allows the use of shorthand notation when working with elemental compositions as well as molecular, condensed and empirical formulae. The use of quasiquoters requires the use of the `QuasiQuotes` language extension (`:set -XQuasiQuotes` can be added to the `.ghci` file when working in GHCi).
```haskell
GHCi> [mol|CH4|]
MolecularFormula {getMolecularFormula = fromList [(H,4),(C,1)]}
```
Importantly, errors in formulae will be detected at compile-time and give informative error messages! (Note the example below is from a GHCi session.)
```haskell
GHCi> [mol|Ch4|]

<interactive>:4:6:
    Could not parse formula: 1:2:
unexpected 'h'
expecting "Ag", "Al", "Ar", "As", "Au", "Ba", "Be", "Bi", "Br", "Ca", "Cd", "Ce", "Cl", "Co", "Cr", "Cs", "Cu", "Dy", "Er", "Eu", "Fe", "Ga", "Gd", "Ge", "He", "Hf", "Hg", "Ho", "In", "Ir", "Kr", "La", "Li", "Lu", "Mg", "Mn", "Mo", "Na", "Nb", "Nd", "Ne", "Ni", "Os", "Pa", "Pb", "Pd", "Pm", "Pr", "Pt", "Rb", "Re", "Rh", "Ru", "Sb", "Sc", "Se", "Si", "Sm", "Sn", "Sr", "Ta", "Tb", "Tc", "Te", "Th", "Ti", "Tl", "Tm", "Xe", "Yb", "Zn", "Zr", '(', 'B', 'C', 'F', 'H', 'I', 'K', 'N', 'O', 'P', 'S', 'U', 'V', 'W', 'Y', or end of input
```

### Conversion between `ElementalComposition`, `MolecularFormula`, `CondensedFormula` and `EmpiricalFormula` data types

A condensed formula can be converted to a molecular or empirical formula and a molecular formula can be converted to an empirical formula. All formulae can be converted to an `ElementalComposition` and an `ElementalComposition` may be converted to an `EmpiricalFormula` In Isotope, this functionality is provided by three type classes, `ToElementalComposition`, `ToMolecularFormula` and `ToEmpiricalFormula`, which contain the methods, `ToElementalComposition`, `toMolecularFormula` and `toEmpiricalFormula`, respectively. In addition, a `ToCondensedFormula` type class containing the `toCondensedFormula` method is also provided for users of Isotope.

```haskell
GHCi> let butane = [con|CH3(CH2)2CH3|]
GHCi> toElementalComposition butane
ElementalComposition {getElementalComposition = fromList [(H,10),(C,4)]}
GHCi> toMolecularFormula butane
MolecularFormula {getMolecularFormula = fromList [(H,10),(C,4)]}
GHCi> toEmpiricalFormula butane
EmpiricalFormula {getEmpiricalFormula = fromList [(H,5),(C,2)]}
```
When using quasiquoters, it is possible to do this implicitly. For example, the quasiquoter `emp` could be applied to the condensed formula CH3(CH2)2CH3 used above to yield a value of type `EmpiricalFormula`.
```haskell
GHCi> [emp|CH3(CH2)2CH3|]
EmpiricalFormula {getEmpiricalFormula = fromList [(H,5),(C,2)]}
```

### Operators for working with formulae and masses

The Isotope library comes with three operators for working with formulae and masses; `|+|`, `|-|` and `|*|`. These operators are provided in the `Operators` type class and have the same fixity and associativity as `+`, `-` and `*`, respectively. This allows us to the `|+|`, `|-|` and `|*|` operators in an intuitive manner (i.e., like basic arithmetic). For example, we could define the molecule formula of propane in terms of its building blocks; that is, 2 methyl groups and 1 methylene group.
```haskell
GHCi> let methyl = [mol|CH3|]
GHCi> let methylene = [mol|CH2|]
GHCi> let propane = methyl |*| 2 |+| methylene
GHCi> propane
MolecularFormula {getMolecularFormula = fromList [(H,8),(C,3)]}
```
We could then go one step further and define propene to be propane minus molecular hydrogen.
```haskell
GHCi> let propene = propane |-| [mol|H2|]
GHCi> propene
MolecularFormula {getMolecularFormula = fromList [(H,6),(C,3)]}
```

### `ToElementalComposition` type class

`ToElementalComposition` is a superclass of `ToMolecularFormula`, `ToCondensedFormula` and `ToEmpiricalFormula`. In addition to the `toElementalComposition` method, the `ToElementalComposition` type class has four other methods; `charge`, `monoisotopicMass`, `nominalMass` and `averageMass`. (`toElementalComposition` and `charge` is the minimal complete definition.) `ElementSymbol`, `ElementalComposition`, `MolecularFormula`, `CondensedFormula` and `EmpiricalFormula` all have instances of `ToElementalComposition`. This provides a uniform approach to working with elements, elemental compositions, molecular formulae, condensed formulae and empirical formulae.
```haskell
ghci> nominalMass C
NominalMass {getNominalMass = 12}
ghci> averageMass [mol|CH4|]
AverageMass {getAverageMass = 16.042498912958358}
ghci> monoisotopicMass [con|N(CH3)3|]
MonoisotopicMass {getMonoisotopicMass = 59.073499294499996}
```

### Behaviour of `ElementalComposition`, `MolecularFormula`, `CondensedFormula` and `EmpiricalFormula` data types

It is possible to combine two `MolecularFormula` to form another `MolecularFormula`. For example, we could combine the molecular formulae of two methyl groups and a methylene group and the resulting molecular formula would be that of propane, regardless of the order of operators. Also, if we add an empty molecular formula to a molecular formula, we get the same molecular formula. These properties indicate that combining molecular formulae is monoidal. Consequently, a monoid instance is provided for `MolecularFormula` where `mempty` is `emptyFormula` and `mappend` is `(|+|)`. This monoid instance can be very helpful in writing clear, concise code. An example of this is provided below. (Note that in this example `a`, `b`, `c` have type `FattyAcyl` and it is assumed a `ToMolecularFormula` instance for `FattyAcyl` is already provided.)

```haskell
instance ToMolecularFormula Triacylglycerol where
  toMolecularFormula (Triacylglycerol a b c) =
    mkMolecularFormula [(C, 3), (H, 5)] |+| foldMap toMolecularFormula [a, b, c]
```
Similar to `MolecularFormula`, a monoid instance is also provided for `CondensedFormula`. (Note, however, that `MolecularFormula` is a commutative monoid, i.e., `a mappend b == b mappend a`, whereas `CondensedFormula` is not.) On the other hand, combining `EmpiricalFormula` is not useful. For example, combining the empirical formula for ethene with the empirical formula of water is not the same as the empirical formula for ethanol.
```haskell
[emp|CH2|] `plus` [emp|H2O|] /= [emp|C2H5O|] -- pseudo Haskell code - `plus` is not provided in Isotope
```
Monoid instances are also provide for `ElementalComposition`, `MonoisotopicMass`, `NominalMass`, `AverageMass` and `IsotopicMass`.

#### Laws for `ElementalComposition`, `MolecularFormula`, `EmpiricalFormula` and `CondensedFormula` data types

Instances of `ToElementalComposition`, `ToMolecularFormula`, `ToCondensedFormula` and `ToEmpiricalFormula` should abide by three laws. 1) Applying `toEmpiricalFormula` to a `CondensedFormula` should give the same result as applying `toMolecularFormula` compose `toEmpiricalFormula`.
2) Applying `toElementalComposition` to a `CondensedFormula` should give the same result as applying `toMolecularFormula` compose `toElementalComposition`.
3) Applying `toElementalComposition . toEmpiricalFormula` to an `EmpiricalFormula` should return the same `EmpiricalFormula`.

```haskell
toEmpiricalFormula x = toEmpiricalFormula . toMolecularFormula $ x

toElementalComposition x = toElementalComposition . toMolecularFormula $ x

(toEmpiricalFormula . toElementalComposition) y = y

where x :: CondensedFormula
      y :: EmpiricalFormula
```

### Additional functions accepting an `ElementSymbol` as input

Isotope also provides a range of addition functions which accepts an `ElementSymbol` as input. For example, to get the masses of all isotopes for titanium, we simply have to pass the element symbol `Ti` to the function `isotopicMasses`.
```haskell
ghci> isotopicMasses Ti
[IsotopicMass {getIsotopicMass = 45.95262772},IsotopicMass {getIsotopicMass = 46.95175879},IsotopicMass {getIsotopicMass = 47.94794198},IsotopicMass {getIsotopicMass = 48.94786568},IsotopicMass {getIsotopicMass = 49.94478689}]
```

### Representing ions in Isotope

Ions are represented in `Isotope` using the `Ion` type class. The `Ion` type class has two methods, `mz` and `polarity`; where `mz` is mass-to-charge ratio and `polarity` is either `Positive` or `Negative`. Any type with an `ToElementalComposition` instance can have an `Ion` instance if charge is not equal to zero. If charge is equal to zero, a runtime exception will occur! Ideally, the type system should be put better use to catch this error at compile-time.

```haskell
data Ammonium = Ammonium deriving Show

instance ToElementalComposition Ammonium where
  toElementalComposition _ = mkElementalComposition [(N, 1), (H, 4)]
  charge _ = Just 1

instance Ion Ammonium

ghci> mz Ammonium
Mz {getMz = 18.03437413335}
```

`Protonated` and `Deprotonated` types, with `Ion` instances, are provided to represent protonated and deprotonated ions, respectively.

```haskell
ghci> mz . Protonated $ mkMolecularFormula [(H, 2), (O, 1)]
Mz {getMz = 19.01838971626}
ghci> mz . Deprotonated $ mkMolecularFormula [(H, 2), (O, 1)]
Mz {getMz = 17.0027396518}
ghci> polarity . Protonated $ mkMolecularFormula [(H, 2), (O, 1)]
Positive
ghci> polarity . Deprotonated $ mkMolecularFormula [(H, 2), (O, 1)]
Negative
```

## Comparison to other chemistry libraries
In addition to Isotope, there are two other open-source chemistry libraries written in Haskell; Radium [2] and Ouch [3].

### Radium

Radium is a Haskell library written by klangner and has been uploaded to Hackage [3]. Radium is also under active development with the last commit occurring 15 hours before the time of writing (27/03/16). In comparison to Isotope, the main difference is that Radium does not provide information on the masses and relative abundances of elemental isotopes. Consequently, Radium could not be used for calculating the monoisotopic masses of molecules and ions. Conversely, Radium provides information on electron negativities, ionisation energies and electron configurations and support for SMILES strings which are not provided by Isotope.

### Ouch

Ouch is a chemistry informatics toolkit written in Haskell by Orion Jankowski (Note: Ouch is not on Hackage). Unfortunately, Ouch is no longer under active development with the last commit occurring three years ago. Similar to Radium, Ouch does not provide information on the masses and relative abundances of elemental isotopes making it unsuitable for mass spectrometry. On the otherhand, Ouch provides support for SMILES strings and molecular structure. More information on Ouch can be found on Orion Jankowski's blog [4].

## Future directions

Isotopic profiles currently cannot be calculated for molecular formulae. This is a major limitation since isotopic profiles are important in mass spectrometry. Therefore, this functionality should be added to Isotope in a future version. Since calculating isotopic profiles is computationally expensive, this feature could be introduced using Rust if performance is an issue using pure Haskell code. (Rust is a modern systems programming language with a strong type system and memory safety [6].)

To increase compile-time checks, refinement types could be introduced using LiquidHaskell [7]. For example, not all elements have naturally-occurring isotopes and such elements therefore do not have an average mass. Using LiquidHaskell, the function `averageMass` could be refined to only accept elements with naturally-occurring isotopes.

## Contributions

Please report bugs and feature suggestions using the [issue tracker](https://github.com/Michaelt293/isotope/issues). Pull requests are welcome and will be merged provided they improve Isotope. Suggestions or questions about Isotope can also be directed towards the [Gitter room](https://gitter.im/Michaelt293/isotope).

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
