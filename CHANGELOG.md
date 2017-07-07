### 0.5.0.0

* Added `Ion` module.
* Added `charge` method to `ToElementalComposition` type class.
* Added `Charge` type alias.

### 0.5.1.0

* Previously, Isotope was using string-based error messages for `mz` and `polarity` methods. A custom exception type, `IonHasChargeZero`, is introduced to replace these string-based errors.
