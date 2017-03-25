# 0.2.0

  * **Breaking change**: Safer API for `manage` and `manage'`.

  * Fix: Wire strictness.  Strict value recursion like the following
    works now:

        rec x <- scan x0 -< x `seq` ev

  * Change: Tighter version bounds.

  * Add: `splitE`, `unalignE`, `unlessE`.

  * Add: `hoistW`.

  * Add: `asksW`, `askW`, `runReaderW`.

# 0.1.0

  * Initial version.
