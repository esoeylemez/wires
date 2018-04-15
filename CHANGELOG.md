# 0.2.1

  * Fix: bumped version bounds

  * Fix: more strictness fixes

  * Fix: `README.md` formatting for Hackage

  * Add: Semigroup and generalised Monoid instances for `Event`

## Contributors

  * [tsahyt](https://github.com/tsahyt)

# 0.2.0

  * **Breaking change**: Safer API for `manage` and `manage'`

  * Fix: wire strictness; strict value recursion like the following
    works now:

        rec x <- scan x0 -< x `seq` ev

  * Change: tighter version bounds

  * Add: event functions `splitE`, `unalignE`, `unlessE`

  * Add: wire transforms `asksW`, `askW`, `hoistW`, `runReaderW`

## Contributors

  * [Kosyrev Serge](mailto:_deepfire@feelingofgreen.ru)

# 0.1.0

  * Initial version.
