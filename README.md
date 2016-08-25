Wires
=====

This is a functional reactive programming library for interactive
applications with the following features:

  * heavy focus on real-time applications like games and simulations,

  * very small core abstraction,

  * efficient in both time and space.

Until a proper tutorial has been written, please check out the
*examples* directory.  If you have questions, join #haskell-game on
irc.freenode.net.  If you would like to report a bug or request a
feature, please [file an
issue](https://github.com/esoeylemez/wires/issues).


Module overview
---------------

The library is split into two roles: *applications* and *controllers*.
An application developer models interactions and implements reactive
systems, i.e. the application logic.  A controller developer implements
the glue between the application and the real world, i.e. how events and
time-varying values map to actual things on the screen or in the
network.  The module structure reflects that distinction:

Module                    | Purpose
--------------------------|-----------------------------------------------
`Control.Wire`            | Application language (basically core + utils).
`Control.Wire.Controller` | Controller language.
`Control.Wire.Core`       | Core application language.
`Control.Wire.Internal`   | You should never need this module.
`Control.Wire.Utils`      | Extra application utilities.

Modules not listed here are highly experimental and should not be used.

If you are asking yourself whether you are an application or a
controller developer: at this early stage of development you are
probably both, which means that you will write the application as well
as connect its inputs and outputs to the real world.
