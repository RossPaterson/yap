# Yet Another Prelude (for Haskell)

A simple refactoring of the Haskell Prelude numeric classes, aiming
to escape from the restrictive bundling of operations in the standard
numeric classes, while preserving backward compatibility for clients.
This is done by adding algebraic classes as superclasses of the Haskell 2010
numeric classes, yielding the following class hierarchy (grey classes
are unchanged):

![Gragh of superclass relationships](/images/hierarchy.svg)

See the [Hackage page](https://hackage.haskell.org/package/yap) for documentation.
