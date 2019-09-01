## Reflex Contributors' Playground

This library is intended to be a public playground for developing
infrastructure, higher level APIs, and widget libraries for reflex FRP
applications.  **This library is experimental and does not have a strong
commitment to preserving backwards compatibility.**  It will not have a high
bar for the quality of contributions.  That being said, we prefer commits that
add new things rather than changing existing ones.  If you are wondering if
there is some convenience code or abstractions and you don't find them in
reflex or reflex-dom, look here and see if anyone has already done it.  If you
have general-purpose reflex code that you find useful that is not already
here, add it to this repository and send us a pull request.

Over time the goal is that this repository will help us discover good
abstractions for reflex development that can eventually be merged upstream to
a more stable home with stronger backwards compatibility guarantees.  But we
think the reflex community needs a place to play and explore different ideas.
This is that place.  Have fun!

## Contributing

The easiest way to build reflex-dom-contrib is with Nix. To build the package,
use nix-build like this:

    nix-build -A ghc.reflex-dom-contrib

To enter a shell with all the dependencies, just run `nix-shell`. Then you can
build with cabal as with any other Haskell package.
