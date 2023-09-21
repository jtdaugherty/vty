Supporting a new platform with Vty
==================================

If the existing platform backends for `vty` don't suit your needs
(e.g. `vty-unix` and `vty-windows`) then you might need to write a new
platform package. This document provides some guidance on doing so.

Steps
-----

Here are the high-level steps required to add support for a new platform
for Vty. We'll use a fictional "toy operating system" as the platform in
this example.

1. Create a new package named `vty-<platform>`, e.g. `vty-toy-os`.
1. Expose a single module, `Graphics.Vty.Platform.<PLATFORM>`, e.g.
   `Graphics.Vty.Platform.ToyOS`.
1. Have `Graphics.Vty.Platform.ToyOS` expose:
   * A required function `mkVty :: VtyUserConfig -> IO Vty`.
   * Optionally, a platform-specific settings type `<PLATFORM>Settings`,
     e.g. `data ToyOSSettings`.
   * Optionally, a platform-specific `Vty` constructor using the
     settings, `mkVtyWithSettings :: VtyUserConfig -> ToyOSSettings -> IO Vty`.
1. Optionally, submit a pull request to the
   [`vty-crossplatform`](https://github.com/jtdaugherty/vty-crossplatform)
   repository to get your new platform's package supported transparently
   for users of `vty-crossplatform`.

The details
-----------

At an API level, each platform package is responsible for providing
one and only one function: `mkVty`. It must construct a `Vty` that has
been initialized for the terminal in use. At a minimum, it must take a
`VtyUserConfig` argument, i.e.,

```haskell
mkVty :: VtyUserConfig -> IO Vty
```

where both `Vty` and `VtyUserConfig` come from `Graphics.Vty`.
**The function should be exported by a module called
`Graphics.Vty.Platform.<PLATFORM>`.**

At an implementation level, its `mkVty` function must take care of
initializing terminal input and output for the platform. This is done by
providing implementations of the `Input` and `Output` records from the
`vty` package.

In addition, a platform package may provide its own platform-specific
settings type. For example, the `vty-unix` package provides a
`UnixSettings` type. Platform-specific settings should be only those
settings that make sense on that particular platform. **If the package
provides a settings type, please name it `<PLATFORM>Settings` to be
consistent with existing packages (e.g., `UnixSettings`).**

If the package provides a settings type, we recommend providing a
`mkVty` alternative called `mkVtyWithSettings` specialized to that
settings type in addition to the `mkVty` function, e.g.,

```haskell
mkVtyWithSettings :: VtyUserConfig -> UnixSettings -> IO Vty
```

Ultimately, the platform package's `mkVty` implementation must
construct an `Input` and an `Output` for the terminal. Those can then
be used to build the required `Vty` handle using the `vty` package's
`mkVtyFromPair` function. Here's a simple skeletal example:

```haskell
import Graphics.Vty (Vty, VtyUserConfig, Input, Output, mkVtyFromPair)

data UnixSettings = ...
buildInput :: VtyUserConfig -> IO Input
buildOutput :: UnixSettings -> IO Output

mkVty :: VtyUserConfig -> IO Vty
mkVty userConfig = do
    input <- buildInput userConfig
    out <- buildOutput settings
    mkVtyFromPair input out
```

Can I get the new package integrated with `vty-crossplatform`?
--------------------------------------------------------------

Yes! The build-time requirement to integrate with
the `vty-crossplatform` package is simply that your
package export a function called `mkVty` with type
`VtyUserConfig -> IO Vty`. Open a ticket on the
[`vty-crossplatform`](https://github.com/jtdaugherty/vty-crossplatform)
repository with a request to add your package and we can work out the
details.
