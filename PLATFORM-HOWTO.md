Supporting a new platform with Vty
==================================

If the existing platform backends for `vty` don't suit your needs
(e.g. `vty-unix` and `vty-windows`) then you might need to write a new
platform package. This document provides some guidance on doing so.

What goes in a platform package?
--------------------------------

At an API level, each platform package is responsible for providing one
and only one function: `mkVty`. Its type must be

```haskell
mkVty :: VtyUserConfig -> IO Vty
```

where both `Vty` and `VtyUserConfig` come from `Graphics.Vty`.
**We recommend that the function be exported by a module called
`Grahpics.Vty.Platform.<PLATFORM>`.**

At an implementation level, though, its `mkVty` function must take care
of initializing terminal input and output for the platform. This is done
by providing implementations of the `Input` and `Output` records from
the `vty` package.

In addition, a platform package may provide its own platform-specific
settings type. For example, the `vty-unix` package provides a
`UnixSettings` type. Platform-specific settings should be only those
settings that make sense on that particular platform. **If the package
provides a settings type, we recommend naming it `<PLATFORM>Settings` to
be consistent with existing packages.**

If the package provides a settings type, we recommend providing a
function `mkVtyWithSettings` specialized to that settings type:

```haskell
mkVtyWithSettings :: VtyUserConfig -> UnixSettings -> IO Vty
```

Ultimately, the platform package's `mkVty` implementation must
construct an `Input` and an `Output` for the terminal. Those can then
be used to build the required `Vty` handle using the `vty` package's
`mkVtyFromPair` function.

```haskell
mkVty :: VtyUserConfig -> IO Vty
mkVty userConfig = do
    input <- buildInput userConfig
    out <- buildOutput settings
    mkVtyFromPair input out
```
