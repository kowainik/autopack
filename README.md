# autopack

![logo](https://user-images.githubusercontent.com/4276606/77097260-6bdab780-6a08-11ea-8b86-f8e52f3dbbbc.png)
[![GitHub CI](https://github.com/kowainik/autopack/workflows/CI/badge.svg)](https://github.com/kowainik/autopack/actions)
[![Build status](https://img.shields.io/travis/kowainik/autopack.svg?logo=travis)](https://travis-ci.org/kowainik/autopack)
[![Windows build status](https://ci.appveyor.com/api/projects/status/github/kowainik/autopack?branch=master&svg=true)](https://ci.appveyor.com/project/kowainik/autopack)

[![Hackage](https://img.shields.io/hackage/v/autopack.svg?logo=haskell)](https://hackage.haskell.org/package/autopack)
[![Stackage Lts](http://stackage.org/package/autopack/badge/lts)](http://stackage.org/lts/package/autopack)
[![Stackage Nightly](http://stackage.org/package/autopack/badge/nightly)](http://stackage.org/nightly/package/autopack)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

Custom Setup to automate package modules discovery.

⚠️ __WARNING:__ `autopack` is in early beta phase. ⚠️

## Motivation

Usually, when working on a Haskell project, you end up having lots of modules.
During the development, you can add, remove or rename any of them. And this is
fine unless you continuously need remembering to add, remove or rename the
corresponding module line in the `.cabal` file. Sometimes all you want is to
make `Cabal` dealing with it, so you won't need to patch any files when
performing any operations on modules. Good news, everyone! This project was
created precisely to help to solve this particular issue.

## How does `autopack` work

`autopack` is a Haskell library that provides custom setup functions that
discover all exposed `.hs` files for your library from the `hs-source-dirs`
folders. It uses this information to prehook the list of identified modules into
the `exposed-modules` field of the library stanza of your package description.

You can use
[Cabal's custom setup scripts](https://www.haskell.org/cabal/users-guide/developing-packages.html#custom-setup-scripts)
to use this library in your project. In the next section, we are going to give
detailed instructions on that.


For now, all you need to keep in mind to use `autopack`:

 * It uses
   [`hs-source-dirs`](https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-hs-source-dirs)
   field to establish where to look up for modules.
 * It can work only with `.hs` extension at the moment.
 * It adds all discovered modules into
   [`exposed-modules`](https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-library-exposed-modules)
   of the library stanza.
 * If there are already some modules in the `exposed-modules`, `autopack` will
   concatenate lists.

## How to use `autopack`

First, make sure that you are using the Cabal version at least `2.0` in your
`.cabal` file. For example:

```haskell
cabal-version:       2.4
```

Now you need to change the
[`build-type`](https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-build-type)
field in your `package_name.cabal` file to `Custom` instead of the default
`Simple`:

```haskell
build-type:    Custom
```

Then you have to add `custom-setup` section before you defining your library
stanza. It should have the `autopack` dependency so you can use it in your
`Setup`:

```haskell
custom-setup
    setup-depends: base
                 , autopack
```

And the final preparation. You should add the `Setup.hs` module (or replace the
default one) in the root directory of the package with the following content:

```haskell
import Autopack (defaultMainAutoModules)


main :: IO ()
main = defaultMainAutoModules
```

You are all set up now!

You can remove `exposed-module` field from your `.cabal`
file completely, and `autopack` will discover all the `.hs` modules in your
folders for you during the build.

## Alternatives

As `Cabal` does not currently provide the feature of automatic modules
discovery, there are some workarounds for this process. One of them is this
library — `autopack` that uses `Cabal`'s Setup feature to discover all modules
during the pre-build stage of `Cabal`. Another tool that provides it
out-of-the-box is [`hpack`](https://github.com/sol/hpack). It is the wrapper on
`Cabal` syntax via `YAML` where there is no need to write modules explicitly.
They are added to the `exposed-modules` field of the `.cabal` file during the
YAML to `.cabal` step of the tool work process.

## Acknowledgement

Icons made by [Freepik](http://www.freepik.com) from [www.flaticon.com](https://www.flaticon.com/) is licensed by [CC 3.0 BY](http://creativecommons.org/licenses/by/3.0/).
