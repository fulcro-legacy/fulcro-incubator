# Fulcro Incubator

This is a set of experimental features/utilities for Fulcro.  As these features mature we will consider migrating them into Fulcro or forking them as first-class libraries.  In the meantime we plan on following usability practices with this library: we will largely refrain from making breaking changes/removal to anything within, but will instead generate new namespaces for variations.

So, if you see something in here that you want to use: please feel free to do so.  The worst thing that can happen is it proves unpopular and you have to adopt some of it into your source base later, but that's better than having to write it from scratch, right?

## Install

Latest version: [![Clojars Project](https://img.shields.io/clojars/v/fulcrologic/fulcro-incubator.svg)](https://clojars.org/fulcrologic/fulcro-incubator)

## DB Helpers

At [`fulcro.incubator.db-helpers`](https://github.com/fulcrologic/fulcro-incubator/blob/develop/src/fulcro/incubator/db_helpers.cljc) you can find helper functions to deal with Fulcro local
database map format. Those are helper functions for common operations like creating
a new entity, recursively removing data and it's references. The documentation for the
functions is in the code, give a scan there to check what's available.

## UI

[`fulcro.incubator.ui.core`](https://github.com/fulcrologic/fulcro-incubator/blob/develop/src/fulcro/incubator/ui/core.cljs)
contains functions to help using React components with Fulcro.

### Reakit

You can use [Reakit](https://reakit.io/) wrapped with Fulcro DOM CSS support from [`fulcro.incubator.ui.reakit`](https://github.com/fulcrologic/fulcro-incubator/blob/develop/src/fulcro/incubator/ui/reakit.cljs).

### React Icons

[React icons](http://react-icons.github.io/react-icons/) support is provided via [`fulcro.incubator.ui.icons.*`](https://github.com/fulcrologic/fulcro-incubator/tree/develop/src/fulcro/incubator/ui/icons)
namespaces, just refer to the functions there to use the icons directly.

## Shadow CLJS required

Currently this library requires usage of Shadow CLJS for compilation, this is due the
direct use of libraries from NPM that are not available in cljsjs.

## Compiling workspaces

To explore the things here, clone this project and run:

```
npm install
npx shadow-cljs watch workspaces
```

Then navigate to

```
http://localhost:3689/
```

You can view a precompiled version of the workspaces on [github.io](https://fulcrologic.github.io/fulcro-incubator/)
