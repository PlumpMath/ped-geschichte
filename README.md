# ped-geschichte

Playground for [geschichte](http://github.com/ghubber/geschichte) repo functions with pedestal. To learn how the pure repo functionality works, look at the docs there.
Use ui-data-renderer for now. Has an easy to debug memory store for the underlying key-value store concept, look on the js-console to see the data-flow and in memory store change.

# Example

To watch how different values flow:

- `:set-user` to "user@mail.com" to allow repository functionality
- `:create` a repository with an initial value and description
- `:set-user` to "other@mail.com"
- copy the uuid string only, e.g. 66c4de57-d4cc-4a5e-bc0f-4cdcc1ca75ee from `:repo`
- and `:clone` it with branch "master"
- `:set-value` to something new and `:commit`
- `:set-user` to "user@mail.com" again
- and `:pull` or `:merge` from "other@mail.com" "master"
- ...


## Usage

To start the app server from a repl, see [this
usage](https://github.com/pedestal/pedestal/tree/master/app#usage)

To start the app server from the commandline: `lein run`.

## Links

* [Overview of how pedestal-app works](http://pedestal.io/documentation/application-overview/)
* [Comprehensive tutorial for pedestal-app](https://github.com/pedestal/app-tutorial)
