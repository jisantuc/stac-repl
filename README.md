# `stac repl` #

A context-aware console application for interacting with local and remote STAC resources.

Quickstart
----------

* clone this repo
* `spago run`
* `<TAB><TAB>` and try out some commands

What can you do so far?

* choose a root url: `set root url stac-api-root-url`
* check conformance: `get conformance`
* list collections (once you have a root url): `list collections`
* choose one of those collections: `set collection <TAB>` (<TAB> will suggest IDs)
* list the first 30 items in a collection: `list items`
* page through items 30 at a time: `next page`
* view a Mapscii map of an item's location: `locate item <TAB>` (<TAB> will suggest item IDs)
* view a Mapscii map of a collection's location: `locate collection`

What can't you do?
------------------

* [search](https://github.com/jisantuc/purescript-stac/issues/11)
* [set item page size](https://github.com/jisantuc/purescript-stac/issues/13)
