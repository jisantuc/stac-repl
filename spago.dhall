{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "stac-repl"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "ansi"
  , "arrays"
  , "console"
  , "control"
  , "decimals"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign-object"
  , "lists"
  , "maybe"
  , "node-readline"
  , "ordered-collections"
  , "parsing"
  , "prelude"
  , "psci-support"
  , "refs"
  , "stac"
  , "strings"
  , "tuples"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
