{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "stac-repl"
, dependencies =
  [ "aff-promise"
  , "aff"
  , "affjax"
  , "ansi"
  , "arrays"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foldabletraversable",
  , "foreign-bject",
  , "lists"
  , "maybe"
  , "node-readline"
  , "ordered-ollections"
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
