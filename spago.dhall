{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "ansi"
  , "console"
  , "effect"
  , "monad-control"
  , "node-process"
  , "node-readline"
  , "parsing"
  , "psci-support"
  , "stac"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
