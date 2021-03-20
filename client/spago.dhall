{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-bus"
  , "affjax"
  , "argonaut-core"
  , "codec-argonaut"
  , "console"
  , "debug"
  , "effect"
  , "formatters"
  , "halogen"
  , "nonempty"
  , "now"
  , "precise-datetime"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "quickcheck"
  , "remotedata"
  , "routing"
  , "routing-duplex"
  , "slug"
  , "test-unit"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
