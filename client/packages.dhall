let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210516/packages.dhall sha256:f5e978371d4cdc4b916add9011021509c8d869f4c3f6d0d2694c0e03a85046c8

let overrides = {=}

let additions =
      { subcategory =
        { dependencies = [ "prelude", "profunctor", "record" ]
        , repo = "https://github.com/matthew-hilty/purescript-subcategory.git"
        , version = "v0.2.0"
        }
      , sequences =
        { dependencies =
            [ "arrays"
            , "assert"
            , "console"
            , "effect"
            , "lazy"
            , "maybe"
            , "newtype"
            , "nonempty"
            , "partial"
            , "prelude"
            , "profunctor"
            , "psci-support"
            , "tuples"
            , "unfoldable"
            , "unsafe-coerce"
            ]
        , repo = "https://github.com/hdgarrood/purescript-sequences.git"
        , version = "v3.0.2"
        }
      }

in  upstream // overrides // additions
