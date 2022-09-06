{ name = "shape-lang-purs"
, dependencies =
  [ "ansi"
  , "arrays"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "free"
  , "fuzzy"
  , "homogeneous"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "react"
  , "react-dom"
  , "record"
  , "refs"
  , "strings"
  , "transformers"
  , "tuples"
  , "undefined"
  , "unsafe-coerce"
  , "uuid"
  , "variant"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
