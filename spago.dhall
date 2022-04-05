{ name = "shape-lang-purs"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "homogeneous"
  , "lists"
  , "maybe"
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
  , "unfoldable"
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
