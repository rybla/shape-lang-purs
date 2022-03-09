{ name = "shape-lang-purs"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "homogeneous"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "record"
  , "tuples"
  , "undefined"
  , "uuid"
  , "variant"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
