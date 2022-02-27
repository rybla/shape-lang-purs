{ name = "shape-lang-purs"
, dependencies =
  [ "arrays"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "lists"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "record"
  , "tuples"
  , "undefined"
  , "uuid"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
