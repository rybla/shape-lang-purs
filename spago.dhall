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
  , "tuples"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
