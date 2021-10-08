{ name = "erl-ranch"
, dependencies =
  [ "convertable-options"
  , "effect"
  , "either"
  , "erl-atom"
  , "erl-kernel"
  , "erl-lists"
  , "erl-maps"
  , "erl-otp-types"
  , "erl-process"
  , "erl-ssl"
  , "erl-tuples"
  , "exceptions"
  , "foreign"
  , "maybe"
  , "prelude"
  , "record"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
