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
  , "erl-untagged-union"
  , "exceptions"
  , "foreign"
  , "maybe"
  , "prelude"
  , "record"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, backend = "purerl"
}
