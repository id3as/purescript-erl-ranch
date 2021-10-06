let upstream =
  https://github.com/purerl/package-sets/releases/download/erl-0.14.3-20210709/packages.dhall sha256:9b07e1fe89050620e2ad7f7623d409f19b5e571f43c2bdb61242377f7b89d941

in upstream
  with convertable-options =
    { repo = "https://github.com/natefaubion/purescript-convertable-options"
    , dependencies = [ "effect", "maybe", "record" ]
    , version = "f20235d464e8767c469c3804cf6bec4501f970e6"
    }
  with erl-untagged-union =
    { repo = "https://github.com/id3as/purescript-erl-untagged-union.git"
    , dependencies =
      [ "erl-atom"
      , "erl-binary"
      , "erl-lists"
      , "erl-tuples"
      , "debug"
      , "foreign"
      , "typelevel-prelude"
      , "maybe"
      , "partial"
      , "prelude"
      , "unsafe-coerce"
      ]
    , version = "e6f009904aa8d14a5417d33ba8533d7d45416cb1"
    }
  with erl-kernel =
    { repo = "https://github.com/id3as/purescript-erl-kernel.git"
    , dependencies =
     [ "convertable-options"
      , "datetime"
      , "effect"
      , "either"
      , "erl-atom"
      , "erl-binary"
      , "erl-lists"
      , "erl-process"
      , "erl-tuples"
      , "erl-untagged-union"
      , "foldable-traversable"
      , "foreign"
      , "functions"
      , "integers"
      , "maybe"
      , "newtype"
      , "partial"
      , "prelude"
      , "record"
      , "typelevel-prelude"
      , "unsafe-coerce"
      ]
    , version = "75765b66c93825afc226282450be43fca8997b2f"
    }
  with unsafe-reference =
    { repo = "https://github.com/purerl/purescript-unsafe-reference.git"
    , dependencies = [ "prelude"  ]
    , version = "464ee74d0c3ef50e7b661c13399697431f4b6251"
    }
  with erl-otp-types = 
    { repo = "https://github.com/id3as/purescript-erl-otp-types.git"
    , dependencies = 
      [ "erl-atom"
      , "erl-binary"
      , "erl-kernel"
      , "foreign"
      , "prelude"
      , "unsafe-reference"
      ]
      , version = "dad9e77458013bdd16d1cad49e6fce702df57f65"
    }
  with erl-ssl = 
    { repo = "https://github.com/id3as/purescript-erl-ssl.git"
    , dependencies = 
      [ "convertable-options"
      , "datetime"
      , "effect"
      , "either"
      , "maybe"
      , "erl-atom"
      , "erl-binary"
      , "erl-lists"
      , "erl-kernel"
      , "erl-tuples"
      , "erl-logger"
      , "erl-otp-types"
      , "foreign"
      , "maybe"
      , "partial"
      , "prelude"
      , "record"
      , "unsafe-reference"
      ]
      , version = "2bd94ce343448406e579425e1b4140a6b6dd7de0"
    }
