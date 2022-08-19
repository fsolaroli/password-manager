{ name = "password-generator"
, sources = [ "src/**/*.purs" ]
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arraybuffer"
  , "arraybuffer-builder"
  , "arraybuffer-types"
  , "arrays"
  , "b64"
  , "bifunctors"
  , "bigints"
  , "concur-core"
  , "concur-react"
  , "console"
  , "control"
  , "decimals"
  , "effect"
  , "either"
  , "encoding"
  , "exceptions"
  , "foldable-traversable"
  , "fortuna"
  , "functions"
  , "http-methods"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "spec"
  , "strings"
  , "subtlecrypto"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
}
