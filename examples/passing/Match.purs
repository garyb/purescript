module Match where

  data Foo a = Foo

  foo = \f -> case f of Foo -> "foo"
