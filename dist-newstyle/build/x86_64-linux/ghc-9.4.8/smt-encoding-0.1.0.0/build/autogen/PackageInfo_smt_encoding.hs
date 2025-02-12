{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_smt_encoding (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "smt_encoding"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A toy implementation of how Gillian encodes symbolic state into SMT."
copyright :: String
copyright = ""
homepage :: String
homepage = ""
