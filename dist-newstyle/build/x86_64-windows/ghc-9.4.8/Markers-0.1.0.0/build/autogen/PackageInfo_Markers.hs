{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Markers (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Markers"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A Markup Language Interpreter made for Academic Papers and Research."
copyright :: String
copyright = ""
homepage :: String
homepage = "themarkersfoundation.github.io"
