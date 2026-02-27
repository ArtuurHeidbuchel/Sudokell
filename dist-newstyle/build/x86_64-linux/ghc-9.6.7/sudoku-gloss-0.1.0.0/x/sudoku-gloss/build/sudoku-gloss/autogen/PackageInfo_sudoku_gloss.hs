{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_sudoku_gloss (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "sudoku_gloss"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A Sudoku game programmed in Haskell"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
