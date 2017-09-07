module Hakyll.Web.CompileSass
( sassCompiler
, scssCompiler
) where

import Control.Monad
import Hakyll

compileWith :: [String] -> Compiler (Item String)
compileWith flags =
    liftM (fmap compressCss) (getResourceString
        >>= withItemBody (unixFilter "sass" flags)
    )

sassCompiler :: Compiler (Item String)
sassCompiler = compileWith ["-s"]

scssCompiler :: Compiler (Item String)
scssCompiler = compileWith ["-s", "-scss"]
