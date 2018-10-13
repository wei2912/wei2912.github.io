module Hakyll.Web.CompileSass (scssCompiler) where

import Control.Monad
import Hakyll

compileWith :: [String] -> Compiler (Item String)
compileWith flags =
    liftM (fmap compressCss) (getResourceString
        >>= withItemBody (unixFilter "sass" flags)
    )

scssCompiler :: Compiler (Item String)
scssCompiler = compileWith []
