module Hakyll.Web.CompileSass
( sassCompiler
, scssCompiler
) where

import Hakyll

compileWith :: [String] -> Compiler (Item String)
compileWith flags =
	getResourceString
		>>= withItemBody (unixFilter "sass" flags)
		>>= return . fmap compressCss

sassCompiler :: Compiler (Item String)
sassCompiler = compileWith ["-s"]

scssCompiler :: Compiler (Item String)
scssCompiler = compileWith ["-s", "-scss"]
