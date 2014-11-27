module Hakyll.Web.CompileSass where

import Hakyll

compilerWithFlags :: [String] -> Compiler (Item String)
compilerWithFlags flags =
	getResourceString
		>>= withItemBody (unixFilter "sass" flags)
		>>= return . fmap compressCss

sassCompiler :: Compiler (Item String)
sassCompiler = compilerWithFlags ["-s"]

scssCompiler :: Compiler (Item String)
scssCompiler = compilerWithFlags ["-s", "-scss"]
