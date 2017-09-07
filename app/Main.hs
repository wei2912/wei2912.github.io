{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Data.Char
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid (mappend)
import qualified Data.Set as S
import Development.GitRev
import Hakyll
import Hakyll.Web.CompileSass (sassCompiler)
import Text.Pandoc.Options

shortHash :: String
shortHash = take 6 $(gitHash)

main :: IO ()
main = hakyll $ do
    match "css/**.sass" $ do
        route $ setExtension $ concat ["min.", shortHash, ".css"]
        compile sassCompiler

    match "posts/**.md" $ do
        route $ setExtension ".html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<<
                loadAllSnapshots "posts/**.md" "content"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "hash" shortHash `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/**" $ compile templateCompiler

    match (fromRegex "^posts/" .&&. complement (fromRegex "\\.md$")) $ do
        route idRoute
        compile copyFileCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    constField "hash" shortHash `mappend`
    defaultContext

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
    let mathExtensions =
            [ Ext_tex_math_dollars
            , Ext_tex_math_double_backslash
            , Ext_latex_macros
            ]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr enableExtension defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions
            { writerExtensions = newExtensions
            , writerHTMLMathMethod = MathJax ""
            }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

