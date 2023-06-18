{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Crypto.Hash as CH
import Data.Binary
import Data.Char
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Map as M
import qualified Data.Set as S
import Hakyll
import Text.Pandoc.Options

main :: IO ()
main = hakyll $ do
    (match . fromList) ["favicon.ico", "favicon-16x16.png",
        "favicon-32x32.png", "favicon.svg"] $ do
        route   idRoute
        compile copyFileCompiler

    match "public/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**.sass" $ do
        route   $ setExtension ".min.css"
        compile $ getResourceString >>=
            withItemBody (unixFilter "sass" ["--stdin", "--indented",
              "--style=compressed"])

    epoch <- preprocess $ nominalDiffTimeToSeconds <$> getPOSIXTime
    let timeHash = take 6 $ show (CH.hashlazy (encode epoch) :: CH.Digest CH.MD5)

    let defaultCtxWithTimeHash =
            constField "timehash" timeHash <>
            defaultContext

    let postCtx =
            dateField "date" "%B %e, %Y" <>
            defaultCtxWithTimeHash

    match "posts/**.md" $ do
        route   $ setExtension ".html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.md" $ do
        route   $ setExtension ".html"
        compile $ do
            posts <- recentFirst =<<
                loadAllSnapshots "posts/**.md" "content"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultCtxWithTimeHash

            pandocMathCompiler
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/**" $ compile templateCompiler

    match (fromRegex "^posts/" .&&. complement (fromRegex "\\.md$")) $ do
        route   idRoute
        compile copyFileCompiler

    create ["sitemap.xml"] $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<<
                loadAllSnapshots "posts/**.md" "content"
            let sitemapCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultCtxWithTimeHash

            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx


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
