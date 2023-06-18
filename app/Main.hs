{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Crypto.Hash (Digest, MD5, hashlazy)
import Data.Binary (encode)
import Data.Char
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Map as M
import qualified Data.Set as S
import Hakyll
import Text.Pandoc.Options
import Text.Pandoc.Templates
import Text.Pandoc.SideNote (usingSideNotes)

main :: IO ()
main = hakyll $ do
    (match . fromList) ["favicon.ico", "favicon-16x16.png",
        "favicon-32x32.png", "favicon.svg"] $ do
        route   idRoute
        compile copyFileCompiler

    match "public/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**.scss" $ do
        route   $ setExtension ".min.css"
        compile $ getResourceString >>=
            withItemBody (unixFilter "sass" ["--stdin", "--style=compressed"])

    epoch <- preprocess $ nominalDiffTimeToSeconds <$> getPOSIXTime
    let timeHash = take 6 $ show (hashlazy (encode epoch) :: Digest MD5)
    let defaultCtxWithTimeHash =
            constField "timehash" timeHash <>
            defaultContext
    let postCtx =
            dateField "date" "%B %e, %Y" <>
            defaultCtxWithTimeHash

    match "posts/**.md" $ do
        route   $ setExtension ".html"
        compile $ do
            underlying <- getUnderlying
            toc <- getMetadataField underlying "toc"
            pandocCustomCompiler (maybe False (\bool -> bool == "true") toc)
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

            pandocCustomCompiler False
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


pandocCustomCompiler :: Bool -> Compiler (Item String)
pandocCustomCompiler withTOC = do
        tmpl <- either (const Nothing) Just <$> unsafeCompiler (
            compileTemplate
            "" $
            "\n$if(toc)$"
            <> "\n<nav id=\"toc\" role=\"doc-toc\">"
            <> "\n<strong>Contents</strong>"
            <> "\n<label for=\"contents\">⊕</label>"
            <> "\n<input type=\"checkbox\" id=\"contents\">"
            <> "\n$toc$"
            <> "\n</nav>"
            <> "\n$endif$"
            <> "\n$body$"
            )

        let defaultExtensions = writerExtensions defaultHakyllWriterOptions
        let mathExtensions =
                [ Ext_tex_math_dollars
                , Ext_tex_math_double_backslash
                , Ext_latex_macros
                ]
        let newExtensions = foldr enableExtension defaultExtensions mathExtensions

        let writerOptions = defaultHakyllWriterOptions
                { writerExtensions = newExtensions
                , writerHTMLMathMethod = MathJax ""
                , writerTableOfContents = withTOC
                , writerTemplate = tmpl
                }

        pandocCompilerWithTransform
            defaultHakyllReaderOptions
            writerOptions
            usingSideNotes
