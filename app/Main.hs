{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import Data.Char
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid (mappend)
import qualified Data.Set as S
import Hakyll
import Hakyll.Web.Sass (sassCompiler)
import Text.Pandoc.Options

main :: IO ()
main = hakyll $ do
    (match . fromList) ["favicon.ico", "favicon-16x16.png", "favicon-32x32.png"] $ do
        route   idRoute
        compile copyFileCompiler

    match "css/**.sass" $ do
        route   $ setExtension ".min.css"
        let compressCssItem = fmap compressCss
        compile (compressCssItem <$> sassCompiler)

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
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

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
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
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

