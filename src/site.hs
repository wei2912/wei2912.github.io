{-# LANGUAGE OverloadedStrings #-}
import Data.Char
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid (mappend)
import Hakyll
import Hakyll.Web.CompileSass (sassCompiler)

main :: IO ()
main = hakyll $ do
    match "css/**.sass" $ do
        route $ setExtension "css"
        compile sassCompiler

    match "posts/**.md" $ do
        route $ setExtension ".html"
        compile $ pandocCompiler
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
    defaultContext
