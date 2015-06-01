{-# LANGUAGE OverloadedStrings #-}
import Data.Char
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid (mappend)
import Hakyll
import Hakyll.Web.CompileSass (sassCompiler)

main :: IO ()
main = hakyll $ do
    match "res/**" $ do
        route idRoute
        compile copyFileCompiler

    match "css/**.css" $ do
        route idRoute
        compile compressCssCompiler

    match "css/**.sass" $ do
        route $ setExtension "css"
        compile sassCompiler

    match "posts/**" $ do
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
                loadAllSnapshots "posts/**" "content"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/**" $ compile templateCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx =
                    postCtx `mappend`
                    bodyField "description"

            posts <- fmap (take 10) . recentFirst =<< loadAll "posts/**"
            renderAtom feedConfig feedCtx posts

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "wei2912’s blog"
    , feedDescription = "The blog of a high school studet, on math and computer science."
    , feedAuthorName  = "Wei En (wei2912)"
    , feedAuthorEmail = "wei2912.supp0rt@gmail.com"
    , feedRoot        = "https://wei2912.github.io"
    }
