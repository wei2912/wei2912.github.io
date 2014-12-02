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

    match "_posts/**" $ do
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "_templates/post.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "_posts/**"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "_templates/default.html" indexCtx
                >>= relativizeUrls

    match "_templates/**" $ compile templateCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx =
                    postCtx `mappend`
                    bodyField "description"

            posts <- fmap (take 10) . recentFirst =<< loadAll "_posts/**"
            renderAtom feedConfig feedCtx posts

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    field "id" getPostID `mappend`
    field "url" setNewURL `mappend`
    defaultContext
    where
    	title2ID :: String -> String
    	title2ID = map (toLower . \ x -> if x == ' ' then '_' else x)
        getPostID :: Item a -> Compiler String
        getPostID item = do
            metadata <- getMetadata $ itemIdentifier item
            let title = fromJust $ M.lookup "title" metadata
            return $ title2ID title
        setNewURL :: Item a -> Compiler String
        setNewURL item = do
        	metadata <- getMetadata $ itemIdentifier item
        	let title = fromJust $ M.lookup "title" metadata
        	return $ '#' : title2ID title

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "wei2912’s blog"
    , feedDescription = "A website where I throw in some stuff which may be useful to others."
    , feedAuthorName  = "Wei En (wei2912)"
    , feedAuthorEmail = "wei2912.supp0rt@gmail.com"
    , feedRoot        = "https://wei2912.github.io"
    }
