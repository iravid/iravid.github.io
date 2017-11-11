{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import           Control.Monad              (forM_)
import           Control.Monad.Reader       (ask)
import           Data.Monoid                ((<>))
import           Hakyll
import           Hakyll.Core.Identifier (toFilePath)
import           Hakyll.Core.Rules.Internal (Rules (..), rulesMatches)
import           System.Process.Typed       (proc, runProcess_)
import           System.FilePath


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*.org" $ do
      preprocess tutblog

    match "posts/output/*.md" $ do
        route $ (setExtension "html") `composeRoutes` (customRoute $ ("posts" </>) . takeFileName . toFilePath) 
        compile $ do
          pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/output/*.md"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Home"                <>
                    siteCtx                                  <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"          <>
    constField "blogName" "Itamar's Blog" <>
    siteCtx                               <>
    defaultContext

siteCtx :: Context String
siteCtx =
  constField "blogName" "Itamar's Blog" <>
  constField "blogDescription" "Functors, and stuff"

----------------------------

tutblog :: IO ()
tutblog = do
  let tutblogArgs = [ "--org-dir", "./posts"
                    , "--tut-dir", "./posts/tut"
                    , "--md-dir",  "./posts/output"
                    , "--coursier-launcher",  "/Users/iravid/Development/personal/tutblog/coursier"
                    , "--coursier-deps", "org.typelevel::cats:0.9.0,org.typelevel::cats-effect:0.3,com.typesafe.play::play-json:2.5.10,com.iravid::play-json-cats:0.2" ]
  runProcess_ $ proc "tutblog" tutblogArgs
  return ()

orgPathToMdPath :: FilePath -> FilePath
orgPathToMdPath orgPath = "posts" </> "output" </> baseName <> ".md"
  where baseName = takeBaseName orgPath
