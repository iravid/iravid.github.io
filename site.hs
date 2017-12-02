{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import           Data.Monoid                ((<>))
import           Hakyll
import           Hakyll.Core.Identifier (toFilePath)
import           Hakyll.Core.Provider.Metadata (parsePage)
import           System.FilePath
import           System.Directory
import           Text.Pandoc.Definition
import           Text.Pandoc (readMarkdown)

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
      compile $ do
        fileBody <- getResourceString
        typechecked <- withItemBody transformWithTut fileBody
        unsafeCompiler $ do
          let body = itemBody typechecked
          createDirectoryIfMissing True "output"
          writeFile ("output" </> (takeBaseName $ toFilePath $ itemIdentifier typechecked) <> ".md") body
        return fileBody

    orgFiles <- makePatternDependency "posts/*.org"
    match "output/*.md" $ rulesExtraDependencies [orgFiles] $ do
        route $ setExtension "html" `composeRoutes` gsubRoute "output" (const "posts")
        compile $ do
          rendered <- pandocCompiler
          postTemplate <- loadAndApplyTemplate "templates/post.html" postCtx rendered
          defaultTemplate <- loadAndApplyTemplate "templates/default.html" postCtx postTemplate
          relativizeUrls defaultTemplate

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "output/*.md"
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

transformWithTut :: String -> Compiler String
transformWithTut contents =
  unixFilter "tutblog" ["--coursier-launcher", "/usr/local/bin/coursier"] contents

orgPathToMdPath :: FilePath -> FilePath
orgPathToMdPath orgPath = "posts" </> "output" </> baseName <> ".md"
  where baseName = takeBaseName orgPath
