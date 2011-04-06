{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Control.Monad (forM_)
import Data.Monoid (mempty, mconcat)

import System.FilePath

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Images
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler
    
    -- Files
    match "files/**" $ do
        route   idRoute
        compile copyFileCompiler
    
    -- Favicon
    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler
    
    -- Robots file
    match "robots.txt" $ do
        route   idRoute
        compile copyFileCompiler
    
    -- JavaScript
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler
    
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    
    -- Read templates
    match "templates/*" $ compile templateCompiler
    
    -- Articles
    match "articles/*" $ do
        route   $ routeArticle
        compile $ pageCompiler
            >>> arr pageTitle
            >>> arr formatDate
            >>> applyTemplateCompiler "templates/article.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
    
    -- Home page
    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "pageTitle" "Extralogical")
        >>> requireAllA "articles/*" (id *** arr (newest 10) >>> addArticlesList)
        >>> applyTemplateCompiler "templates/home.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
    
    -- Articles listing
    match  "articles.html" $ route routePage
    create "articles.html" $ constA mempty
        >>> arr (setField "title" "Articles")
        >>> arr pageTitle
        >>> requireAllA "articles/*" addFullArticleListing
        >>> applyTemplateCompiler "templates/articles.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
    
    -- Site pages
    forM_ [ "projects/hatt.md"
          , "projects/mobile.md"
          , "projects/papertrail.md"
          , "archives.md"
          , "about.md"
          , "about/coffee.md"
          , "about/notebooks.md"
          ] $ \page -> do
        match page $ do
            route   $ routePage
            compile $ pageCompiler
                >>> arr pageTitle
                >>> applyTemplateCompiler "templates/page.html"
                >>> applyTemplateCompiler "templates/default.html"
                >>> relativizeUrlsCompiler
    
    -- Projects page
    match   "projects.html" $ do
        route   $ routePage
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr applySelf
            >>> arr pageTitle
            >>> applyTemplateCompiler "templates/page.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
    
    -- Atom feed
    match  "articles.atom" $ route idRoute
    create "articles.atom" $
        requireAll_ "articles/*" >>> renderAtom feedConfiguration
    
    -- Fin
    return ()

addArticlesList :: Compiler (Page String, [Page String]) (Page String)
addArticlesList = addPageList "articles" "templates/short.html"

addFullArticleListing :: Compiler (Page String, [Page String]) (Page String)
addFullArticleListing = addPageList "articles" "templates/item.html"

addPageList :: String -> Identifier -> Compiler (Page String, [Page String]) (Page String)
addPageList field template = setFieldA field $
    arr (reverse . sortByBaseName)
        >>> require template (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

routePage :: Routes
routePage = customRoute fileToDirectory

routeArticle :: Routes
routeArticle = customRoute (flip replaceExtension ".html" . dropDate)

dropDate :: Identifier -> FilePath
dropDate ident = let file = toFilePath ident
                 in  replaceFileName file (drop 11 $ takeFileName file)

fileToDirectory :: Identifier -> FilePath
fileToDirectory = flip combine "index.html" . dropExtension . toFilePath

formatDate :: Page a -> Page a
formatDate = renderDateField "published" "%B %e, %Y" "Date unknown"

pageTitle :: Page a -> Page a
pageTitle = renderField "title" "pageTitle" ("Extralogical: " ++)

newest :: Int -> [Page a] -> [Page a]
newest n = take n . reverse . sortByBaseName

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Extralogical"
    , feedDescription = "Logic and programming articles"
    , feedAuthorName  = "Benedict Eastaugh"
    , feedRoot        = "http://extralogical.net"
    }
