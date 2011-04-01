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
    -- Copy images
    route   "images/**" idRoute
    compile "images/**" copyFileCompiler
    
    -- Copy files
    route   "files/*" idRoute
    compile "files/*" copyFileCompiler
    
    -- Copy favicon
    route   "favicon.ico" idRoute
    compile "favicon.ico" copyFileCompiler
    
    -- Copy robots file
    route   "robots.txt" idRoute
    compile "robots.txt" copyFileCompiler
    
    -- Copy JavaScript
    route   "js/*" idRoute
    compile "js/*" copyFileCompiler
    
    -- Compress CSS
    route   "css/*" idRoute
    compile "css/*" compressCssCompiler
    
    -- Read templates
    compile "templates/*" templateCompiler
    
    -- Render articles
    route   "articles/*" $ routePage
    compile "articles/*" $ pageCompiler
        >>> arr pageTitle
        >>> arr formatDate
        >>> applyTemplateCompiler "templates/article.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
    
    -- Render home page
    route  "index.html" $ idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "pageTitle" "Extralogical")
        >>> requireAllA "articles/*" (id *** arr (newest 10) >>> addArticlesList)
        >>> applyTemplateCompiler "templates/home.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
    
    -- Articles listing
    route  "articles.html" $ routePage
    create "articles.html" $ constA mempty
        >>> arr (setField "title" "Articles")
        >>> arr pageTitle
        >>> requireAllA "articles/*" addFullArticleListing
        >>> applyTemplateCompiler "templates/articles.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
    
    -- Render site pages
    forM_ [ "projects/hatt.md"
          , "projects/mobile.md"
          , "projects/papertrail.md"
          , "archives.md"
          , "about.md"
          , "about/coffee.md"
          , "about/notebooks.md"
          ] $ \page -> do
        route   page $ routePage
        compile page $ pageCompiler
            >>> arr pageTitle
            >>> applyTemplateCompiler "templates/page.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
    
    -- Render HTML pages
    route   "projects.html" $ routePage
    compile "projects.html" $ readPageCompiler
        >>> addDefaultFields
        >>> arr applySelf
        >>> arr pageTitle
        >>> applyTemplateCompiler "templates/page.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
    
    -- Render Atom feed
    route  "articles.atom" $ idRoute
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
        >>> arr (map stripIndexLink)
        >>> require template (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

routePage :: Routes
routePage = customRoute fileToDirectory

stripIndexLink :: Page a -> Page a
stripIndexLink = changeField "url" dropFileName

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
