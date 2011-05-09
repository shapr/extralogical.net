{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Control.Monad (forM_)
import Data.Monoid (mappend, mempty, mconcat)

import System.FilePath
import Text.Pandoc (HTMLMathMethod(..), WriterOptions(..), defaultWriterOptions)
import Text.Pandoc.Shared (ObfuscationMethod(..))

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
        compile $ articleCompiler
            >>> arr pageTitle
            >>> arr formatDate
            >>> applyTemplateCompiler "templates/article.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
    
    -- Plain text versions of articles
    group "raw" $ do
        match "articles/*" $ do
            route   $ routeArticleRaw
            compile $ readPageCompiler
                >>> addDefaultFields
                >>> arr formatDate
                >>> arr (markdownH1 "rawTitle")
                >>> arr (htmlUrl "articleUrl")
                >>> applyTemplateCompiler "templates/raw.txt"
    
    -- Home page
    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "pageTitle" "Extralogical")
        >>> requireAllA ("articles/*" `mappend` inGroup Nothing) (id *** arr (newest 10) >>> addArticles)
        >>> applyTemplateCompiler "templates/home.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
    
    -- Articles listing
    match  "articles.html" $ route routePage
    create "articles.html" $ constA mempty
        >>> arr (setField "title" "Articles")
        >>> arr pageTitle
        >>> requireAllA ("articles/*" `mappend` inGroup Nothing) addArticleListing
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
          , "about/pkd.md"
          ] $ \page -> do
        match page $ do
            route   $ routePage
            compile $ articleCompiler
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
        requireAll_ ("articles/*" `mappend` inGroup Nothing) >>> renderAtom feedConfiguration
    
    -- Fin
    return ()

-- | Read a page, add default fields, substitute fields and render with Pandoc.
--
articleCompiler :: Compiler Resource (Page String)
articleCompiler = pageCompilerWith defaultHakyllParserState articleWriterOptions

-- | Pandoc writer options for articles on Extralogical.
--
articleWriterOptions :: WriterOptions
articleWriterOptions = defaultWriterOptions
    { writerEmailObfuscation = NoObfuscation
    , writerHTMLMathMethod   = MathML Nothing
    , writerLiterateHaskell  = True
    }

-- | Add some articles to a page, applying a template that shows an abbreviated
-- version of each article.
--
addArticles :: Compiler (Page String, [Page String]) (Page String)
addArticles = addPageList "articles" "templates/short.html"

-- | Add an articles listing to a page.
--
addArticleListing :: Compiler (Page String, [Page String]) (Page String)
addArticleListing = addPageList "articles" "templates/item.html"

-- | Add a page listing to a page under the given field, applying the template
-- to each listed page.
--
addPageList :: String -> Identifier -> Compiler (Page String, [Page String]) (Page String)
addPageList field template = setFieldA field $
    arr (reverse . sortByBaseName)
        >>> require template (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

-- | Take a page like @\"/about/notebooks.md\"@ and route it to
-- @\"/about/notebooks\"@, i.e. turn a filename into a drectory.
--
routePage :: Routes
routePage = customRoute fileToDirectory

-- | Drop the date and set the file extension to ".html" when routing articles.
--
routeArticle :: Routes
routeArticle = routeArticleExt ".html"

-- | Drop the date and set the file extension to ".raw" when routing the raw
-- versions of articles.
--
routeArticleRaw :: Routes
routeArticleRaw = routeArticleExt ".txt"

-- | Article routing with a specific file extension.
--
routeArticleExt :: String -> Routes
routeArticleExt ext = customRoute (flip replaceExtension ext . dropDate)

-- | Turn an @Identifier@ into a @FilePath@, dropping the date prefix (e.g.
-- @\"2011-04-07-\"@) along the way.
dropDate :: Identifier -> FilePath
dropDate ident = let file = toFilePath ident
                 in  replaceFileName file (drop 11 $ takeFileName file)

-- | Turn a filename reference into a directory with an index file.
--
fileToDirectory :: Identifier -> FilePath
fileToDirectory = flip combine "index.html" . dropExtension . toFilePath

-- | Extralogical date formatting.
--
formatDate :: Page a -> Page a
formatDate = renderDateField "published" "%B %e, %Y" "Date unknown"

-- | Prefix page titles with "Extralogical: "
--
pageTitle :: Page a -> Page a
pageTitle = renderField "title" "pageTitle" ("Extralogical: " ++)

markdownH1 :: String -> Page a -> Page a
markdownH1 field page = setField field md page
  where
    title = getField "title" page
    line  = replicate (length title) '='
    md    = init $ unlines [title, line]

htmlUrl :: String -> Page a -> Page a
htmlUrl field page = setField field url page
  where
    url = replaceExtension (getField "url" page) ".html"

-- | Take the most recent n articles.
--
newest :: Int -> [Page a] -> [Page a]
newest n = take n . reverse . sortByBaseName

-- | Extralogical feed metadata.
--
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Extralogical"
    , feedDescription = "Logic and programming articles"
    , feedAuthorName  = "Benedict Eastaugh"
    , feedRoot        = "http://extralogical.net"
    }
