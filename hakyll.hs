import Data.List (reverse, sort)
import Control.Arrow ((>>>))
import Control.Monad (liftM, mapM_)

import Text.Hakyll (defaultHakyllConfiguration, hakyllWithConfiguration)
import Text.Hakyll.Feed (FeedConfiguration (..), renderAtom)
import Text.Hakyll.Render (renderChain, static, css)
import Text.Hakyll.File (getRecursiveContents, directory)
import Text.Hakyll.CreateContext (createPage, createListing)
import Text.Hakyll.ContextManipulations (renderDate, changeValue)
import Text.Hakyll.HakyllMonad (HakyllConfiguration (..))

exlConfig :: HakyllConfiguration
exlConfig = (defaultHakyllConfiguration "http://extralogical.net")
    { enableIndexUrl = True
    }

main = hakyllWithConfiguration exlConfig $ do
    directory static "files"
    directory static "images"
    directory css "css"
    static "favicon.ico"
    static "robots.txt"
    
    articlePaths <- liftM (reverse . sort) $ getRecursiveContents "articles"
    let articlePages = map ((>>> postManipulation) . createPage) articlePaths
    
    mapM_ (renderChain [ "templates/article.html"
                       , "templates/default.html"
                       ]) articlePages
    
    let index = createListing "index.html"
                             ["templates/item.html"]
                             (take 10 articlePages)
                             [("title", Left "Extralogical")]
    
    renderChain ["index.html", "templates/default.html"] index
    
    renderAtom feedConfiguration $ take 5 articlePages
    
    -- Some static pages.
    mapM_ (renderChain [ "templates/page.html"
                       , "templates/default.html" ] . createPage)
            [ "projects.html"
            , "projects/mobile.md"
            , "projects/papertrail.md"
            , "archives.md"
            , "about.md"
            , "about/notebooks.md"
            ]

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedUrl         = "articles.atom"
    , feedTitle       = "Extralogical"
    , feedDescription = "Logic and programming articles"
    , feedAuthorName  = "Benedict Eastaugh"
    }

postManipulation = renderDate "published" "%B %e, %Y" "Date unknown"
