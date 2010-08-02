import Data.List (reverse, sort)
import Control.Arrow ((>>>))
import Control.Monad (liftM, mapM_)

import Text.Hakyll (hakyll, defaultHakyllConfiguration, hakyllWithConfiguration)
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
    directory static "js"
    directory css "css"
    static "favicon.ico"
    
    articlePaths <- liftM (reverse . sort) $ getRecursiveContents "articles"
    let articlePages = map ((>>> postManipulation) . createPage) articlePaths
    
    mapM_ (renderChain [ "templates/article.html"
                       , "templates/default.html"
                       ]) articlePages
    
    let index = createListing "index.html"
                             ["templates/item.html"]
                             (take 10 articlePages)
                             [("title", Left "Logic and programming")]
    
    renderChain ["index.html", "templates/default.html"] index
    
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

postManipulation = renderDate "published" "%B %e, %Y" "Date unknown"
