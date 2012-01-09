{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (id)

import Control.Arrow
import Control.Category (id)
import Control.Monad
import Control.Monad.List
import Data.List (isPrefixOf, sortBy)
import Data.Monoid
import Data.Ord (comparing)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath

import Text.Pandoc (HTMLMathMethod(..), WriterOptions(..), defaultWriterOptions)
import Text.Pandoc.Shared (ObfuscationMethod(..))
import Hakyll

data Portal = Portal String
            | Archive String String

-- | Set up deply command
--
hakyllConf = defaultHakyllConfiguration
    { deployCommand = "rsync --checksum -ave ssh \
                       \_site/* simongmzlj@vodik.local:/srv/http/notes"
    }

getSubDirectories :: FilePath -> IO [FilePath]
getSubDirectories dir = getDirectoryContents dir >>=
    filterM (doesDirectoryExist . (</>) dir)

findArchives :: FilePath -> ListT IO Portal
findArchives path = do
    sec <- ListT $ getSubDirectories path
    guardDotFile sec
    dir <- ListT $ getSubDirectories $ path </> sec
    guardDotFile dir
    return $ Archive sec dir

findPortals :: FilePath -> ListT IO Portal
findPortals path = do
    dir <- ListT $ getSubDirectories path
    guardDotFile dir
    if dir == "archive"
       then findArchives $ path </> dir
       else return $ Portal dir

-- | Guard against anything prefixed with a dot. Filters out previous
-- and current directory plus lets us hide folders.
--
guardDotFile :: (MonadPlus m) => FilePath -> m ()
guardDotFile = guard . not . isPrefixOf "."

main :: IO ()
main = runListT (findPortals "notes") >>= doHakyll

-- | Where the magic happens
--
doHakyll :: [Portal] -> IO ()
doHakyll dirs = hakyllWith hakyllConf $ do
    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Compile all my class notes
    match "notes/*/notes/*/*" $ do
        route   $ dropPath 1 `composeRoutes` setExtension ".html"
        compile $ notesCompiler
            >>> applyTemplateCompiler "templates/note.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Compile the actuall class portals
    forM_ dirs makeClassPortal

    -- Build index
    match "index.html"  $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> setFieldPageList chronological "templates/class.html" "classes" "notes/*/*"
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

-- | Sort pages chronologically based on their path. This assumes a
-- @year/month/day[.extension]@ like naming scheme.
--
byPath :: [Page String] -> [Page String]
byPath = reverse . sortBy (comparing $ getField "path")

-- | Drop n sections from a route
--
dropPath :: Int -> Routes
dropPath n = customRoute $ joinPath . drop n . splitPath . toFilePath

-- | Compile a portal page for the class
--
makeClassPortal :: Portal -> RulesM (Pattern (Page String))
makeClassPortal (Portal d) =
    let notes = parseGlob $ "notes" </> d </> "notes/*/*"
        index = parseGlob $ "notes" </> d </> "index.md"
    in makeClassPortal' notes index
makeClassPortal (Archive s d) =
    let notes = parseGlob $ "notes/archive" </> s </> d </> "notes/*/*"
        index = parseGlob $ "notes/archive" </> s </> d </> "index.md"
    in makeClassPortal' notes index

makeClassPortal' :: Pattern (Page String) -> Pattern (Page String) -> RulesM (Pattern (Page String))
makeClassPortal' index notes = match index $ do
    route   $ dropPath 1 `composeRoutes` setExtension ".html"
    compile $ pageCompiler
        >>> setFieldPageList byPath "templates/noteitem.html" "notes" notes
        >>> arr (changeField "url" dropFileName)
        >>> applyTemplateCompiler "templates/portal.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

-- | Override the default compiler because we want to use MathJax
--
notesCompiler :: Compiler Resource (Page String)
notesCompiler = pageCompilerWith defaultHakyllParserState notesWriterOptions

-- | WriterOptions with MathJax for math rendering set
--
notesWriterOptions :: WriterOptions
notesWriterOptions = defaultWriterOptions
    { writerEmailObfuscation = NoObfuscation
    , writerHTMLMathMethod   = MathJax ""
    }
