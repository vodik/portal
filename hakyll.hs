{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (id)
import Debug.Trace

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
            | Archive String
    deriving (Show)

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
    dir <- ListT $ getSubDirectories $ path
    guardDotFile dir
    return $ Archive dir

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
    -- Images and static files, compress css, process templates
    [ "favicon.ico" ] --> copy
    [ "images/**" ]   --> copy
    [ "static/**" ]   --> copy
    [ "js/**" ]       --> copy
    [ "css/*" ]       --> css
    [ "templates/*" ] --> templates

    -- Compile all my class notes
    match "notes/*/notes/*/*" $ do
        route   $ dropPath 1 `composeRoutes` setExtension ".html"
        compile $ notesCompiler
            >>> applyTemplateCompiler "templates/note.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    match "notes/archive/*/notes/*/*" $ do
        route   $ dropPath 2 `composeRoutes` setExtension ".html"
        compile $ notesCompiler
            >>> applyTemplateCompiler "templates/note.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Generate the actuall class portals
    forM_ dirs makeClassPortal

    -- Build index
    match "index.html"  $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> setFieldPageList chronological "templates/class.html" "classes" "notes/*/*"
        >>> setFieldPageList chronological "templates/class.html" "archived" "notes/archive/*/*"
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

  where
    -- Useful combinator here
    xs --> f = mapM_ (\p -> match p $ f) xs

    copy = route idRoute >> compile copyFileCompiler
    css  = route idRoute >> compile compressCssCompiler
    templates = compile templateCompiler

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
    let index = parseGlob $ "notes" </> d </> "index.md"
        notes = parseGlob $ "notes" </> d </> "notes/*/*"
    in makeClassPortal' index notes 1 "templates/portal.html"
makeClassPortal (Archive d) =
    let index = parseGlob $ "notes/archive" </> d </> "index.md"
        notes = parseGlob $ "notes/archive" </> d </> "notes/*/*"
    in makeClassPortal' index notes 2 "templates/archive.html"

makeClassPortal' :: Pattern (Page String) -> Pattern (Page String) -> Int -> Identifier Template -> RulesM (Pattern (Page String))
makeClassPortal' index notes l template = match index $ do
    route   $ dropPath l `composeRoutes` setExtension ".html"
    compile $ pageCompiler
        >>> setFieldPageList byPath "templates/noteitem.html" "notes" notes
        >>> arr (changeField "url" dropFileName)
        >>> applyTemplateCompiler template
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
