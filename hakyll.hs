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

-- | Set up deply command
--
hakyllConf = defaultHakyllConfiguration
    { deployCommand = "rsync --checksum -ave ssh \
                       \_site/* simongmzlj@vodik.local:/srv/http/notes"
    }

-- | Get all subdirectories of a directory
--
getSubDirectories :: FilePath -> IO [FilePath]
getSubDirectories dir = getDirectoryContents dir >>=
    filterM (doesDirectoryExist . (</>) dir)

findFolders :: FilePath -> ListT IO FilePath
findFolders path = do
    dir <- ListT $ getSubDirectories path
    guardDotFile dir
    return $ path </> dir

-- | Guard against anything prefixed with a dot. Filters out previous
-- and current directory plus lets us hide folders.
--
guardDotFile :: (MonadPlus m) => FilePath -> m ()
guardDotFile = guard . not . isPrefixOf "."

main :: IO ()
main = do
    c <- runListT $ findFolders "class"
    a <- runListT $ findFolders "archive"
    doHakyll c a

-- | Where the magic happens
--
doHakyll :: [FilePath] -> [FilePath] -> IO ()
doHakyll c a = hakyllWith hakyllConf $ do
    -- Images and static files, compress css, process templates
    [ "favicon.ico" ] --> copy
    [ "images/**" ]   --> copy
    [ "static/**" ]   --> copy
    [ "js/**" ]       --> copy
    [ "css/*" ]       --> css
    [ "templates/*" ] --> templates

    -- Compile all my class notes
    match "*/*/notes/*/*" $ do
        route   $ dropPath 1 `composeRoutes` setExtension ".html"
        compile $ notesCompiler
            >>> applyTemplateCompiler "templates/note.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Generate the actuall class portals
    forM_ c $ makeClassPortal "template/portal.hml"
    forM_ a $ makeClassPortal "template/archive.hml"

    -- Build index
    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> setFieldPageList chronological "templates/class.html" "classes" "notes/*/*"
        >>> setFieldPageList chronological "templates/class.html" "archived" "notes/archive/*/*"
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

  where
    -- Useful combinator here
    xs --> f = mapM_ (`match` f) xs

    copy = route idRoute >> compile copyFileCompiler
    css  = route idRoute >> compile compressCssCompiler
    templates = compile templateCompiler

-- | Sort pages chronologically based on their path. This assumes a
-- @year/month/day[.extension]@ like naming scheme.
--
byPath :: [Page String] -> [Page String]
byPath = reverse . sortBy (comparing $ getField "path")

-- | Drop n sections of a path.
--
dropPath :: Int -> Routes
dropPath n = customRoute $ joinPath . drop n . splitPath . toFilePath

-- | Compile a portal page for the class
--
-- makeClassPortal :: FilePath -> RulesM (Pattern (Page String))
makeClassPortal template d = do
    route   $ dropPath 1 `composeRoutes` setExtension ".html"
    compile $ pageCompiler
        >>> setFieldPageList byPath "templates/noteitem.html" "notes" notes
        >>> arr (changeField "url" dropFileName)
        >>> applyTemplateCompiler template
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
  where
    index = parseGlob $ d </> "index.md"
    notes = parseGlob $ d </> "notes/*/*"

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
