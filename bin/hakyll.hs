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
import Text.Hamlet (HamletSettings(..), defaultHamletSettings)
import Text.Hamlet --(HamletSettings(..), defaultHamletSettings)
import Hakyll

-- | Set up deply command
--
hakyllConfig = defaultHakyllConfiguration
    { deployCommand = "rsync --checksum -ave ssh \
                       \_site/* simongmzlj@vodik.local:/srv/http/notes"
    }

-- | Get all subdirectories of a directory
--
getSubDirectories :: FilePath -> ListT IO FilePath
getSubDirectories path = do
    directory <- ListT $ getDirectoryContents path
    guard . not $ isPrefixOf "." directory
    subdir <- lift . doesDirectoryExist $ path </> directory
    guard subdir
    return $ path </> directory

main :: IO ()
main = do
    c <- runListT $ getSubDirectories "classes"
    a <- runListT $ getSubDirectories "archive"
    doHakyll c a

-- | Where the magic happens
--
doHakyll :: [FilePath] -> [FilePath] -> IO ()
doHakyll classes archived = hakyllWith hakyllConfig $ do
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
            >>> applyTemplateCompiler "templates/note.hamlet"
            >>> applyTemplateCompiler "templates/default.hamlet"
            >>> relativizeUrlsCompiler

    -- Generate the actuall class portals
    forM_ classes  $ makeClassPortal "templates/portal.hamlet"
    forM_ archived $ makeClassPortal "templates/archive.hamlet"

    -- Build main index
    match  "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> setFieldPageList chronological "templates/class.hamlet" "classes"  "classes/*/*"
        >>> setFieldPageList chronological "templates/class.hamlet" "archived" "archive/*/*"
        >>> applyTemplateCompiler "templates/index.hamlet"
        >>> applyTemplateCompiler "templates/default.hamlet"
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
makeClassPortal template d = match index $ do
    route   $ dropPath 1 `composeRoutes` setExtension ".html"
    compile $ pageCompiler
        >>> setFieldPageList byPath "templates/noteitem.hamlet" "notes" notes
        >>> arr (changeField "url" dropFileName)
        >>> applyTemplateCompiler template
        >>> applyTemplateCompiler "templates/default.hamlet"
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
