{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (id)

import Control.Arrow
import Control.Category (id)
import Control.Monad
import Data.List (sortBy)
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

-- | Sort pages chronologically based on their path. This assumes a
-- @year/month/day[.extension]@ like naming scheme.
--
byPath :: [Page String] -> [Page String]
byPath = reverse . sortBy (comparing $ getField "path")

-- | Filter out unuseful contents
--
getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return $ filter (`notElem` [".", "..", "archive"]) names

-- | Get a list of subdirectories
--
getDirectories :: FilePath -> IO [String]
getDirectories dir = getUsefulContents dir >>=
    filterM (doesDirectoryExist . (</>) dir)

-- | Drop n sections from a route
--
dropPath :: Int -> Routes
dropPath n  = customRoute $ joinPath . drop n . splitPath . toFilePath

main :: IO ()
main = getDirectories "notes" >>= doHakyll

-- | Where the magic happens
--
doHakyll :: [FilePath] -> IO ()
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

-- | Compile a portal page for the class
--
makeClassPortal :: FilePath -> RulesM (Pattern (Page String))
makeClassPortal d = match index $ do
    route   $ dropPath 1 `composeRoutes` setExtension ".html"
    compile $ pageCompiler
        >>> setFieldPageList byPath "templates/noteitem.html" "notes" notes
        >>> arr (changeField "url" dropFileName)
        >>> applyTemplateCompiler "templates/portal.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
    where
        notes = parseGlob $ "notes/" ++ d ++ "/notes/*/*"
        index = parseGlob $ "notes/" ++ d ++ "/index.md"

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
