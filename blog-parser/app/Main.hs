{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Blog
import Control.Exception (IOException, catch, try)
import Control.Monad (forM_, unless)
import Data.Bifunctor (first)
import Data.Either (fromRight, partitionEithers)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Text.IO as Text
import qualified Javascript as JS
import Options.Applicative
import Post (AppTemplates (..), PostMeta (..))
import qualified Post
import System.Directory (createDirectory, doesDirectoryExist, listDirectory, removeDirectory, removeFile, writable)
import System.FilePath (takeBaseName, takeFileName, (</>))

data Args a
  = Args
  { target :: a
  , output :: a
  , jsDir :: a
  }

args :: Parser (Args FilePath)
args =
  Args
    <$> strOption (long "target" <> short 't' <> help "Target directory")
    <*> strOption (long "output" <> short 'o' <> help "Output directory")
    <*> strOption (long "javascript" <> short 'j' <> help "Directory Javascript metadata should be written to")

desc :: String
desc = "Convert a directory of directories, each of which containing a markdown file blog-post and its assets, into a collection of HTML files"

opts :: ParserInfo (Args FilePath)
opts =
  info
    args
    (fullDesc <> progDesc desc)

main :: IO ()
main = do
  args <- execParser opts
  (_failures, success) <- partitionEithers <$> Post.getPosts (target args)
  postTemplate <- Post.fetchTemplate PostTemplate
  tagTemplate <- Post.fetchTemplate TagTemplate
  renderedPosts <- sequence <$> mapM (Post.renderPost postTemplate) success
  case renderedPosts of
    Left e -> print e
    Right posts ->
      let Args{output, jsDir} = args
          orderedPosts = map (first $ outputPath output) $ sortBy (compare `on` date . fst) posts
          blog = Blog.fromPosts success
       in do
            outputExists <- doesDirectoryExist output
            if outputExists
              then
                catch @IOException (removeDirectory output) (const $ return ())
              else
                createDirectory output

            Blog.render output tagTemplate blog
            JS.writeJSMetadata jsDir (map fst orderedPosts)
            forM_ orderedPosts (uncurry writePost)
 where
  outputPath output post = post{path = output </> takeBaseName (path post) ++ ".html"}
  writePost PostMeta{title, path} content = do
    Text.writeFile path content
    putStrLn $ "Success: " <> show title
