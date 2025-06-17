{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import qualified Blog


import           Control.Exception     (IOException, catch)
import           Control.Monad.Except
import           Data.List             (sortBy)
import           Data.Ord              (comparing)
import qualified Data.Text.IO          as Text
import qualified Data.Text.IO          as TIO
import qualified Javascript            as JS
import           Options.Applicative
import           Post                  (AppTemplates (..), PostMeta (..))
import qualified Post
import           System.Directory      (createDirectoryIfMissing,
                                        doesDirectoryExist, removeDirectory)
import           System.FilePath       (takeBaseName, (</>))
import           System.FilePath.Posix ((<.>))

data Args a
  = Args
  { target :: a
  , output :: a
  , jsDir  :: a
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

mapLeft :: (t -> a) -> Either t b -> Either a b
mapLeft f = \case
  Left e -> Left (f e)
  Right x -> Right x


mainLogic :: ExceptT Post.Error IO ()
mainLogic = do
  cliArgs <- liftIO $ execParser opts
  posts <- ExceptT $ Post.getPosts (target cliArgs)

  -- Fetch templates concurrently if you want
  postTemplate <- liftIO $ Post.fetchTemplate PostTemplate
  tagTemplate <- liftIO $ Post.fetchTemplate TagTemplate

  -- Render all posts, collecting any errors
  renderedPosts <- ExceptT (sequence <$> traverse (Post.renderPost postTemplate) posts)

  let Args{output = outputBase, jsDir} = cliArgs
      blog = Blog.fromPosts posts
      previews = map JS.fromPost posts
      outputArticles = outputBase </> "articles"
      outputTags = outputBase </> "tags"
      orderedPosts = map ( withArticlePath outputArticles ) $ sortBy (comparing $ Post.date . Post.meta) renderedPosts

  -- Handle output directory
  liftIO $ forM_ [outputArticles, outputTags] $ \path -> do
    outputExists <- doesDirectoryExist path
    putStrLn $ "Path '" <> path <> "' exists? [" <> show outputExists <> "]"
    when outputExists $ do
      catch @IOException (removeDirectory path) (const $ return ())
    createDirectoryIfMissing True path

  -- Render everything
  liftIO $ do
    Blog.render outputTags tagTemplate blog
    JS.writeJSMetadata jsDir previews
    forM_ orderedPosts writePost

  where
    withArticlePath path post = post { Post.meta = (Post.meta post) { Post.path = path </> takeBaseName (Post.path . Post.meta $ post) <.> "html"}}
    writePost Post.Post{Post.content, Post.meta} = do
      Text.writeFile (Post.path meta) content
      TIO.putStrLn $ "Success: " <> Post.title meta

main :: IO ()
main = do
  result <- runExceptT mainLogic
  case result of
    Left err -> error $ show err
    Right () -> return ()
