{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Javascript (writeJSMetadata, Preview, fromPost) where

import           Control.Exception (SomeException)
import           Control.Monad     (when)
import           Data.Aeson        (ToJSON, encodeFile)
import           Data.Foldable     (toList)
import           Data.Text         (Text)
import           Data.Time         (Day)
import           GHC.Generics      (Generic)
import           GHC.IO            (catch)
import           Post              (Post)
import qualified Post              as Posts
import           System.Directory  (createDirectoryIfMissing, doesFileExist,
                                    removeFile)
import           System.FilePath   ((<.>), (</>))
import           Text.Pandoc       (Block)

data Preview
  = Preview
  { title   :: Text
  , date    :: Day
  , slug    :: Text
  , tags    :: [Text]
  , content :: Text
  } deriving (Show, Generic)

instance ToJSON Preview

fromPost :: Post [Block] -> Preview
fromPost post =
  Preview
    (Posts.title meta)
    (Posts.date meta)
    (Posts.slug meta)
    (toList $ Posts.tags meta)
    preview
  where
    meta = Posts.meta post
    preview = Posts.preview post

writeJSMetadata :: FilePath -> [Preview] -> IO ()
writeJSMetadata jsDir previews =
  createDirectoryIfMissing True jsDir
    >> deleteIfExists dir
    >> encodeFile dir previews
  `catch` onFailure
  where
  dir = jsDir </> "blogs" <.> "json"
  deleteIfExists filePath = do
    exists <- doesFileExist filePath
    when exists $ removeFile filePath
  onFailure :: SomeException -> IO ()
  onFailure e = putStrLn $ "Failed to write JSON metadata: " <> show e
