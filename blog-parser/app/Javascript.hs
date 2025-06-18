{-# LANGUAGE DeriveGeneric #-}
module Javascript (writeJSMetadata, Preview, fromPost) where

import           Data.Aeson       (ToJSON, encodeFile)
import           Data.Foldable    (toList)
import           Data.Text        (Text)
import           Data.Time        (Day)
import           GHC.Generics     (Generic)
import           Post             (Post)
import qualified Post             as Posts
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           System.FilePath  (takeDirectory, (<.>), (</>))
import           Text.Pandoc      (Block)

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
writeJSMetadata jsDir previews  = createDirectoryIfMissing True jsDir >> encodeFile dir previews
   where dir = jsDir </> "blogs" <.> "json"
