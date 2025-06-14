{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
module Javascript (writeJSMetadata, Preview, fromPost) where

import           Data.Aeson      (ToJSON, encodeFile)
import           Data.Foldable   (toList)
import           Data.Text       (Text)
import           Data.Time       (Day)
import           GHC.Generics    (Generic)
import           Post            (Post)
import qualified Post            as Posts
import           System.FilePath ((</>))

data Preview
  = Preview
  { title   :: Text
  , date    :: Day
  , slug    :: Text
  , tags    :: [Text]
  , content :: Text
  } deriving (Show, Generic)

instance ToJSON Preview

fromPost :: Post -> Preview
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
writeJSMetadata jsDir = encodeFile $ jsDir </> "blog-previews.json"
