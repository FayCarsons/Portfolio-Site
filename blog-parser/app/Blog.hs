{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Blog (Blog (..), fromPosts, render) where

import Control.Monad (forM_)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Post hiding (content, preview, tags)
import qualified Post
import System.FilePath ((</>))
import Text.Pandoc (Template, runIOorExplode)
import Text.Pandoc.Builder (Meta (..), MetaValue (..), ToMetaValue (..))
import Text.Pandoc.Definition (Pandoc (..))
import Text.Pandoc.Extensions (Extension (..), enableExtension, pandocExtensions)
import Text.Pandoc.Options (def, writerExtensions, writerTemplate)
import Text.Pandoc.Writers.HTML (writeHtml5String)

data Blog
  = Blog
  { categories :: !(Map Text (Set PostMeta))
  , tags :: !(Map Text (Set PostMeta))
  , content :: !(Map Text Text)
  }

empty :: Blog
empty =
  Blog
    Map.empty
    Map.empty
    Map.empty

{- Properly group tags and categories for rendering pls :3 -}
fromPosts :: [Post] -> Blog
fromPosts = foldl go empty
 where
  go Blog{categories, tags, content} post =
    let title = Post.title meta
        meta = Post.meta post
        preview = Post.preview post
     in Blog
          { categories = groupCats categories meta
          , tags = groupTags tags meta
          , content = Map.insert title preview content
          }
  insertTag meta tagMap tag = Map.insertWith Set.union tag (Set.singleton meta) tagMap
  groupTags tags meta = Set.foldl (insertTag meta) tags (Post.tags meta)
  groupCats cats meta = Map.insertWith Set.union (Post.category meta) (Set.singleton meta) cats

-- TODO: This needs to handle tag and category pages differently, because each
-- should look slightly different
render :: FilePath -> Template Text -> Blog -> IO ()
render outputPath tagTemplate Blog{categories, tags, content} =
  renderTemplate "Posts tagged: " tags
    *> renderTemplate "Category: " categories
 where
  renderTemplate :: Text -> Map Text (Set PostMeta) -> IO ()
  renderTemplate tagOrCategory m =
    forM_ (Map.toList m) $ \(tag, ps) -> do
      let
        postsAsMeta = MetaList $ map (withPreview content) $ Set.toAscList ps
        meta =
          Meta $
            Map.fromList
              [ ("tag", toMetaValue tag)
              , ("postCount", toMetaValue $ show $ length ps)
              , ("tagOrCategory", toMetaValue tagOrCategory)
              , ("posts", postsAsMeta)
              , ("year", toMetaValue @Text "2024")
              ]
        fmtPath path = reverse $ Text.foldl (flip (:)) "" $ path <> ".html"
      rendered <- runIOorExplode $ writeHtml5String writerOpts (Pandoc meta [])
      TIO.writeFile (outputPath </> fmtPath tag) rendered
  withPreview contentMap post =
    case Map.lookup (Post.title post) contentMap of
      Just preview ->
        MetaMap $ Map.insert "preview" (toMetaValue preview) $ unMetaMap $ toMetaValue post
      Nothing -> error $ "Post <" <> show (Post.title post) <> "> doesn't have a preview"
  unMetaMap (MetaMap m) = m
  unMetaMap _ = error "Couldn't unwrap metaMap"
  writerOpts =
    def
      { writerExtensions = enableExtension Ext_raw_html pandocExtensions
      , writerTemplate = Just tagTemplate
      }
