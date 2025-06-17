{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Blog (Blog (..), fromPosts, render) where

import           Control.Monad            (forM_)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.IO             as TIO
import           Post                     hiding (content, preview, tags)
import qualified Post
import           System.FilePath          ((</>))
import           Text.Pandoc              (Block, Template, runIOorExplode)
import           Text.Pandoc.Builder      (Meta (..), MetaValue (..),
                                           ToMetaValue (..))
import           Text.Pandoc.Definition   (Pandoc (..))
import           Text.Pandoc.Extensions   (Extension (..), enableExtension,
                                           pandocExtensions)
import           Text.Pandoc.Options      (def, writerExtensions,
                                           writerTemplate)
import           Text.Pandoc.Writers.HTML (writeHtml5String)
import qualified Text.Show                as Text

data Blog
  = Blog
  { tags    :: !(Map Text (Set PostMeta))
  , content :: !(Map Text Text)
  }

empty :: Blog
empty =
  Blog
    Map.empty
    Map.empty

fromPosts :: [Post [Block]] -> Blog
fromPosts = foldl go empty
 where
  go Blog{tags, content} post =
    let title = Post.title meta
        meta = Post.meta post
        preview = Post.preview post
     in Blog
          { tags = groupTags tags meta
          , content = Map.insert title preview content
          }
  insertTag meta tagMap tag = Map.insertWith Set.union tag (Set.singleton meta) tagMap
  groupTags tags meta = Set.foldl (insertTag meta) tags (Post.tags meta)

render :: FilePath -> Template Text -> Blog -> IO ()
render outputPath tagTemplate Blog{tags, content} = renderTemplate tags
 where
  renderTemplate :: Map Text (Set PostMeta) -> IO ()
  renderTemplate m =
    forM_ (Map.toList m) $ \(tag, ps) -> do
      let
        postsAsMeta = MetaList $ map (withPreview content) $ Set.toAscList ps
        meta =
          Meta $
            Map.fromList
              [ ("tag", toMetaValue tag)
              , ("postCount", toMetaValue $ show $ length ps)
              , ("posts", postsAsMeta)
              , ("year", toMetaValue @Text "2024")
              ]
        fmtPath path = filter (/= '\"') $ Text.show $ path <> ".html"
      rendered <- runIOorExplode $ writeHtml5String writerOpts (Pandoc meta [])
      TIO.writeFile (outputPath </> fmtPath tag) rendered
  withPreview contentMap post =
    case Map.lookup (Post.title post) contentMap of
      Just preview ->
        MetaMap $ Map.insert "preview" (toMetaValue preview) $ unMetaMap $ toMetaValue post
      Nothing -> error $ "Post '" <> show (Post.title post) <> "' doesn't have a preview"
  unMetaMap (MetaMap m) = m
  unMetaMap _           = error "Couldn't unwrap metaMap"
  writerOpts =
    def
      { writerExtensions = enableExtension Ext_raw_html pandocExtensions
      , writerTemplate = Just tagTemplate
      }
