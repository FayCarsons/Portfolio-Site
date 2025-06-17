{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Post (Post (..), PostMeta (..), AppTemplates (..), getPosts, renderPost, fetchTemplate, preview, Error(..), moveAssets) where

import           Control.Monad                (filterM, (>=>))
import           Data.Either                  (fromRight)
import           Data.Functor                 ((<&>))
import           Data.List                    (isSuffixOf)
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Time                    (Day)
import qualified Data.Time                    as Time
import           System.Directory
import           System.FilePath
-- Paths
import qualified System.FilePath              as Path
import           System.FilePattern.Directory (getDirectoryFiles)
-- Pandoc

import           Data.Aeson                   (ToJSON)
import qualified Data.Char                    as Char
import           Data.Foldable                (forM_)
import           GHC.Generics                 (Generic)
import           Text.Pandoc                  (Block (..), Template,
                                               compileTemplate,
                                               defaultMathJaxURL,
                                               getDefaultExtensions,
                                               getTemplate, nullMeta, runIO,
                                               runPure, writePlain)
import           Text.Pandoc.Builder          (MetaValue (..), ToMetaValue (..))
import           Text.Pandoc.Definition       (Inline (..), Meta (..),
                                               Pandoc (..), lookupMeta)
import           Text.Pandoc.Extensions       (Extension (..), enableExtension,
                                               pandocExtensions)
import           Text.Pandoc.Highlighting     (kate)
import           Text.Pandoc.Options          (HTMLMathMethod (..), def,
                                               readerExtensions,
                                               writerExtensions,
                                               writerHTMLMathMethod,
                                               writerHighlightStyle,
                                               writerTemplate)
import           Text.Pandoc.Readers.Markdown (readMarkdown)
import           Text.Pandoc.Writers.HTML     (writeHtml5String)

data Error
  = MissingFrontmatter FilePath Text
  | ParseError FilePath Text
  | WriteError FilePath Text
  | RenderError Text Text
  | PostNotFound FilePath
  | TooManyMDFiles FilePath [FilePath]
  | Template Text

instance Show Error where
  show (MissingFrontmatter path field) = "[Blog parser]: missing metadata field [" <> Text.unpack field <> "] in file <" <> show path <> ">"
  show (ParseError path cause) = "[Blog parser]: invalid markdown in file <" <> show path <> "> - " <> Text.unpack cause
  show (WriteError path cause) = "[Blog parser]: cannot write file <" <> show path <> "> to HTML - " <> Text.unpack cause
  show (PostNotFound path) = "[Blog parser]: blog post not found in directory <" <> show path <> ">"
  show (TooManyMDFiles dir files) = "[Blog parser]: Cannot decide which '.md' to use for blog <" <> show dir <> "> - found files:\n" <> show files
  show (RenderError post err) = "[Blog parser]: Error rendering template for post <" <> show post <> ">: " <> show err
  show (Template cause) = "[Blog parser]: Cannot create template: " <> show cause

toEither :: Error -> Maybe a -> Either Error a
toEither _ (Just x)  = Right x
toEither err Nothing = Left err

lookupEither :: Error -> Text -> Meta -> Either Error MetaValue
lookupEither err key meta = toEither err $ lookupMeta key meta

data Post a
  = Post
  { content :: a,
    meta    :: PostMeta
  }
  deriving (Show)

data PostMeta
  = PostMeta
  { title :: Text,
    tags  :: Set Text,
    date  :: Day,
    slug  :: Text,
    path  :: FilePath
  }
  deriving (Show, Eq, Generic)

instance ToJSON PostMeta

instance Ord PostMeta where
  compare a b = compare (date a) (date b)

instance ToMetaValue PostMeta where
  toMetaValue Post.PostMeta {Post.title, Post.tags, Post.date, Post.slug, Post.path} =
    MetaMap $
      Map.fromList
        [ ("title", toMetaValue title),
          ("tags", MetaList $ map toMetaValue $ Set.toList tags),
          ("date", toMetaValue $ show date),
          ("slug", toMetaValue slug),
          ("path", toMetaValue path)
        ]

preview :: Post [Block] -> Text
preview Post{content} =
  case content of
    openingParagraph : _ ->
      (<>) "..."
        $ fromRight (error "impossible!")
        $ runPure
        $ writePlain def (Pandoc nullMeta [openingParagraph])
    _ -> ""

plainText :: MetaValue -> Text
plainText (MetaString s) = s
plainText (MetaInlines xs) = Text.concat $ map fromInline xs
  where
    fromInline :: Inline -> Text
    fromInline (Str s) = s
    fromInline Space   = " "
    fromInline _       = ""
plainText _ = ""

parseDate :: FilePath -> Text -> Either Error Day
parseDate path date =
  case Time.parseTimeM True Time.defaultTimeLocale "%m-%d-%Y" (Text.unpack date) of
    Nothing  -> Left $ ParseError path "Failed to parse date"
    Just day -> Right day

mapLeft :: forall a b c. (a -> b) -> Either a c -> Either b c
mapLeft f (Left e)  = Left $ f e
mapLeft _ (Right x) = Right x

parseMeta :: FilePath -> Meta -> [Block] -> Either Error (Post [Block])
parseMeta path meta content = do
  title <- plainText <$> lookupEither (MissingFrontmatter path "No title") "title" meta
  date <- lookupEither (MissingFrontmatter path "No date") "date" meta >>= parseDate path . plainText
  tags <- Set.fromList . map Text.strip . Text.splitOn "," . plainText <$> lookupEither (MissingFrontmatter path "No tags") "tags" meta
  return $
    Post
      { content,
        meta =
          PostMeta {tags, date, path, title, slug}
      }
  where
    slug = Text.pack $ Path.takeBaseName path

parsePost :: FilePath -> IO (Either Error (Post [Block]))
parsePost path = do
  content <- runIO . readMarkdown readerOpts . Text.pack =<< readFile path
  case content of
    Left e -> return $ Left $ ParseError path (Text.pack $ show e)
    Right (Pandoc meta blocks) -> return $ parseMeta path meta blocks
  where
    readerOpts =
      def { readerExtensions =
              enableExtension Ext_yaml_metadata_block
                $ enableExtension Ext_fenced_code_blocks
                $ enableExtension Ext_fenced_code_attributes
                $ getDefaultExtensions "markdown"
          }

findPost :: FilePath -> IO (Either Error FilePath)
findPost dir = do
  post <- getDirectoryFiles dir ["**/*.md"]
  return $ case post of
    []            -> Left $ PostNotFound dir
    [p]           -> Right $ dir </> p
    markdownFiles -> Left $ TooManyMDFiles dir markdownFiles

moveAssets :: FilePath -> FilePath -> IO ()
moveAssets dir targetDir = do
  -- Create target directory if it doesn't exist
  createDirectoryIfMissing True targetDir

  -- Get all files in the source directory
  files <- listDirectory dir

  -- Filter to only include files (not directories) and exclude markdown files
  assetFiles <- filterM isAssetFile files

  -- Copy each asset file to the target directory
  forM_ assetFiles $ \file -> do
    let sourcePath = dir </> file
    let targetPath = targetDir </> file
    copyFile sourcePath targetPath
    putStrLn $ "Copied: " ++ file

  where
    isAssetFile :: FilePath -> IO Bool
    isAssetFile file = do
      let fullPath = dir </> file
      isFile <- doesFileExist fullPath
      let isMarkdown = any (`isSuffixOf` map Char.toLower file) [".md", ".markdown"]
      return (isFile && not isMarkdown)

getPosts :: FilePath -> IO (Either Error [Post [Block]])
getPosts target = fmap sequence
  $ listDirectory target
      >>= filterM doesDirectoryExist . map (target </>)
      >>= mapM (findPost >=> either (return . Left) parsePost)


data AppTemplates
  = PostTemplate
  | TagTemplate

fetchTemplate :: AppTemplates -> IO (Template Text)
fetchTemplate whichTemplate = do
  let templatePath = case whichTemplate of
        PostTemplate -> "./blog-parser/post.pandoc"
        TagTemplate  -> "./blog-parser/tag.pandoc"
  templateText <- fromRight (error $ "Cannot read '" <> show templatePath <> "': ") <$> runIO (getTemplate templatePath)
  fromRight (error "Cannot compile template: ") <$> compileTemplate "" templateText

renderPost :: Template Text -> Post [Block] -> IO (Either Error (Post Text))
renderPost template post =
  let write = writeHtml5String writerOpts (Pandoc (toMeta post) (content post))
   in fmap withRendered <$> runIO write <&> mapLeft (WriteError (path $ meta post) . Text.pack . show)
  where
    withRendered content  = post { content }
    writerOpts =
      def
        { writerExtensions = enableExtension Ext_raw_html pandocExtensions,
          writerHTMLMathMethod = MathJax defaultMathJaxURL,
          writerHighlightStyle = Just kate,
          writerTemplate = Just template
        }
    toMeta Post {meta} =
      Meta $
        Map.fromList
          [ ("title", toMetaValue $ title meta),
            ("date", toMetaValue $ show $ date meta),
            ("tags", toMetaValue $ Set.toList $ tags meta)
          ]
