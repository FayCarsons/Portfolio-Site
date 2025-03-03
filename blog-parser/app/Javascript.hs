{-# LANGUAGE TypeApplications #-}

module Javascript (writeJSMetadata) where

import Control.Exception (IOException, catch)
import Data.List (intercalate)
import Post (PostMeta (..))
import System.Directory (createDirectory, doesDirectoryExist, removeDirectory)
import System.FilePath ((</>))

writeJSMetadata :: FilePath -> [PostMeta] -> IO ()
writeJSMetadata jsDir posts = do
  exists <- doesDirectoryExist jsDir
  if exists
    then
      catch @IOException (removeDirectory jsDir) (const $ return ())
    else
      createDirectory jsDir
  writeFile jsPath js
 where
  js = "const posts = [" <> intercalate (repeat ',') (map (delimit . path) posts) <> "]"
  jsPath = jsDir </> "posts.js"
  delimit s = "\"" <> s <> "\""
