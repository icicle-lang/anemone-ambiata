{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Anemone.Headers (
    getCollatedHeaders
  ) where

import           P

import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Data.FileEmbed

import qualified Data.List as List

import           System.FilePath

getCollatedHeaders :: T.Text
getCollatedHeaders
 = T.unlines
 $ fmap (uncurry prepareHeader)
 $ filter (\(nm,_) -> takeExtension nm == ".h")
 $ getAllFiles

getAllFiles :: [(FilePath, ByteString)]
getAllFiles
 = List.sortBy (compare `on` fst)
 $ $(embedDir "csrc")

prepareHeader :: FilePath -> ByteString -> T.Text
prepareHeader path bs
 = T.unlines
 [ "// " <> T.pack path
 , "#line " <> int lineNo <> " \"" <> T.pack path <> "\""
 , T.unlines file
 , ""
 ]
 where
  (includes, file)
   = List.span (T.isPrefixOf "#include \"")
   . T.lines
   $ T.decodeUtf8 bs

  int
   = T.pack . show

  lineNo
   = List.length includes + 1

