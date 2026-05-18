{-# LANGUAGE CApiFFI #-}

module Oke.Util.Plist (Plist, readPlistXML, withPlistXML, getDictItem, getString) where

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.Text.Foreign qualified as T
import Data.Text.IO (hPutStrLn)
import Foreign
import Foreign.C

data PlistNode

type Plist = Ptr PlistNode

foreign import capi "plist/plist.h plist_from_xml"
  plist_from_xml :: CString -> Word32 -> Ptr Plist -> IO CInt

foreign import capi "plist/plist.h plist_free"
  plist_free :: Plist -> IO ()

-- foreign import capi "plist/plist.h plist_print"
--   plist_print :: Plist -> IO ()

foreign import capi "plist/plist.h plist_dict_get_item"
  plist_dict_get_item :: Plist -> CString -> IO Plist

foreign import capi "plist/plist.h plist_get_string_ptr"
  plist_get_string_ptr :: Plist -> Ptr Word64 -> IO CString

readPlistXML :: Text -> IO Plist
readPlistXML xml =
  BS.useAsCStringLen (encodeUtf8 xml) $ \(ptr, len) ->
    alloca $ \plistPtr -> do
      poke plistPtr nullPtr
      err <- plist_from_xml ptr (fromIntegral len) plistPtr
      plist <- peek plistPtr
      if err == 0
        then pure plist
        else hPutStrLn stderr "plist: failed to parse plist xml" *> exitFailure

withPlistXML :: Text -> (Plist -> IO a) -> IO a
withPlistXML xml = bracket (readPlistXML xml) plist_free

getDictItem :: Plist -> Text -> IO Plist
getDictItem plist key = T.withCString key $ \ckey -> do
  node <- plist_dict_get_item plist ckey
  if node == nullPtr
    then hPutStrLn stderr ("plist: undefined key " <> key) *> exitFailure
    else pure node

getString :: Plist -> IO Text
getString plist = do
  cstr <- plist_get_string_ptr plist nullPtr
  if cstr == nullPtr
    then hPutStrLn stderr "plist: not a string value" *> exitFailure
    else T.peekCString cstr
