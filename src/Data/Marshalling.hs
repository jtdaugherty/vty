{-# LANGUAGE BangPatterns #-}
-- Copyright 2009 Corey O'Connor
module Data.Marshalling ( module Data.Marshalling
                        , module Data.Word
                        , module Foreign.Ptr
                        , module Foreign.ForeignPtr
                        , module Foreign.Marshal
                        , module Foreign.Storable
                        )
    where

import Control.Monad.Trans

import Data.Word

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable

type OutputBuffer = Ptr Word8

string_to_bytes :: String -> [Word8]
string_to_bytes str = map (toEnum . fromEnum) str

serialize_bytes :: MonadIO m => [Word8] -> OutputBuffer -> m OutputBuffer
serialize_bytes bytes !out_ptr = do
    liftIO $! pokeArray out_ptr bytes
    return $! out_ptr `plusPtr` ( length bytes )

