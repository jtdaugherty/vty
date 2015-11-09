module Graphics.Vty.Output.Terminfo.PosixIOExtras where

import Graphics.Vty.Prelude

import Blaze.ByteString.Builder (Builder, toByteString)

import Data.ByteString.Internal (toForeignPtr)
import Data.Word

-- Fd is an alias of CInt, import CInt for serialization of Fd
import Foreign.C.Types (CInt(..), CLong(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)

import System.Posix.IO (fdWriteBuf)
import System.Posix.Types (Fd(..))

-- | Write count bytes from buffer of size len.
--
-- Will throw on write error. Unless the error is EINTR. On EINTR the write will be retried.
--
-- Reference: https://code.google.com/p/vim/source/browse/src/fileio.c#10422
fdWriteAll :: Fd -> Ptr Word8 -> Int -> Int -> IO Int
fdWriteAll outFd ptr len count
    | len <  0  = fail "fdWriteAll: len is less than 0"
    | len == 0  = return count
    | otherwise = do
        writeCount <- fromEnum <$> fdWriteBuf outFd ptr (toEnum len)
        let len' = len - writeCount
            ptr' = ptr `plusPtr` writeCount
            count' = count + writeCount
        fdWriteAll outFd ptr' len' count'

-- | output the given `Builder` to the given Fd
fdWriteBuilder :: Fd -> Builder -> IO ()
fdWriteBuilder outFd builder = do
    let bytes = toByteString builder
    let (fptr, offset, len) = toForeignPtr bytes
    actualLen <- withForeignPtr fptr
                 $ \ptr -> fdWriteAll outFd (ptr `plusPtr` offset) len 0
    let errorMsg = "Graphics.Vty.Output: outputByteBuffer "
                   ++ "length mismatch. " ++ show len ++ " /= " ++ show actualLen
                   ++ " Please report this bug to vty project."
    when (toEnum len /= actualLen) $ fail errorMsg

foreign import ccall "gwinsz.h vty_c_get_window_size" c_getWindowSize :: Fd -> IO CLong

-- | Assuming the given Fd is a terminal device, return the window size.
getWindowSize :: Fd -> IO (Int,Int)
getWindowSize fd = do
    (a,b) <- (`divMod` 65536) `fmap` c_getWindowSize fd
    return (fromIntegral b, fromIntegral a)
