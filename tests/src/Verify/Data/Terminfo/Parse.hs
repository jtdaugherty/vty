module Verify.Data.Terminfo.Parse
  ( module Verify.Data.Terminfo.Parse
  , module Data.Terminfo.Parse
  )
where

import Data.Terminfo.Parse
import Data.Terminfo.Eval
import Verify

import Data.Word
import qualified Data.Vector.Unboxed as Vector

import Numeric

hexDump :: [Word8] -> String
hexDump bytes = foldr (\b s -> showHex b s) "" bytes

data NonParamCapString = NonParamCapString String
    deriving Show

instance Arbitrary NonParamCapString where
    arbitrary
        = ( do
            s <- listOf1 $ (choose (0, 255) >>= return . toEnum) `suchThat` (/= '%')
            return $ NonParamCapString s
          ) `suchThat` ( \(NonParamCapString str) -> length str < 255 )

data LiteralPercentCap = LiteralPercentCap String [Word8]
    deriving ( Show )

instance Arbitrary LiteralPercentCap where
    arbitrary
        = ( do
            NonParamCapString s <- arbitrary
            (s', bytes) <- insertEscapeOp "%" [toEnum $ fromEnum '%'] s
            return $ LiteralPercentCap s' bytes
          ) `suchThat` ( \(LiteralPercentCap str _) -> length str < 255 )

data IncFirstTwoCap = IncFirstTwoCap String [Word8]
    deriving ( Show )

instance Arbitrary IncFirstTwoCap where
    arbitrary
        = ( do
            NonParamCapString s <- arbitrary
            (s', bytes) <- insertEscapeOp "i" [] s
            return $ IncFirstTwoCap s' bytes
          ) `suchThat` ( \(IncFirstTwoCap str _) -> length str < 255 )

data PushParamCap = PushParamCap String Int [Word8]
    deriving ( Show )

instance Arbitrary PushParamCap where
    arbitrary
        = ( do
            NonParamCapString s <- arbitrary
            n <- choose (1,9)
            (s', bytes) <- insertEscapeOp ("p" ++ show n) [] s
            return $ PushParamCap s' n bytes
          ) `suchThat` ( \(PushParamCap str _ _) -> length str < 255 )

data DecPrintCap = DecPrintCap String Int [Word8]
    deriving ( Show )

instance Arbitrary DecPrintCap where
    arbitrary
        = ( do
            NonParamCapString s <- arbitrary
            n <- choose (1,9)
            (s', bytes) <- insertEscapeOp ("p" ++ show n ++ "%d") [] s
            return $ DecPrintCap s' n bytes
          ) `suchThat` ( \(DecPrintCap str _ _) -> length str < 255 )

insertEscapeOp opStr replBytes s = do
    insertPoints <- listOf1 $ elements [0 .. length s - 1]
    let s' = f s ('%' : opStr)
        remainingBytes = f (map (toEnum . fromEnum) s) replBytes
        f inVs out_v = concat [ vs
                              | vi <- zip inVs [0 .. length s - 1]
                              , let vs = fst vi : ( if snd vi `elem` insertPoints
                                                       then out_v
                                                       else []
                                                  )
                              ]
    return (s', remainingBytes)

isBytesOp :: CapOp -> Bool
isBytesOp (Bytes {}) = True
-- isBytesOp _ = False

bytesForRange cap offset count
    = Vector.toList $ Vector.take count $ Vector.drop offset $ capBytes cap

collectBytes :: CapExpression -> [Word8]
collectBytes e = concat [ bytes
                        | Bytes offset count <- capOps e
                        , let bytes = bytesForRange e offset count
                        ]


verifyBytesEqual :: [Word8] -> [Word8] -> Result
verifyBytesEqual outBytes expectedBytes
    = if outBytes == expectedBytes
        then succeeded
        else failed
             { reason = "outBytes ["
                      ++ hexDump outBytes
                      ++ "] /= expectedBytes ["
                      ++ hexDump expectedBytes
                      ++ "]"
             }
