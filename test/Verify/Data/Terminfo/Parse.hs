{-# LANGUAGE ScopedTypeVariables #-}
module Verify.Data.Terminfo.Parse ( module Verify.Data.Terminfo.Parse
                                  , module Data.Terminfo.Parse
                                  )
    where

import Data.Terminfo.Parse
import Data.Terminfo.Eval
import Verify

import Data.Word

import Numeric

instance Show CapExpression where
    show c 
        = "CapExpression { " ++ show (cap_ops c) ++ " }"
        ++ " <- [" ++ hex_dump ( map ( toEnum . fromEnum ) $! source_string c ) ++ "]"
        ++ " <= " ++ show (source_string c) 

hex_dump :: [Word8] -> String
hex_dump bytes = foldr (\b s -> showHex b s) "" bytes

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
            (s', bytes) <- insert_escape_op "%" [toEnum $ fromEnum '%'] s
            return $ LiteralPercentCap s' bytes
          ) `suchThat` ( \(LiteralPercentCap str _) -> length str < 255 ) 

data IncFirstTwoCap = IncFirstTwoCap String [Word8]
    deriving ( Show )

instance Arbitrary IncFirstTwoCap where
    arbitrary
        = ( do
            NonParamCapString s <- arbitrary
            (s', bytes) <- insert_escape_op "i" [] s
            return $ IncFirstTwoCap s' bytes
          ) `suchThat` ( \(IncFirstTwoCap str _) -> length str < 255 ) 

data PushParamCap = PushParamCap String Word [Word8]
    deriving ( Show )

instance Arbitrary PushParamCap where
    arbitrary
        = ( do
            NonParamCapString s <- arbitrary
            n :: Word <- choose (1,9) >>= return . toEnum
            (s', bytes) <- insert_escape_op ("p" ++ show n) [] s
            return $ PushParamCap s' n bytes
          ) `suchThat` ( \(PushParamCap str _ _) -> length str < 255 ) 

data DecPrintCap = DecPrintCap String Word [Word8]
    deriving ( Show )

instance Arbitrary DecPrintCap where
    arbitrary
        = ( do
            NonParamCapString s <- arbitrary
            n :: Word <- choose (1,9) >>= return . toEnum
            (s', bytes) <- insert_escape_op ("p" ++ show n ++ "%d") [] s
            return $ DecPrintCap s' n bytes
          ) `suchThat` ( \(DecPrintCap str _ _) -> length str < 255 ) 

insert_escape_op op_str repl_bytes s = do
    insert_points <- listOf1 $ elements [0 .. length s - 1]
    let s' = f s ('%' : op_str)
        remaining_bytes = f (map (toEnum . fromEnum) s) repl_bytes
        f in_vs out_v = concat [ vs
                               | vi <- zip in_vs [0 .. length s - 1]
                               , let vs = fst vi : ( if snd vi `elem` insert_points
                                                        then out_v
                                                        else []
                                                   )
                               ]
    return (s', remaining_bytes)

is_bytes_op :: CapOp -> Bool
is_bytes_op (Bytes {}) = True
-- is_bytes_op _ = False

bytes_for_range cap offset c
    = take (fromEnum c)
    $ drop (fromEnum offset)
    $ ( map ( toEnum . fromEnum ) $! source_string cap )

collect_bytes :: CapExpression -> [Word8]
collect_bytes e = concat [ bytes 
                         | Bytes offset c _ <- cap_ops e
                         , let bytes = bytes_for_range e offset c
                         ]
    

verify_bytes_equal :: [Word8] -> [Word8] -> Result
verify_bytes_equal out_bytes expected_bytes 
    = if out_bytes == expected_bytes
        then succeeded
        else failed 
             { reason = "out_bytes [" 
                       ++ hex_dump out_bytes
                       ++ "] /= expected_bytes ["
                       ++ hex_dump expected_bytes
                       ++ "]"
             }

