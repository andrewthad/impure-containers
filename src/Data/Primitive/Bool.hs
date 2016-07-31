{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Primitive.Bool 
  ( BoolByte(..)
  ) where

import Data.Primitive.Types
import qualified Data.Primitive.Types as P
import Data.Word
import GHC.Prim 
import GHC.Types (Int(..))

{-@ data BoolByte = BoolByte { getBoolByte :: Bool } @-}
newtype BoolByte = BoolByte { getBoolByte :: Bool }

toBool :: Word8 -> BoolByte
toBool w = case w of
  0 -> BoolByte False
  _ -> BoolByte True
{-# INLINE toBool #-}

toBool# :: Word# -> BoolByte
toBool# w# = case eqWord# w# 0## of
  0# -> BoolByte False
  _  -> BoolByte True
{-# INLINE toBool# #-}

fromBool# :: BoolByte -> Word#
fromBool# (BoolByte b) = case b of
  True -> 1##
  False -> 0##
{-# INLINE fromBool# #-}

fromBool :: BoolByte -> Word8
fromBool (BoolByte b) = case b of
  True -> 1
  False -> 0

instance Prim BoolByte where
  sizeOf# _ = 1#
  alignment# _ = 1#
  indexByteArray# arr# i# = toBool# (indexWord8Array# arr# i#)               
  readByteArray# arr# i# s# = 
    case readWord8Array# arr# i# s# of        
      { (# s1#, x# #) -> (# s1#, toBool# x# #) }  
  writeByteArray# arr# i# b s# = 
    writeWord8Array# arr# i# (fromBool# b) s#    
  setByteArray# arr# i# n# b s# = P.setByteArray# arr# i# n# (fromBool b) s#
  indexOffAddr# addr# i# = toBool (indexOffAddr# addr# i#)
  readOffAddr#  addr# i# s# = 
    case readOffAddr# addr# i# s# of
      (# s1#, w #) -> (# s1#, toBool w #)
  writeOffAddr# addr# i# b s# = writeOffAddr# addr# i# (fromBool b) s#
  setOffAddr# addr# i# n# b s# = 
    setOffAddr# addr# i# n# (fromBool b) s#
  {-# INLINE sizeOf# #-}                                        
  {-# INLINE alignment# #-}                                     
  {-# INLINE indexByteArray# #-}                                
  {-# INLINE readByteArray# #-}                                 
  {-# INLINE writeByteArray# #-}                                
  {-# INLINE setByteArray# #-}                                  
  {-# INLINE indexOffAddr# #-}                                  
  {-# INLINE readOffAddr# #-}                                   
  {-# INLINE writeOffAddr# #-}                                  
  {-# INLINE setOffAddr# #-}                                    

