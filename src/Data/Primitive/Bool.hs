{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Primitive.Bool where

import Data.Primitive.Types
import GHC.Prim 
import GHC.Types (Int(..))

newtype BoolByte = BoolByte { getBoolByte :: Bool }

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

-- #define derivePrim(ty, ctr, sz, align, idx_arr, rd_arr, wr_arr, set_arr, idx_addr, rd_addr, wr_addr, set_addr) \
instance Prim BoolByte where
  sizeOf# _ = 1#
  alignment# _ = 1#
  indexByteArray# arr# i# = toBool# (indexWord8Array# arr# i#)               
  readByteArray# arr# i# s# = 
    case readWord8Array# arr# i# s# of        
      { (# s1#, x# #) -> (# s1#, toBool# x# #) }  
  writeByteArray# arr# i# b s# = 
    writeWord8Array# arr# i# (fromBool# b) s#    
-- ; setByteArray# arr# i# n# (ctr x#) s#                          
--     = let { i = fromIntegral (I# i#)                            
--           ; n = fromIntegral (I# n#)                            
--           } in                                                  
--       case unsafeCoerce# (internal (set_arr arr# i n x#)) s# of 
--         { (# s1#, _ #) -> s1# }                                 
--                                                                 
-- ; indexOffAddr# addr# i# = ctr (idx_addr addr# i#)              
-- ; readOffAddr#  addr# i# s# = case rd_addr addr# i# s# of       
--                         { (# s1#, x# #) -> (# s1#, ctr x# #) }  
-- ; writeOffAddr# addr# i# (ctr x#) s# = wr_addr addr# i# x# s#   
-- ; setOffAddr# addr# i# n# (ctr x#) s#                           
--     = let { i = fromIntegral (I# i#)                            
--           ; n = fromIntegral (I# n#)                            
--           } in                                                  
--       case unsafeCoerce# (internal (set_addr addr# i n x#)) s# of 
--         { (# s1#, _ #) -> s1# }                                 
  {-# INLINE sizeOf# #-}                                        
  {-# INLINE alignment# #-}                                     
  {-# INLINE indexByteArray# #-}                                
  {-# INLINE readByteArray# #-}                                 
  {-# INLINE writeByteArray# #-}                                
-- {-# INLINE setByteArray# #-}                                  
-- {-# INLINE indexOffAddr# #-}                                  
-- {-# INLINE readOffAddr# #-}                                   
-- {-# INLINE writeOffAddr# #-}                                  
-- {-# INLINE setOffAddr# #-}                                    

unI# :: Int -> Int#
unI# (I# n#) = n#

-- derivePrim(Word8, W8#, sIZEOF_WORD8, aLIGNMENT_WORD8,
--            indexWord8Array#, readWord8Array#, writeWord8Array#, setWord8Array#,
--            indexWord8OffAddr#, readWord8OffAddr#, writeWord8OffAddr#, setWord8OffAddr#)


