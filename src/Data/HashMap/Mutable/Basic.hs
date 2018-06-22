{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MagicHash    #-}

-- | This whole module is mostly copied from the
--   <http://hackage.haskell.org/package/hashtables hashtables> package.
--   You can find much better documentation there. Additionally, if you
--   problem in the implementation of a function, you should probably open
--   up the issue on the <https://github.com/gregorycollins/hashtables hashtables github repo>
--   since Gregory is the one who actually authored (and the one who
--   actually understands) this code. The main differences are as follows:
--
--   * The type is named @MHashMap@ instead of @HashTable@.
--   * All functions are generalized to work in any 'PrimMonad' instead
--     of only 'ST'.
--   * The functions 'mapM_' and 'foldM' have been curried. They do not
--     take tuples here.
--   * There is no type class used to overload the hashtable operations.
--

module Data.HashMap.Mutable.Basic
  ( MHashMap
  , new
  , newSized
  , delete
  , lookup
  , insert
  , mapM_
  , foldM
  , computeOverhead
  ) where


------------------------------------------------------------------------------
import           Control.Exception                       (assert)
import           Control.Monad                           hiding (foldM, mapM_)
import           Control.Monad.Primitive (PrimMonad, PrimState, unsafePrimToPrim)
import           Control.Monad.ST                        (ST)
import           Data.Bits
import           Data.Hashable                           (Hashable)
import           Data.Maybe
import           Data.Monoid
import           Data.Primitive.MutVar             (MutVar, newMutVar, readMutVar, writeMutVar)
import           Data.STRef
import           Data.Semigroup                    (Semigroup)
import           GHC.Exts
import           Prelude hiding (lookup, mapM_, read)
import qualified Data.Hashable                           as H
import qualified Data.Primitive.ByteArray                as A
import qualified Data.Primitive.ByteArray          as A
------------------------------------------------------------------------------
import           Data.HashMap.Mutable.Internal.Array
import           Data.HashMap.Mutable.Internal.CacheLine
import           Data.HashMap.Mutable.Internal.IntArray  (Elem)
import qualified Data.HashMap.Mutable.Internal.IntArray  as U
import           Data.HashMap.Mutable.Internal.Utils
import qualified Data.Semigroup                          as SG


------------------------------------------------------------------------------
-- | An open addressing hash table using linear probing.
newtype MHashMap s k v = HT (MutVar s (HashTable_ s k v))

type SizeRefs s = A.MutableByteArray s

intSz :: Int
intSz = (finiteBitSize (0::Int) `div` 8)

readLoad :: PrimMonad m => SizeRefs (PrimState m) -> m Int
readLoad = flip A.readByteArray 0

writeLoad :: PrimMonad m => SizeRefs (PrimState m) -> Int -> m ()
writeLoad = flip A.writeByteArray 0

readDelLoad :: PrimMonad m => SizeRefs (PrimState m) -> m Int
readDelLoad = flip A.readByteArray 1

writeDelLoad :: PrimMonad m => SizeRefs (PrimState m) -> Int -> m ()
writeDelLoad = flip A.writeByteArray 1

newSizeRefs :: PrimMonad m => m (SizeRefs (PrimState m))
newSizeRefs = do
    let asz = 2 * intSz
    a <- A.newAlignedPinnedByteArray asz intSz
    A.fillByteArray a 0 asz 0
    return a


data HashTable_ s k v = MHashMap
    { _size   :: {-# UNPACK #-} !Int
    , _load   :: !(SizeRefs s)   -- ^ 2-element array, stores how many entries
                                  -- and deleted entries are in the table.
    , _hashes :: !(U.IntArray s)
    , _keys   :: {-# UNPACK #-} !(MutableArray s k)
    , _values :: {-# UNPACK #-} !(MutableArray s v)
    }


------------------------------------------------------------------------------
instance Show (MHashMap s k v) where
    show _ = "<MHashMap>"


new :: PrimMonad m => m (MHashMap (PrimState m) k v)
new = newSized 1
{-# INLINE new #-}

newSized :: PrimMonad m => Int -> m (MHashMap (PrimState m) k v)
newSized n = do
    debug $ "entering: newSized " ++ show n
    let m = nextBestPrime $ ceiling (fromIntegral n / maxLoad)
    ht <- newSizedReal m
    newRef ht
{-# INLINE newSized #-}

newSizedReal :: PrimMonad m => Int -> m (HashTable_ (PrimState m) k v)
newSizedReal m = do
    -- make sure the hash array is a multiple of cache-line sized so we can
    -- always search a whole cache line at once
    let m' = ((m + numElemsInCacheLine - 1) `div` numElemsInCacheLine)
             * numElemsInCacheLine
    h  <- U.newArray m'
    k  <- newArray m undefined
    v  <- newArray m undefined
    ld <- newSizeRefs
    return $! MHashMap m ld h k v

delete :: (PrimMonad m, Hashable k, Eq k) =>
          (MHashMap (PrimState m) k v)
       -> k
       -> m ()
delete htRef k = do
    debug $ "entered: delete: hash=" ++ show h
    ht <- readRef htRef
    _  <- delete' ht True k h
    return ()
  where
    !h = hash k
{-# INLINE delete #-}

lookup :: (PrimMonad m, Eq k, Hashable k) => (MHashMap (PrimState m) k v) -> k -> m (Maybe v)
lookup htRef !k = do
    ht <- readRef htRef
    lookup' ht
  where
    lookup' (MHashMap sz _ hashes keys values) = do
        let !b = whichBucket h sz
        debug $ "lookup h=" ++ show h ++ " sz=" ++ show sz ++ " b=" ++ show b
        go b 0 sz

      where
        !h  = hash k
        !he = hashToElem h

        go !b !start !end = {-# SCC "lookup/go" #-} do
            debug $ concat [ "lookup'/go: "
                           , show b
                           , "/"
                           , show start
                           , "/"
                           , show end
                           ]
            idx <- forwardSearch2 hashes b end he emptyMarker
            debug $ "forwardSearch2 returned " ++ show idx
            if (idx < 0 || idx < start || idx >= end)
               then return Nothing
               else do
                 h0  <- U.readArray hashes idx
                 debug $ "h0 was " ++ show h0

                 if recordIsEmpty h0
                   then do
                       debug $ "record empty, returning Nothing"
                       return Nothing
                   else do
                     k' <- readArray keys idx
                     if k == k'
                       then do
                         debug $ "value found at " ++ show idx
                         v <- readArray values idx
                         return $! Just v
                       else do
                         debug $ "value not found, recursing"
                         if idx < b
                           then go (idx + 1) (idx + 1) b
                           else go (idx + 1) start end
{-# INLINE lookup #-}

insert :: (PrimMonad m, Eq k, Hashable k) =>
          (MHashMap (PrimState m) k v)
       -> k
       -> v
       -> m ()
insert htRef !k !v = do
    ht <- readRef htRef
    !ht' <- insert' ht
    writeRef htRef ht'

  where
    insert' ht = do
        debug "insert': calling delete'"
        b <- delete' ht False k h

        debug $ concat [ "insert': writing h="
                       , show h
                       , " he="
                       , show he
                       , " b="
                       , show b
                       ]
        U.writeArray hashes b he
        writeArray keys b k
        writeArray values b v

        checkOverflow ht

      where
        !h     = hash k
        !he    = hashToElem h
        hashes = _hashes ht
        keys   = _keys ht
        values = _values ht
{-# INLINE insert #-}

foldM :: PrimMonad m => (a -> k -> v -> m a) -> a -> MHashMap (PrimState m) k v -> m a
foldM f seed0 htRef = readRef htRef >>= work
  where
    work (MHashMap sz _ hashes keys values) = go 0 seed0
      where
        go !i !seed | i >= sz = return seed
                    | otherwise = do
            h <- U.readArray hashes i
            if recordIsEmpty h || recordIsDeleted h
              then go (i+1) seed
              else do
                k <- readArray keys i
                v <- readArray values i
                !seed' <- f seed k v
                go (i+1) seed'

mapM_ :: PrimMonad m => (k -> v -> m b) -> MHashMap (PrimState m) k v -> m ()
mapM_ f htRef = readRef htRef >>= work
  where
    work (MHashMap sz _ hashes keys values) = go 0
      where
        go !i | i >= sz = return ()
              | otherwise = do
            h <- U.readArray hashes i
            if recordIsEmpty h || recordIsDeleted h
              then go (i+1)
              else do
                k <- readArray keys i
                v <- readArray values i
                _ <- f k v
                go (i+1)

computeOverhead :: PrimMonad m => MHashMap (PrimState m) k v -> m Double
computeOverhead htRef = readRef htRef >>= work
  where
    work (MHashMap sz' loadRef _ _ _) = do
        !ld <- readLoad loadRef
        let k = fromIntegral ld / sz
        return $ constOverhead/sz + (2 + 2*ws*(1-k)) / (k * ws)
      where
        ws = fromIntegral $! finiteBitSize (0::Int) `div` 8
        sz = fromIntegral sz'
        -- Change these if you change the representation
        constOverhead = 14


------------------------------
-- Private functions follow --
------------------------------


------------------------------------------------------------------------------
{-# INLINE insertRecord #-}
insertRecord :: PrimMonad m
             => Int
             -> U.IntArray (PrimState m)
             -> MutableArray (PrimState m) k
             -> MutableArray (PrimState m) v
             -> Int
             -> k
             -> v
             -> m ()
insertRecord !sz !hashes !keys !values !h !key !value = do
    let !b = whichBucket h sz
    debug $ "insertRecord sz=" ++ show sz ++ " h=" ++ show h ++ " b=" ++ show b
    probe b

  where
    he = hashToElem h

    probe !i = {-# SCC "insertRecord/probe" #-} do
        !idx <- forwardSearch2 hashes i sz emptyMarker deletedMarker
        debug $ "forwardSearch2 returned " ++ show idx
        assert (idx >= 0) $ do
            U.writeArray hashes idx he
            writeArray keys idx key
            writeArray values idx value


------------------------------------------------------------------------------
checkOverflow :: (PrimMonad m, Eq k, Hashable k) =>
                 (HashTable_ (PrimState m) k v)
              -> m (HashTable_ (PrimState m) k v)
checkOverflow ht@(MHashMap sz ldRef _ _ _) = do
    !ld <- readLoad ldRef
    let !ld' = ld + 1
    writeLoad ldRef ld'
    !dl <- readDelLoad ldRef

    debug $ concat [ "checkOverflow: sz="
                   , show sz
                   , " entries="
                   , show ld
                   , " deleted="
                   , show dl ]

    if fromIntegral (ld + dl) / fromIntegral sz > maxLoad
      then if dl > ld `div` 2
             then rehashAll ht sz
             else growTable ht
      else return ht


------------------------------------------------------------------------------
rehashAll :: (Hashable k, PrimMonad m) => HashTable_ (PrimState m) k v -> Int -> m (HashTable_ (PrimState m) k v)
rehashAll (MHashMap sz loadRef hashes keys values) sz' = do
    debug $ "rehashing: old size " ++ show sz ++ ", new size " ++ show sz'
    ht' <- newSizedReal sz'
    let (MHashMap _ loadRef' newHashes newKeys newValues) = ht'
    readLoad loadRef >>= writeLoad loadRef'
    rehash newHashes newKeys newValues
    return ht'

  where
    rehash newHashes newKeys newValues = go 0
      where
        go !i | i >= sz   = return ()
              | otherwise = {-# SCC "growTable/rehash" #-} do
                    h0 <- U.readArray hashes i
                    when (not (recordIsEmpty h0 || recordIsDeleted h0)) $ do
                        k <- readArray keys i
                        v <- readArray values i
                        insertRecord sz' newHashes newKeys newValues
                                     (hash k) k v
                    go $ i+1


------------------------------------------------------------------------------
growTable :: (Hashable k, PrimMonad m) => HashTable_ (PrimState m) k v -> m (HashTable_ (PrimState m) k v)
growTable ht@(MHashMap sz _ _ _ _) = do
    let !sz' = bumpSize maxLoad sz
    rehashAll ht sz'


------------------------------------------------------------------------------
-- Helper data structure for delete'
data Slot = Slot {
      _slot       :: {-# UNPACK #-} !Int
    , _wasDeleted :: {-# UNPACK #-} !Int  -- we use Int because Bool won't
                                          -- unpack
    }
  deriving (Show)


------------------------------------------------------------------------------
instance Semigroup Slot where
    Slot x1 b1 <> Slot x2 b2 = if x1 == maxBound then Slot x2 b2 else Slot x1 b1

instance Monoid Slot where
    mempty = Slot maxBound 0
    mappend = (SG.<>)

------------------------------------------------------------------------------
-- Returns the slot in the array where it would be safe to write the given key.
delete' :: (PrimMonad m, Hashable k, Eq k) =>
           (HashTable_ (PrimState m) k v)
        -> Bool
        -> k
        -> Int
        -> m Int
delete' (MHashMap sz loadRef hashes keys values) clearOut k h = do
    debug $ "delete': h=" ++ show h ++ " he=" ++ show he
            ++ " sz=" ++ show sz ++ " b0=" ++ show b0
    pair@(found, slot) <- go mempty b0 False
    debug $ "go returned " ++ show pair

    let !b' = _slot slot

    when found $ bump loadRef (-1)

    -- bump the delRef lower if we're writing over a deleted marker
    when (not clearOut && _wasDeleted slot == 1) $ bumpDel loadRef (-1)
    return b'

  where
    he = hashToElem h
    bump ref i = do
        !ld <- readLoad ref
        writeLoad ref $! ld + i
    bumpDel ref i = do
        !ld <- readDelLoad ref
        writeDelLoad ref $! ld + i

    !b0 = whichBucket h sz

    haveWrapped !(Slot fp _) !b = if fp == maxBound
                                    then False
                                    else b <= fp

    -- arguments:

    --   * fp    maintains the slot in the array where it would be safe to
    --           write the given key
    --   * b     search the buckets array starting at this index.
    --   * wrap  True if we've wrapped around, False otherwise

    go !fp !b !wrap = do
        debug $ concat [ "go: fp="
                       , show fp
                       , " b="
                       , show b
                       , ", wrap="
                       , show wrap
                       , ", he="
                       , show he
                       , ", emptyMarker="
                       , show emptyMarker
                       , ", deletedMarker="
                       , show deletedMarker ]

        !idx <- forwardSearch3 hashes b sz he emptyMarker deletedMarker
        debug $ "forwardSearch3 returned " ++ show idx ++ " with sz=" ++ show sz ++ ", b=" ++ show b

        if wrap && idx >= b0
          -- we wrapped around in the search and didn't find our hash code;
          -- this means that the table is full of deleted elements. Just return
          -- the first place we'd be allowed to insert.
          --
          -- TODO: if we get in this situation we should probably just rehash
          -- the table, because every insert is going to be O(n).
          then return $!
                   (False, fp `mappend` (Slot (error "impossible") 0))
          else do
            -- because the table isn't full, we know that there must be either
            -- an empty or a deleted marker somewhere in the table. Assert this
            -- here.
            assert (idx >= 0) $ return ()
            h0 <- U.readArray hashes idx
            debug $ "h0 was " ++ show h0

            if recordIsEmpty h0
              then do
                  let pl = fp `mappend` (Slot idx 0)
                  debug $ "empty, returning " ++ show pl
                  return (False, pl)
              else do
                let !wrap' = haveWrapped fp idx
                if recordIsDeleted h0
                  then do
                      let pl = fp `mappend` (Slot idx 1)
                      debug $ "deleted, cont with pl=" ++ show pl
                      go pl (idx + 1) wrap'
                  else
                    if he == h0
                      then do
                        debug $ "found he == h0 == " ++ show h0
                        k' <- readArray keys idx
                        if k == k'
                          then do
                            let samePlace = _slot fp == idx
                            debug $ "found at " ++ show idx
                            debug $ "clearout=" ++ show clearOut
                            debug $ "sp? " ++ show samePlace
                            -- "clearOut" is set if we intend to write a new
                            -- element into the slot. If we're doing an update
                            -- and we found the old key, instead of writing
                            -- "deleted" and then re-writing the new element
                            -- there, we can just write the new element. This
                            -- only works if we were planning on writing the
                            -- new element here.
                            when (clearOut || not samePlace) $ do
                                bumpDel loadRef 1
                                U.writeArray hashes idx deletedMarker
                                writeArray keys idx undefined
                                writeArray values idx undefined
                            return (True, fp `mappend` (Slot idx 0))
                          else go fp (idx + 1) wrap'
                      else go fp (idx + 1) wrap'

------------------------------------------------------------------------------
maxLoad :: Double
maxLoad = 0.82


------------------------------------------------------------------------------
emptyMarker :: Elem
emptyMarker = 0

------------------------------------------------------------------------------
deletedMarker :: Elem
deletedMarker = 1


------------------------------------------------------------------------------
{-# INLINE recordIsEmpty #-}
recordIsEmpty :: Elem -> Bool
recordIsEmpty = (== emptyMarker)


------------------------------------------------------------------------------
{-# INLINE recordIsDeleted #-}
recordIsDeleted :: Elem -> Bool
recordIsDeleted = (== deletedMarker)


------------------------------------------------------------------------------
{-# INLINE hash #-}
hash :: (Hashable k) => k -> Int
hash = H.hash


------------------------------------------------------------------------------
{-# INLINE hashToElem #-}
hashToElem :: Int -> Elem
hashToElem !h = out
  where
    !(I# lo#) = h .&. U.elemMask

    !m#  = maskw# lo# 0# `or#` maskw# lo# 1#
    !nm# = not# m#

    !r#  = ((int2Word# 2#) `and#` m#) `or#` (int2Word# lo# `and#` nm#)
    !out = U.primWordToElem r#


------------------------------------------------------------------------------
newRef :: PrimMonad m => HashTable_ (PrimState m) k v -> m (MHashMap (PrimState m) k v)
newRef = liftM HT . newMutVar
{-# INLINE newRef #-}

writeRef :: PrimMonad m => MHashMap (PrimState m) k v -> HashTable_ (PrimState m) k v -> m ()
writeRef (HT ref) ht = writeMutVar ref ht
{-# INLINE writeRef #-}

readRef :: PrimMonad m => MHashMap (PrimState m) k v -> m (HashTable_ (PrimState m) k v)
readRef (HT ref) = readMutVar ref
{-# INLINE readRef #-}


------------------------------------------------------------------------------
{-# INLINE debug #-}
debug :: PrimMonad m => String -> m ()
#ifdef DEBUG
debug s = unsafePrimToPrim (putStrLn s)
#else
debug _ = return ()
#endif
