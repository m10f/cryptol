-- |
-- Module      :  Cryptol.Backend.WordValue
-- Copyright   :  (c) 2013-2021 Galois, Inc.
-- License     :  BSD3
-- Maintainer  :  cryptol@galois.com
-- Stability   :  provisional
-- Portability :  portable

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Cryptol.Backend.WordValue
  ( -- * WordValue
    WordValue
  , wordVal
  , largeBitsVal
  , asWordList
  , asWordVal
  , asBitsMap
  , joinWordVal
  , splitWordVal
  , extractWordVal
  , wordValLogicOp
  , wordValUnaryOp
  , assertWordValueInBounds
  , enumerateWordValue
  , enumerateWordValueRev
  , wordValueSize
  , indexWordValue
  , updateWordValue
  , viewWordOrBits
  , viewWordOrBitsMap
  , lazyViewWordOrBits
  , lazyViewWordOrBitsMap
  , delayWordValue
  , joinWords
  , wordShiftByInt
  , wordShiftByWord
  , wordValAsLit
  , forceWordValue

  , enumerateIntBits'
  , mergeWord
  , mergeWord'
  ) where

import Control.Monad (join, unless)
import Data.Bits
import GHC.Generics (Generic)

import Cryptol.Backend
import Cryptol.Backend.Concrete (Concrete(..))
import Cryptol.Backend.Monad (EvalError(..))
import Cryptol.Backend.SeqMap

import Cryptol.TypeCheck.Solver.InfNat(widthInteger)

-- | Force the evaluation of a word value
forceWordValue :: Backend sym => WordValue sym -> SEval sym ()
forceWordValue (WordVal w)  = seq w (return ())
forceWordValue (ThunkWordVal _ m)  = forceWordValue =<< m
forceWordValue (LargeBitsVal n xs) = mapM_ (\x -> const () <$> x) (enumerateSeqMap n xs)

-- | An arbitrarily-chosen number of elements where we switch from a dense
--   sequence representation of bit-level words to 'SeqMap' representation.
largeBitSize :: Integer
largeBitSize = 1 `shiftL` 48

-- | For efficiency reasons, we handle finite sequences of bits as special cases
--   in the evaluator.  In cases where we know it is safe to do so, we prefer to
--   used a "packed word" representation of bit sequences.  This allows us to rely
--   directly on Integer types (in the concrete evaluator) and SBV's Word types (in
--   the symbolic simulator).
--
--   However, if we cannot be sure all the bits of the sequence
--   will eventually be forced, we must instead rely on an explicit sequence of bits
--   representation.
data WordValue sym
  = ThunkWordVal Integer !(SEval sym (WordValue sym))
  | WordVal !(SWord sym)                      -- ^ Packed word representation for bit sequences.
  | LargeBitsVal !Integer !(SeqMap sym (SBit sym))
                                              -- ^ A large bitvector sequence, represented as a
                                              --   'SeqMap' of bits.
 deriving (Generic)

wordVal :: SWord sym -> WordValue sym
wordVal = WordVal

largeBitsVal :: Backend sym => Integer -> SeqMap sym (SBit sym) -> WordValue sym
largeBitsVal = LargeBitsVal

{-# INLINE joinWordVal #-}
joinWordVal :: Backend sym => sym -> WordValue sym -> WordValue sym -> SEval sym (WordValue sym)
joinWordVal sym (WordVal w1) (WordVal w2)
  | wordLen sym w1 + wordLen sym w2 < largeBitSize
  = WordVal <$> joinWord sym w1 w2

joinWordVal sym (ThunkWordVal _ m1) w2
  = do w1 <- m1
       joinWordVal sym w1 w2

joinWordVal sym w1 (ThunkWordVal _ m2)
  = do w2 <- m2
       joinWordVal sym w1 w2

joinWordVal sym w1 w2
  = pure $ LargeBitsVal (n1+n2) (concatSeqMap n1 (asBitsMap sym w1) (asBitsMap sym w2))
 where n1 = wordValueSize sym w1
       n2 = wordValueSize sym w2


{-# INLINE splitWordVal #-}

splitWordVal ::
  Backend sym =>
  sym ->
  Integer ->
  Integer ->
  WordValue sym ->
  SEval sym (WordValue sym, WordValue sym)
splitWordVal sym leftWidth rightWidth (WordVal w) =
  do (lw, rw) <- splitWord sym leftWidth rightWidth w
     pure (WordVal lw, WordVal rw)

splitWordVal sym leftWidth rightWidth (ThunkWordVal n m)
  | isReady sym m = splitWordVal sym leftWidth rightWidth =<< m
  | otherwise =
      do m' <- sDelay sym (splitWordVal sym leftWidth rightWidth =<< m)
         return (ThunkWordVal n (fst <$> m'), ThunkWordVal n (snd <$> m'))

splitWordVal _ leftWidth rightWidth (LargeBitsVal _n xs) =
  let (lxs, rxs) = splitSeqMap leftWidth xs
   in pure (LargeBitsVal leftWidth lxs, LargeBitsVal rightWidth rxs)

{-# INLINE extractWordVal #-}

-- | Extract a subsequence of bits from a @WordValue@.
--   The first integer argument is the number of bits in the
--   resulting word.  The second integer argument is the
--   number of less-significant digits to discard.  Stated another
--   way, the operation `extractWordVal n i w` is equivalent to
--   first shifting `w` right by `i` bits, and then truncating to
--   `n` bits.
extractWordVal ::
  Backend sym =>
  sym ->
  Integer ->
  Integer ->
  WordValue sym ->
  SEval sym (WordValue sym)
extractWordVal sym len start (WordVal w) =
   WordVal <$> extractWord sym len start w
extractWordVal sym len start (ThunkWordVal n m)
  | isReady sym m = extractWordVal sym len start =<< m
  | otherwise =
      do m' <- sDelay sym (extractWordVal sym len start =<< m)
         pure (ThunkWordVal n m')
extractWordVal _ len start (LargeBitsVal n xs) =
   let xs' = dropSeqMap (n - start - len) xs
    in pure $ LargeBitsVal len xs'

{-# INLINE wordValLogicOp #-}

wordValLogicOp ::
  Backend sym =>
  sym ->
  (SBit sym -> SBit sym -> SEval sym (SBit sym)) ->
  (SWord sym -> SWord sym -> SEval sym (SWord sym)) ->
  WordValue sym ->
  WordValue sym ->
  SEval sym (WordValue sym)
wordValLogicOp _sym _ wop (WordVal w1) (WordVal w2) = WordVal <$> wop w1 w2

wordValLogicOp sym bop wop (ThunkWordVal _ m1) w2 =
  do w1 <- m1
     wordValLogicOp sym bop wop w1 w2

wordValLogicOp sym bop wop w1 (ThunkWordVal _ m2) =
  do w2 <- m2
     wordValLogicOp sym bop wop w1 w2

wordValLogicOp sym bop _ w1 w2 = LargeBitsVal (wordValueSize sym w1) <$> zs
     where zs = memoMap sym $ indexSeqMap $ \i -> join (bop <$> (lookupSeqMap xs i) <*> (lookupSeqMap ys i))
           xs = asBitsMap sym w1
           ys = asBitsMap sym w2

{-# INLINE wordValUnaryOp #-}
wordValUnaryOp ::
  Backend sym =>
  sym ->
  (SBit sym -> SEval sym (SBit sym)) ->
  (SWord sym -> SEval sym (SWord sym)) ->
  WordValue sym ->
  SEval sym (WordValue sym)
wordValUnaryOp _ _ wop (WordVal w)  = WordVal <$> (wop w)
wordValUnaryOp sym bop wop (ThunkWordVal _ m) = wordValUnaryOp sym bop wop =<< m
wordValUnaryOp sym bop _ (LargeBitsVal n xs) = LargeBitsVal n <$> mapSeqMap sym bop xs

{-# SPECIALIZE joinWords ::
  Concrete ->
  Integer ->
  Integer ->
  SeqMap Concrete (WordValue Concrete)->
  SEval Concrete (WordValue Concrete)
  #-}
joinWords :: forall sym.
  Backend sym =>
  sym ->
  Integer ->
  Integer ->
  SeqMap sym (WordValue sym) ->
  SEval sym (WordValue sym)

-- small enough to pack
joinWords sym nParts nEach xs | nParts * nEach < largeBitSize =
  loop (wordVal <$> wordLit sym 0 0) (enumerateSeqMap nParts xs)

 where
 loop :: SEval sym (WordValue sym) -> [SEval sym (WordValue sym)] -> SEval sym (WordValue sym)
 loop !wv [] =
    let len = (nParts * nEach) in
    delayWordValue sym len wv
 loop !wv (w : ws) =
    do w' <- w
       let wv' = join (joinWordVal sym <$> wv <*> pure w')
       loop wv' ws

-- too large to pack
joinWords sym nParts nEach xs =
   return $ LargeBitsVal (nParts * nEach) zs
  where
    zs = indexSeqMap $ \i ->
            do let (q,r) = divMod i nEach
               ys <- lookupSeqMap xs q
               indexWordValue sym ys r

wordValAsLit :: Backend sym => sym -> WordValue sym -> Maybe Integer
wordValAsLit sym (WordVal w) = snd <$> wordAsLit sym w
wordValAsLit _ _ = Nothing

-- | Force a word value into packed word form
asWordVal :: Backend sym => sym -> WordValue sym -> SEval sym (SWord sym)
asWordVal _   (WordVal w)         = return w
asWordVal sym (ThunkWordVal _ m)  = asWordVal sym =<< m
asWordVal sym (LargeBitsVal n xs) = packWord sym =<< sequence (enumerateSeqMap n xs)

asWordList :: forall sym. Backend sym => sym -> [WordValue sym] -> SEval sym (Maybe [SWord sym])
asWordList sym = loop id
 where
   loop :: ([SWord sym] -> [SWord sym]) -> [WordValue sym] -> SEval sym (Maybe [SWord sym])
   loop f [] = pure (Just (f []))
   loop f (WordVal x : vs) = loop (f . (x:)) vs
   loop f (ThunkWordVal _ m : vs)
     | isReady sym m = do m' <- m; loop f (m' : vs)
   loop _ _ = pure Nothing

-- | Force a word value into a sequence of bits
asBitsMap :: Backend sym => sym -> WordValue sym -> SeqMap sym (SBit sym)
asBitsMap sym (WordVal w)         = indexSeqMap $ \i -> wordBit sym w i
asBitsMap sym (ThunkWordVal _ m)  = indexSeqMap $ \i -> do mp <- asBitsMap sym <$> m; lookupSeqMap mp i
asBitsMap _   (LargeBitsVal _ xs) = xs

-- | Turn a word value into a sequence of bits, forcing each bit.
--   The sequence is returned in big-endian order.
enumerateWordValue :: Backend sym => sym -> WordValue sym -> SEval sym [SBit sym]
enumerateWordValue sym (WordVal w) = unpackWord sym w
enumerateWordValue sym (ThunkWordVal _ m) = enumerateWordValue sym =<< m
enumerateWordValue _ (LargeBitsVal n xs)  = sequence (enumerateSeqMap n xs)

-- | Turn a word value into a sequence of bits, forcing each bit.
--   The sequence is returned in reverse of the usual order, which is little-endian order.
enumerateWordValueRev :: Backend sym => sym -> WordValue sym -> SEval sym [SBit sym]
enumerateWordValueRev sym (WordVal w)  = reverse <$> unpackWord sym w
enumerateWordValueRev sym (ThunkWordVal _ m)  = enumerateWordValueRev sym =<< m
enumerateWordValueRev _   (LargeBitsVal n xs) = sequence (enumerateSeqMap n (reverseSeqMap n xs))

{-# SPECIALIZE bitsValueLessThan ::
  Concrete ->
  Integer ->
  [SBit Concrete] ->
  Integer ->
  SEval Concrete (SBit Concrete)
  #-}
bitsValueLessThan ::
  Backend sym =>
  sym ->
  Integer {- ^ bit-width -} ->
  [SBit sym] {- ^ big-endian list of index bits -} ->
  Integer {- ^ Upper bound to test against -} ->
  SEval sym (SBit sym)
bitsValueLessThan sym _w [] _n = pure $ bitLit sym False
bitsValueLessThan sym w (b:bs) n
  | nbit =
      do notb <- bitComplement sym b
         bitOr sym notb =<< bitsValueLessThan sym (w-1) bs n
  | otherwise =
      do notb <- bitComplement sym b
         bitAnd sym notb =<< bitsValueLessThan sym (w-1) bs n
 where
 nbit = testBit n (fromInteger (w-1))


assertWordValueInBounds ::
  Backend sym => sym -> Integer -> WordValue sym -> SEval sym ()

-- Can't index out of bounds for a sequence that is
-- longer than the expressible index values
assertWordValueInBounds sym n idx
  | n >= 2^(wordValueSize sym idx)
  = return ()

assertWordValueInBounds sym n (WordVal idx)
  | Just (_w,i) <- wordAsLit sym idx
  = unless (i < n) (raiseError sym (InvalidIndex (Just i)))

-- If the index is a packed word, test that it
-- is less than the concrete value of n, which
-- fits into w bits because of the above test.
assertWordValueInBounds sym n (WordVal idx) =
  do n' <- wordLit sym (wordLen sym idx) n
     p <- wordLessThan sym idx n'
     assertSideCondition sym p (InvalidIndex Nothing)

-- Force thunks
assertWordValueInBounds sym n (ThunkWordVal _ m) =
  assertWordValueInBounds sym n =<< m

-- If the index is an unpacked word, force all the bits
-- and compute the unsigned less-than test directly.
assertWordValueInBounds sym n (LargeBitsVal w bits) =
  do bitsList <- sequence (enumerateSeqMap w bits)
     p <- bitsValueLessThan sym w bitsList n
     assertSideCondition sym p (InvalidIndex Nothing)


lazyViewWordOrBitsMap ::
  Backend sym =>
  sym ->
  (SWord sym -> SEval sym (WordValue sym)) ->
  (Integer -> SeqMap sym (SBit sym) -> SEval sym (WordValue sym)) ->
  WordValue sym -> SEval sym (WordValue sym)

lazyViewWordOrBitsMap sym wop bop (ThunkWordVal sz m)
  | isReady sym m = viewWordOrBitsMap sym wop bop =<< m
  | otherwise     = delayWordValue sym sz (viewWordOrBitsMap sym wop bop =<< m)

lazyViewWordOrBitsMap _sym wop _bop (WordVal w) =
  wop w
lazyViewWordOrBitsMap _sym _wop bop (LargeBitsVal n bs) =
  bop n bs


viewWordOrBitsMap ::
  Backend sym =>
  sym ->
  (SWord sym -> SEval sym a) ->
  (Integer -> SeqMap sym (SBit sym) -> SEval sym a) ->
  WordValue sym -> SEval sym a
viewWordOrBitsMap sym wop bop (ThunkWordVal _ m) =
  viewWordOrBitsMap sym wop bop =<< m
viewWordOrBitsMap _sym wop _bop (WordVal w) =
  wop w
viewWordOrBitsMap _sym _wop bop (LargeBitsVal n bs) =
  bop n bs

viewWordOrBits ::
  Backend sym =>
  sym ->
  (SWord sym -> SEval sym a) ->
  ([SBit sym] -> SEval sym a) ->
  WordValue sym -> SEval sym a

viewWordOrBits sym wop bop (ThunkWordVal _ m) =
  viewWordOrBits sym wop bop =<< m
viewWordOrBits _sym wop _bop (WordVal w) =
  wop w
viewWordOrBits _sym _wop bop (LargeBitsVal n bs) =
  bop =<< sequence (enumerateSeqMap n bs)

lazyViewWordOrBits ::
  Backend sym =>
  sym ->
  (SWord sym -> SEval sym (WordValue sym)) ->
  ([SBit sym] -> SEval sym (WordValue sym)) ->
  WordValue sym -> SEval sym (WordValue sym)

lazyViewWordOrBits sym wop bop (ThunkWordVal sz m)
  | isReady sym m = viewWordOrBits sym wop bop =<< m
  | otherwise     = delayWordValue sym sz (viewWordOrBits sym wop bop =<< m)

lazyViewWordOrBits _sym wop _bop (WordVal w) =
  wop w
lazyViewWordOrBits _sym _wop bop (LargeBitsVal n bs) =
  bop =<< sequence (enumerateSeqMap n bs)


delayWordValue :: Backend sym => sym -> Integer -> SEval sym (WordValue sym) -> SEval sym (WordValue sym)
delayWordValue sym sz m
  | isReady sym m = m
  | otherwise     = ThunkWordVal sz <$> sDelay sym m


wordShiftByInt ::
  Backend sym =>
  sym ->
  (SWord sym -> SWord sym -> SEval sym (SWord sym)) ->
  (Integer -> Integer -> Maybe Integer) ->
  SInteger sym ->
  WordValue sym ->
  SEval sym (WordValue sym)

wordShiftByInt sym wop reindex idx x =
  case x of
    ThunkWordVal w wm
      | isReady sym wm ->
          wordShiftByInt sym wop reindex idx =<< wm
      | otherwise ->
         do m' <- sDelay sym (wordShiftByInt sym wop reindex idx =<< wm)
            return (ThunkWordVal w m')

    WordVal x' -> WordVal <$> (wop x' =<< wordFromInt sym (wordLen sym x') idx)

    LargeBitsVal n bs0 ->
         do idx_bits <- enumerateIntBits' sym n idx
            LargeBitsVal n <$> barrelShifter sym (iteBit sym) shiftOp bs0 idx_bits

 where
   shiftOp vs shft =
      memoMap sym $ indexSeqMap $ \i ->
        case reindex i shft of
          Nothing -> pure $ bitLit sym False
          Just i' -> lookupSeqMap vs i'




wordShiftByWord ::
  Backend sym =>
  sym ->
  (SWord sym -> SWord sym -> SEval sym (SWord sym)) ->
  (Integer -> Integer -> Maybe Integer) ->
  WordValue sym ->
  WordValue sym ->
  SEval sym (WordValue sym)
wordShiftByWord sym wop reindex idx x =
  case x of
    ThunkWordVal w wm
      | isReady sym wm ->
          wordShiftByWord sym wop reindex idx =<< wm
      | otherwise ->
         do m' <- sDelay sym (wordShiftByWord sym wop reindex idx =<< wm)
            return (ThunkWordVal w m')

    WordVal x' -> WordVal <$> (wop x' =<< asWordVal sym idx)

    LargeBitsVal n bs0 ->
      do idx_bits <- enumerateWordValue sym idx
         LargeBitsVal n <$> barrelShifter sym (iteBit sym) shiftOp bs0 idx_bits

 where
   shiftOp vs shft =
      memoMap sym $ indexSeqMap $ \i ->
        case reindex i shft of
          Nothing -> pure $ bitLit sym False
          Just i' -> lookupSeqMap vs i'


-- | Compute the size of a word value
-- TODO, can we get rid of this? If feels like it should be
--  unnecessary.
wordValueSize :: Backend sym => sym -> WordValue sym -> Integer
wordValueSize sym (WordVal w)  = wordLen sym w
wordValueSize _ (ThunkWordVal n _) = n
wordValueSize _ (LargeBitsVal n _) = n

-- | Select an individual bit from a word value
indexWordValue :: Backend sym => sym -> WordValue sym -> Integer -> SEval sym (SBit sym)
indexWordValue sym (ThunkWordVal _ m) idx = do m' <- m ; indexWordValue sym m' idx
indexWordValue sym (WordVal w) idx
   | 0 <= idx && idx < wordLen sym w = wordBit sym w idx
   | otherwise = invalidIndex sym idx
indexWordValue sym (LargeBitsVal n xs) idx
   | 0 <= idx && idx < n = lookupSeqMap xs idx
   | otherwise = invalidIndex sym idx

-- | Produce a new 'WordValue' from the one given by updating the @i@th bit with the
--   given bit value.
updateWordValue :: Backend sym =>
  sym -> WordValue sym -> Integer -> SEval sym (SBit sym) -> SEval sym (WordValue sym)
updateWordValue sym (WordVal w) idx b
   | idx < 0 || idx >= wordLen sym w = invalidIndex sym idx
   | isReady sym b = WordVal <$> (wordUpdate sym w idx =<< b)

updateWordValue sym wv idx b
   | 0 <= idx && idx < wordValueSize sym wv =
        pure $ LargeBitsVal (wordValueSize sym wv) $ updateSeqMap (asBitsMap sym wv) idx b
   | otherwise = invalidIndex sym idx

-- | Compute the list of bits in an integer in big-endian order.
--   The integer argument is a concrete upper bound for
--   the symbolic integer.
enumerateIntBits' :: Backend sym =>
  sym ->
  Integer ->
  SInteger sym ->
  SEval sym [SBit sym]
enumerateIntBits' sym n idx =
  do w <- wordFromInt sym (widthInteger n) idx
     unpackWord sym w

{-# INLINE mergeWord #-}
mergeWord :: Backend sym =>
  sym ->
  SBit sym ->
  WordValue sym ->
  WordValue sym ->
  SEval sym (WordValue sym)
mergeWord sym c (ThunkWordVal _ m1) (ThunkWordVal _ m2) =
  mergeWord' sym c m1 m2
mergeWord sym c (ThunkWordVal _ m1) w2 =
  mergeWord' sym c m1 (pure w2)
mergeWord sym c w1 (ThunkWordVal _ m2) =
  mergeWord' sym c (pure w1) m2
mergeWord sym c (WordVal w1) (WordVal w2) =
  WordVal <$> iteWord sym c w1 w2
mergeWord sym c w1 w2 =
  LargeBitsVal (wordValueSize sym w1) <$>
    memoMap sym (mergeSeqMap sym (iteBit sym) c (asBitsMap sym w1) (asBitsMap sym w2))

{-# INLINE mergeWord' #-}
mergeWord' :: Backend sym =>
  sym ->
  SBit sym ->
  SEval sym (WordValue sym) ->
  SEval sym (WordValue sym) ->
  SEval sym (WordValue sym)
mergeWord' sym = mergeEval sym (mergeWord sym)
