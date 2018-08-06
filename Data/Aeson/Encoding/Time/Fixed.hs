{-# LANGUAGE BangPatterns #-}

-- |
-- Module:      Data.Aeson.Encoding.Time.Fixed
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2013 Simon Meier <iridcode@gmail.com>
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize date/time values to fixed-length Builders.

module Data.Aeson.Encoding.Time.Fixed
    (
      day
    , localTime
    , utcTime
    , timeOfDay
    , zonedTime
    , timeZone
    ) where

import Data.Aeson.Internal.Time
import Data.ByteString.Builder as B
import Data.ByteString.Builder.Prim as BP
import Data.Char (chr)
import Data.Semigroup ((<>))
import Data.Time (UTCTime(..))
import Data.Time.Calendar (Day(..), toGregorian)
import Data.Time.LocalTime


ascii4 :: (Char, (Char, (Char, Char))) -> BP.BoundedPrim a
ascii4 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii4 #-}

ascii5 :: (Char, (Char, (Char, (Char, Char)))) -> BP.BoundedPrim a
ascii5 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii5 #-}

ascii6 :: (Char, (Char, (Char, (Char, (Char, Char))))) -> BP.BoundedPrim a
ascii6 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii6 #-}

ascii12 :: (Char, (Char, (Char, (Char, (Char, (Char, (Char, (Char, (Char, (Char, (Char, Char)))))))))))
        -> BoundedPrim a
ascii12 cs = BP.liftFixedToBounded $ const cs >$<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7 >*<
    BP.char7 >*< BP.char7 >*< BP.char7 >*< BP.char7
{-# INLINE ascii12 #-}

day :: Day -> Builder
day dd = encodeYear yr <>
         BP.primBounded (ascii6 ('-',(mh,(ml,('-',(dh,dl)))))) ()
  where (yr,m,d)    = toGregorian dd
        !(T mh ml)  = twoDigits m
        !(T dh dl)  = twoDigits d
        encodeYear y
            | y >= 1000 = B.integerDec y
            | y >= 0    = BP.primBounded (ascii4 (padYear y)) ()
            | y >= -999 = BP.primBounded (ascii5 ('-',padYear (- y))) ()
            | otherwise = B.integerDec y
        padYear y =
            let (ab,c) = fromIntegral y `quotRem` 10
                (a,b)  = ab `quotRem` 10
            in ('0',(digit a,(digit b,digit c)))
{-# INLINE day #-}

timeOfDay :: TimeOfDay -> Builder
timeOfDay t = timeOfDay64 (toTimeOfDay64 t)
{-# INLINE timeOfDay #-}

timeOfDay64 :: TimeOfDay64 -> Builder
timeOfDay64 (TOD h m s)
  = hhmmssSSS
  where
    hhmmssSSS  = BP.primBounded (ascii12 (hh,(hl,(':',(mh,(ml,(':',(sh,(sl,('.',(ms1,(ms2,ms3)))))))))))) ()
    !(T hh hl)  = twoDigits h
    !(T mh ml)  = twoDigits m
    !(T sh sl)  = twoDigits (fromIntegral real)
    !(T3 ms1 ms2 ms3) = threeDigits (fromIntegral (frac `div` 1000000000))
    (real,frac) = s `quotRem` pico
    pico       = 1000000000000 -- number of picoseconds  in 1 second

timeZone :: TimeZone -> Builder
timeZone (TimeZone off _ _)
  = BP.primBounded (ascii6 (s,(hh,(hl,(':',(mh,ml)))))) ()
  where !s         = if off < 0 then '-' else '+'
        !(T hh hl) = twoDigits h
        !(T mh ml) = twoDigits m
        (h,m)      = abs off `quotRem` 60
{-# INLINE timeZone #-}

dayTime :: Day -> TimeOfDay64 -> Builder
dayTime d t = day d <> B.char7 'T' <> timeOfDay64 t
{-# INLINE dayTime #-}

utcTime :: UTCTime -> B.Builder
utcTime (UTCTime d s) = dayTime d (diffTimeOfDay64 s) <> B.char7 'Z'
{-# INLINE utcTime #-}

localTime :: LocalTime -> Builder
localTime (LocalTime d t) = dayTime d (toTimeOfDay64 t)
{-# INLINE localTime #-}

zonedTime :: ZonedTime -> Builder
zonedTime (ZonedTime t z) = localTime t <> timeZone z
{-# INLINE zonedTime #-}

data T = T {-# UNPACK #-} !Char {-# UNPACK #-} !Char

twoDigits :: Int -> T
twoDigits a     = T (digit hi) (digit lo)
  where (hi,lo) = a `quotRem` 10

data T3 = T3 {-# UNPACK #-} !Char {-# UNPACK #-} !Char {-# UNPACK #-} !Char

threeDigits :: Int -> T3
threeDigits a = T3 (digit hi) (digit mid) (digit lo)
  where (r, lo) = a `quotRem` 10
        (hi,mid) = r `quotRem` 10

digit :: Int -> Char
digit x = chr (x + 48)
