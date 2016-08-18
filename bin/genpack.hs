#!/usr/bin/env runghc

import Data.Bits (shiftL)
import Data.Monoid ((<>))
import Data.Word (Word64)
import System.IO (IOMode(..), withFile, hPutStrLn)
import Text.Printf (printf)

inputCount :: Int
inputCount =
  64

wordsReq :: Int -> Int
wordsReq bits =
  (inputCount * bits + 63) `div` 64

bytesReq :: Int -> Int
bytesReq bits =
  if inputCount * bits `mod` 8 /= 0 then
    error "bytesReq"
  else
    (inputCount * bits + 7) `div` 8

packFnName :: Int -> String
packFnName bits =
  "pack64_" <> show inputCount <> "_" <> show bits

unpackFnName :: Int -> String
unpackFnName bits =
  "unpack64_" <> show inputCount <> "_" <> show bits

packFn :: Int -> [String] -> [String]
packFn bits body =
  [ "/* pack " <> show inputCount <> " x " <> show bits <> "-bit integers */"
  , "static void " <> packFnName bits <> " (const uint64_t **pin, uint8_t **pout) {" ] <>
  fmap ("  " <>) body <>
  [ "}" ]

unpackFn :: Int -> [String] -> [String]
unpackFn bits body =
  [ "/* unpack " <> show inputCount <> " x " <> show bits <> "-bit integers */"
  , "static void " <> unpackFnName bits <> " (const uint8_t **pin, uint64_t **pout) {" ] <>
  fmap ("  " <>) body <>
  [ "}" ]

fstWord :: Int -> Int -> Int
fstWord bits i =
  i * bits `div` 64

sndWord :: Int -> Int -> Int
sndWord bits i =
  (i * bits + bits - 1) `div` 64

fstShift :: Int -> Int -> Int
fstShift bits i =
  (i * bits) `mod` 64

sndShift :: Int -> Int -> Int
sndShift bits i =
  64 - fstShift bits i

pack :: Int -> [String]
pack bits =
  let
    nwords = wordsReq bits
    nbytes = bytesReq bits

    defWord w =
      "uint64_t out" <> show w <> ";"

    copyWord w =
      "pout64[" <> show w <> "] = out" <> show w <> ";"

    packWordFrom i =
      let
        out0 = "out" <> show (fstWord bits i)
        out1 = "out" <> show (sndWord bits i)

        shift0 = fstShift bits i
        shift1 = sndShift bits i

        inp = "(uint64_t) in[" <> show i <> "]"
      in
        if out0 == out1 then
          if shift0 == 0 then
            [ out0 <> "  = " <> inp <> ";" ]
          else
            [ out0 <> " |= " <> inp <> " << " <> show shift0 <> ";" ]
        else
          [ out0 <> " |= " <> inp <> " << " <> show shift0 <> ";"
          , out1 <> "  = " <> inp <> " >> " <> show shift1 <> ";" ]
  in
    packFn bits $
      [ "uint64_t *pout64 = *(uint64_t **) pout;"
      , "const uint64_t *in = *pin;"
      , "/* packing in to " <>
          show nwords <> " words / " <>
          show nbytes <> " bytes */"
      ] <>
      fmap defWord [0..nwords-1] <>
      concatMap packWordFrom [0..inputCount-1] <>
      fmap copyWord [0..nwords-1] <>
      [ "*pin  += " <> show inputCount <> "; " <>
          "/* consumed " <> show inputCount <> " integers */"
      , "*pout += " <> show nbytes <> "; " <>
          "/* produced " <> show nbytes <> " bytes */"
      ]

unpack :: Int -> [String]
unpack bits =
  let
    nwords = wordsReq bits
    nbytes = bytesReq bits

    mask :: Word64
    mask =
      (1 `shiftL` bits) - 1

    (applyMask, defMask) =
      if bits < 64 then
        (" & mask", [ printf "const uint64_t mask = UINT64_C(0x%0X);" mask ])
      else
        ("", [])

    defCopyWord w =
      "uint64_t in" <> show w <> " = pin64[" <> show w <> "];"

    unpackWordFrom i =
      let
        in0 = "in" <> show (fstWord bits i)
        in1 = "in" <> show (sndWord bits i)

        shift0 = fstShift bits i
        shift1 = sndShift bits i

        applyShift0 =
          if shift0 == 0 then
            ""
          else
            " >> " <> show shift0

        applyShift1 =
          " << " <> show shift1

        out = "out[" <> show i <> "]"
      in
        if in0 == in1 then
          if shift0 + bits == 64 then
            out <> " = (uint64_t) (" <> in0 <> applyShift0 <> ");"
          else
            out <> " = (uint64_t) ((" <> in0 <> applyShift0 <> ")" <> applyMask <> ");"
        else
          out <> " = (uint64_t) (((" <>
            in0 <> applyShift0 <> ") | (" <>
            in1 <> applyShift1 <> "))" <>
            applyMask <> ");"
  in
    unpackFn bits $
      [ "const uint64_t *pin64 = *(const uint64_t **) pin;"
      , "uint64_t *out = *pout;"
      ] <>
      defMask <>
      [ "/* unpacking from " <>
          show nwords <> " words / " <>
          show nbytes <> " bytes */"
      ] <>
      fmap defCopyWord [0..nwords-1] <>
      [ "*pin += " <> show nbytes <> "; " <>
          "/* consumed " <> show nbytes <> " bytes */"
      ] <>
      fmap unpackWordFrom [0..inputCount-1] <>
      [ "*pout += " <> show inputCount <> "; " <>
          "/* produced " <> show inputCount <> " integers */"
      ]

main :: IO ()
main = do
  withFile "cbits/anemone_pack.c" WriteMode $ \h -> do
    hPutStrLn h . unlines $
      [ "#include <stdint.h>"
      , "#include <string.h>"
      ]

    hPutStrLn h . unlines $
      [ "typedef void (*pack64fn)(const uint64_t **pin, uint8_t **pout);"
      , "typedef void (*unpack64fn)(const uint8_t **pin, uint64_t **pout);"
      ]

    hPutStrLn h . unlines . packFn 0 $
      [ "(void) pout;"
      , "*pin  += " <> show inputCount <> "; " <>
          "/* consumed " <> show inputCount <> " integers */"
      ]

    mapM_ (hPutStrLn h . unlines . pack) [1..64]

    hPutStrLn h . unlines . unpackFn 0 $
      [ "(void) pin;"
      , "memset (*pout, 0, " <> show (inputCount * 8) <> ");"
      , "*pout += " <> show inputCount <> "; " <>
          "/* produced " <> show inputCount <> " integers */"
      ]

    mapM_ (hPutStrLn h . unlines . unpack) [1..64]

    hPutStrLn h . unlines $
      [ "static pack64fn pack64_" <> show inputCount <> "_table[] = {" ] <>
      [ "  &" <> packFnName bits <> "," | bits <- [0..63] ] <>
      [ "  &" <> packFnName 64
      , "};" ]

    hPutStrLn h . unlines $
      [ "static unpack64fn unpack64_" <> show inputCount <> "_table[] = {" ] <>
      [ "  &" <> unpackFnName bits <> "," | bits <- [0..63] ] <>
      [ "  &" <> unpackFnName 64
      , "};" ]

    hPutStrLn h . unlines $
      [ "/* write |count| blocks of " <> show inputCount <> " x 64-bit values from |in| at |bits| bits per value to |out|. */"
      , "uint64_t anemone_pack64_" <> show inputCount <> " (uint64_t blocks, const uint64_t bits, const uint64_t *in, uint8_t *out) {"
      , "  if (bits > 64) return 1;"
      , "  pack64fn pack = pack64_" <> show inputCount <> "_table[bits];"
      , "  for (uint64_t b = 0; b < blocks; b++) {"
      , "      pack (&in, &out);"
      , "  }"
      , "  return 0;"
      , "}" ]

    hPutStrLn h . unlines $
      [ "/* read |count| blocks of " <> show inputCount <> " values from |in| at |bits| bits per value, and write 64-bit values to |out|. */"
      , "uint64_t anemone_unpack64_" <> show inputCount <> " (uint64_t blocks, const uint64_t bits, const uint8_t *in, uint64_t *out) {"
      , "  if (bits > 64) return 1;"
      , "  unpack64fn unpack = unpack64_" <> show inputCount <> "_table[bits];"
      , "  for (uint64_t b = 0; b < blocks; b++) {"
      , "    unpack (&in, &out);"
      , "  }"
      , "  return 0;"
      , "}" ]

    hPutStrLn h . unlines $
      [ "/* calculate number of bits required to store |value|. */"
      , "uint64_t anemone_bitsof (uint64_t value) {"
      , "  return value ? 64 - __builtin_clzll (value) : 0;"
      , "}" ]

  withFile "cbits/anemone_pack.h" WriteMode $ \h -> do
    hPutStrLn h . unlines $
      [ "#ifndef __ANEMONE_PACK_H"
      , "#define __ANEMONE_PACK_H"
      , ""
      , "#include <stdint.h>"
      , ""
      , "uint64_t anemone_pack64_" <> show inputCount <> " (uint64_t blocks, const uint64_t bits, const uint64_t *in, uint8_t *out);"
      , ""
      , "uint64_t anemone_unpack64_" <> show inputCount <> " (uint64_t blocks, const uint64_t bits, const uint8_t *in, uint64_t *out);"
      , ""
      , "#endif//__ANEMONE_PACK_H"
      ]
