module Codec.Avif ( encode
                  , decode
                  ) where

import Codec.Avif.FFI
import Codec.Picture (Image (Image), PixelRGBA8, PixelYCbCr8)
import Control.Exception (throwIO)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (memcpy)
import qualified Data.ByteString.Unsafe as BS
import Foreign.Ptr (castPtr)
import Foreign.ForeignPtr (castForeignPtr, newForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal (allocaBytes)
import qualified Data.Vector.Storable as VS
import System.IO.Unsafe (unsafePerformIO)

#include <avif/avif.h>

-- PixelYCbCr8?

throwRes :: AvifResult -> IO ()
throwRes AvifResultOk = pure (); throwRes err = throwIO err

{-# NOINLINE encode #-}
encode :: Image PixelRGBA8 -> BS.ByteString
encode img = unsafePerformIO $ do
    avifImgPtr <- avifImageCreate (fromIntegral w) (fromIntegral h) 8 AvifPixelFormatYuv444
    avifImg <- castForeignPtr <$> newForeignPtr avifImageDestroy (castPtr avifImgPtr)
    res <- allocaBytes {# sizeof avifRGBImage #} $ \rgbImagePtr ->
        allocaBytes {# sizeof avifRWData #} $ \rwDataPtr -> do
            avifRGBImageSetDefaults rgbImagePtr avifImg
            pxSz <- avifRGBImagePixelSize rgbImagePtr

            preEnc <- avifEncoderCreate
            enc <- castForeignPtr <$> newForeignPtr avifEncoderDestroy (castPtr preEnc)

            withForeignPtr imgPtr $ \iPtr -> do

                {# set avifRGBImage.height #} rgbImagePtr (fromIntegral h)
                {# set avifRGBImage.width #} rgbImagePtr (fromIntegral w)
                {# set avifRGBImage.pixels #} rgbImagePtr (castPtr iPtr)
                {# set avifRGBImage.rowBytes #} rgbImagePtr (fromIntegral w*pxSz)

                throwRes =<< avifImageRGBToYUV avifImg rgbImagePtr

                throwRes =<< avifEncoderWrite enc avifImg rwDataPtr

                sz <- {# get avifRWData->size #} rwDataPtr
                bs <- {# get avifRWData->data #} rwDataPtr

                BS.packCStringLen (castPtr bs, fromIntegral sz)

    pure res

    where (Image w h bytes) = img
          (imgPtr, _) = VS.unsafeToForeignPtr0 bytes

{-# NOINLINE decode #-}
decode :: BS.ByteString -> Image PixelRGBA8
decode bs = unsafePerformIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, sz) -> do
    preDec <- avifDecoderCreate
    dec <- castForeignPtr <$> newForeignPtr avifDecoderDestroy (castPtr preDec)

    avifImgPtr <- avifImageCreateEmpty
    avifImg <- castForeignPtr <$> newForeignPtr avifImageDestroy (castPtr avifImgPtr)

    throwRes =<< avifDecoderReadMemory dec avifImg (castPtr ptr) (fromIntegral sz)

    allocaBytes {# sizeof avifRGBImage #} $ \rgbImagePtr -> do
        avifRGBImageSetDefaults rgbImagePtr avifImg
        avifRGBImageAllocatePixels rgbImagePtr
        throwRes =<< avifImageYUVToRGB avifImg rgbImagePtr

        w <- {# get avifRGBImage->width #} rgbImagePtr
        h <- {# get avifRGBImage->height #} rgbImagePtr
        pxSz <- avifRGBImagePixelSize rgbImagePtr

        pxPtr <- {# get avifRGBImage->pixels #} rgbImagePtr

        let sz' = w*h*pxSz

        outBytes <- mallocForeignPtrBytes (fromIntegral sz')

        withForeignPtr outBytes $ \outPtr -> do
            memcpy (castPtr outPtr) (castPtr pxPtr) (fromIntegral sz')
            Image (fromIntegral w) (fromIntegral h) (VS.unsafeFromForeignPtr0 outBytes (fromIntegral sz')) <$ (avifRGBImageFreePixels rgbImagePtr)
