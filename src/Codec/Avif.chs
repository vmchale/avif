module Codec.Avif ( encode
                  , decode
                  ) where

import Codec.Avif.FFI
import Codec.Picture (Image (Image), PixelRGBA16, PixelYCbCr8)
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
throwRes AvifResultOk = pure ()
throwRes err          = throwIO err

encode :: Image PixelRGBA16 -> BS.ByteString
encode img = unsafePerformIO $ do
    avifImgPtr <- avifImageCreate (fromIntegral w) (fromIntegral h) 8 AvifPixelFormatYuv444
    res <- allocaBytes {# sizeof avifRGBImage #} $ \rgbImagePtr ->
        allocaBytes {# sizeof avifRWData #} $ \rwDataPtr -> do
            avifRGBImageSetDefaults rgbImagePtr avifImgPtr

            preEnc <- avifEncoderCreate
            enc <- castForeignPtr <$> newForeignPtr avifEncoderDestroy (castPtr preEnc)

            withForeignPtr imgPtr $ \iPtr -> do

                {# set avifRGBImage.pixels #} rgbImagePtr (castPtr iPtr)

                throwRes =<< avifEncoderWrite enc avifImgPtr rwDataPtr

                sz <- {# get avifRWData->size #} rwDataPtr
                bs <- {# get avifRWData->data #} rwDataPtr

                BS.packCStringLen (castPtr bs, fromIntegral sz)

    res <$ avifImageDestroy avifImgPtr

    where (Image w h bytes) = img
          (imgPtr, _) = VS.unsafeToForeignPtr0 bytes

decode :: BS.ByteString -> Image PixelRGBA16
decode bs = unsafePerformIO $ BS.unsafeUseAsCStringLen bs $ \(ptr, sz) -> do
    preDec <- avifDecoderCreate
    dec <- castForeignPtr <$> newForeignPtr avifDecoderDestroy (castPtr preDec)
    avifImg <- avifImageCreateEmpty

    throwRes =<< avifDecoderReadMemory dec avifImg (castPtr ptr) (fromIntegral sz)

    allocaBytes {# sizeof avifRGBImage #} $ \rgbImagePtr -> do
        avifImageYUVToRGB avifImg rgbImagePtr

        w <- {# get avifRGBImage->width #} rgbImagePtr
        h <- {# get avifRGBImage->height #} rgbImagePtr
        pxSz <- avifRGBImagePixelSize rgbImagePtr

        pxPtr <- {# get avifRGBImage->pixels #} rgbImagePtr

        let sz' = w * h * pxSz

        outBytes <- mallocForeignPtrBytes (fromIntegral pxSz)

        withForeignPtr outBytes $ \outPtr -> do
            memcpy (castPtr outPtr) (castPtr pxPtr) (fromIntegral sz')
            Image (fromIntegral w) (fromIntegral h) (VS.unsafeFromForeignPtr0 outBytes (fromIntegral sz')) <$ avifImageDestroy avifImg
