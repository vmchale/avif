module Codec.Avif ( encode
                  , decode
                  , decodeE
                  , AvifResult (..)
                  , RgbImage (..)
                  ) where

import Codec.Avif.FFI
import Codec.Picture (Image (Image), PixelRGBA8, PixelRGBA16)
import Control.DeepSeq (NFData (rnf), deepseq)
import Control.Exception (throw, throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Foreign.Ptr (castPtr)
import Foreign.ForeignPtr (castForeignPtr, newForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal (allocaBytes)
import Foreign.Marshal.Utils (copyBytes)
import qualified Data.Vector.Storable as VS
import System.IO.Unsafe (unsafePerformIO)

#include <avif/avif.h>

throwRes :: AvifResult -> IO ()
throwRes AvifResultOk = pure (); throwRes err = throwIO err

mDo AvifResultOk x = x; mDo e _ = pure (Left e)

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

-- | Cf. 'Codec.Picture.DynamicImage'
data RgbImage = ImageRGBA8 (Image PixelRGBA8) | ImageRGBA16 (Image PixelRGBA16)

instance NFData RgbImage where
    rnf (ImageRGBA8 img)  = img `deepseq` ()
    rnf (ImageRGBA16 img) = img `deepseq` ()

decode :: BS.ByteString -> RgbImage
decode = either throw id.decodeE

{-# NOINLINE decodeE #-}
-- | @since 0.1.2.0
decodeE :: BS.ByteString -> Either AvifResult RgbImage
decodeE bs = unsafePerformIO $ BS.unsafeUseAsCStringLen bs $ \(p, sz) -> do
    preDec <- avifDecoderCreate
    dec <- castForeignPtr <$> newForeignPtr avifDecoderDestroy (castPtr preDec)

    avifImgPtr <- avifImageCreateEmpty
    avifImg <- castForeignPtr <$> newForeignPtr avifImageDestroy (castPtr avifImgPtr)

    m <- avifDecoderReadMemory dec avifImg (castPtr p) (fromIntegral sz)

    mDo m $ do

        allocaBytes {# sizeof avifRGBImage #} $ \rgbImagePtr -> do
            avifRGBImageSetDefaults rgbImagePtr avifImg
            avifRGBImageAllocatePixels rgbImagePtr
            r <- avifImageYUVToRGB avifImg rgbImagePtr

            mDo r $ do

                w <- {# get avifRGBImage->width #} rgbImagePtr
                h <- {# get avifRGBImage->height #} rgbImagePtr
                pxSz <- avifRGBImagePixelSize rgbImagePtr

                pxPtr <- {# get avifRGBImage->pixels #} rgbImagePtr

                let sz' = fromIntegral (w*h*pxSz) :: Int

                outBytes <- mallocForeignPtrBytes sz'

                withForeignPtr outBytes $ \outPtr -> do
                    copyBytes (castPtr outPtr) (castPtr pxPtr) sz'
                    let img | pxSz==8 = ImageRGBA16 (Image (fromIntegral w) (fromIntegral h) (VS.unsafeFromForeignPtr0 (castForeignPtr outBytes) sz'))
                            | pxSz==4 = ImageRGBA8 (Image (fromIntegral w) (fromIntegral h) (VS.unsafeFromForeignPtr0 outBytes sz'))
                    Right img <$ avifRGBImageFreePixels rgbImagePtr
