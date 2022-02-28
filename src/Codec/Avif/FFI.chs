{-# LANGUAGE DeriveDataTypeable #-}

module Codec.Avif.FFI ( avifImageCreate
                      , avifImageCreateEmpty
                      , avifImageDestroy
                      , avifEncoderCreate
                      , avifEncoderDestroy
                      , avifEncoderWrite
                      , avifDecoderCreate
                      , avifDecoderDestroy
                      , avifDecoderReadMemory
                      , avifRGBImageSetDefaults
                      , avifRGBImagePixelSize
                      , avifImageRGBToYUV
                      , avifImageYUVToRGB
                      , avifRGBImageFreePixels
                      , AvifPixelFormat (..)
                      , AvifResult (..)
                      ) where

import Control.Exception (Exception)
import Data.Coerce (coerce)
import Data.Typeable (Typeable (..))
import Foreign.C.Types (CInt, CSize)
import Foreign.Ptr (castPtr, Ptr)

#include <avif/avif.h>

type UInt8 = {# type uint8_t #}
type UInt32 = {# type uint32_t #}

{# enum avifPixelFormat as AvifPixelFormat {underscoreToCase} #}
{# enum avifResult as AvifResult {underscoreToCase} deriving (Eq, Show, Typeable) #}

instance Exception AvifResult where

-- {# pointer *LZ_Decoder as LZDecoderPtr foreign finalizer LZ_decompress_close as ^ -> LZDecoder #}
data AvifImage -- TODO: finalizer for AvifImage
data AvifEncoder
data AvifDecoder
data AvifRwData
data AvifRGBImage

{# pointer *avifEncoder as AvifEncoderPtr foreign finalizer avifEncoderDestroy as ^ -> AvifEncoder #}
{# pointer *avifDecoder as AvifDecoderPtr foreign finalizer avifDecoderDestroy as ^ -> AvifDecoder #}

{# fun avifImageCreate as ^ { `CInt', `CInt', `CInt', `AvifPixelFormat' } -> `Ptr AvifImage' castPtr #}
{# fun avifImageCreateEmpty as ^ { } -> `Ptr AvifImage' castPtr #}
{# fun avifImageDestroy as ^ { castPtr `Ptr AvifImage' } -> `()' #}

{# fun avifEncoderCreate as ^ { } -> `Ptr AvifEncoder' castPtr #}
{# fun avifEncoderWrite as ^ { `AvifEncoderPtr', castPtr `Ptr AvifImage', castPtr `Ptr AvifRwData' } -> `AvifResult' #}

{# fun avifDecoderCreate as ^ { } -> `Ptr AvifDecoder' castPtr #}
{# fun avifDecoderReadMemory as ^ { `AvifDecoderPtr', castPtr `Ptr AvifImage', castPtr `Ptr UInt8', coerce `CSize' } -> `AvifResult' #}

{# fun avifRGBImageSetDefaults as ^ { castPtr `Ptr AvifRGBImage', castPtr `Ptr AvifImage' } -> `()' #}
{# fun avifRGBImagePixelSize as ^ { castPtr `Ptr AvifRGBImage' } -> `UInt32' id #}

{# fun avifImageRGBToYUV as ^ { castPtr `Ptr AvifImage', castPtr `Ptr AvifRGBImage' } -> `()' #}
{# fun avifImageYUVToRGB as ^ { castPtr `Ptr AvifImage', castPtr `Ptr AvifRGBImage' } -> `()' #}

{# fun avifRGBImageFreePixels as ^ { castPtr `Ptr AvifRGBImage' } -> `()' #}
