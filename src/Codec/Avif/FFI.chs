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
                      , avifRGBImageAllocatePixels
                      , avifRGBImageFreePixels
                      , AvifPixelFormat (..)
                      , AvifResult (..)
                      ) where

import Control.Exception (Exception)
import Data.Coerce (coerce)
import Data.Typeable (Typeable)
import Foreign.C.Types (CInt, CSize)
import Foreign.Ptr (castPtr, Ptr)

#include <avif/avif.h>

type UInt8 = {# type uint8_t #}; type UInt32 = {# type uint32_t #}

{# enum avifPixelFormat as AvifPixelFormat {underscoreToCase} #}

-- | @since 0.1.2.0
{# enum avifResult as AvifResult {underscoreToCase} deriving (Eq, Show, Typeable) #}

instance Exception AvifResult where

{# pointer *avifImage as AvifImagePtr foreign finalizer avifImageDestroy as ^ -> AvifImage #}

data AvifImage; data AvifEncoder; data AvifDecoder; data AvifRwData; data AvifRGBImage

{# pointer *avifEncoder as AvifEncoderPtr foreign finalizer avifEncoderDestroy as ^ -> AvifEncoder #}
{# pointer *avifDecoder as AvifDecoderPtr foreign finalizer avifDecoderDestroy as ^ -> AvifDecoder #}

{# fun avifImageCreate as ^ { `CInt', `CInt', `CInt', `AvifPixelFormat' } -> `Ptr AvifImage' id #}
{# fun avifImageCreateEmpty as ^ { } -> `Ptr AvifImage' id #}

{# fun avifEncoderCreate as ^ { } -> `Ptr AvifEncoder' castPtr #}
{# fun avifEncoderWrite as ^ { `AvifEncoderPtr', `AvifImagePtr', castPtr `Ptr AvifRwData' } -> `AvifResult' #}

{# fun avifDecoderCreate as ^ { } -> `Ptr AvifDecoder' castPtr #}
{# fun avifDecoderReadMemory as ^ { `AvifDecoderPtr', `AvifImagePtr', castPtr `Ptr UInt8', coerce `CSize' } -> `AvifResult' #}

{# fun avifRGBImageSetDefaults as ^ { castPtr `Ptr AvifRGBImage', `AvifImagePtr' } -> `()' #}
{# fun avifRGBImagePixelSize as ^ { castPtr `Ptr AvifRGBImage' } -> `UInt32' id #}

{# fun avifImageRGBToYUV as ^ { `AvifImagePtr', castPtr `Ptr AvifRGBImage' } -> `AvifResult' #}
{# fun avifImageYUVToRGB as ^ { `AvifImagePtr', castPtr `Ptr AvifRGBImage' } -> `AvifResult' #}

{# fun avifRGBImageAllocatePixels as ^ { castPtr `Ptr AvifRGBImage' } -> `()' #}
{# fun avifRGBImageFreePixels as ^ { castPtr `Ptr AvifRGBImage' } -> `()' #}
