{-|
Module      : Network.UHD.Internal
Copyright   : Travis Whitaker 2026
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable (Windows, POSIX)

This module provides the guts of the bindings. You should only have to use this
if you want direct access to the underlying UHD handles for some reason.
-}

{-# LANGUAGE BangPatterns
           , CApiFFI
           , DataKinds
           , DeriveAnyClass
           , DeriveGeneric
           , DerivingStrategies
           , FlexibleInstances
           , GADTs
           , GeneralizedNewtypeDeriving
           , LambdaCase
           , MagicHash
           , RecordWildCards
           , RoleAnnotations
           , ScopedTypeVariables
           , TypeFamilies
           , UnliftedFFITypes
           #-}

module Network.UHD.Internal where

import Control.Exception (Exception(..), throwIO)

import Control.Monad (when)

import Data.Complex

import Data.Data

import Data.Fixed

import Data.Foldable

import Data.Int

import Data.Time.Clock

import Data.Word

import Foreign.C.ConstPtr
import Foreign.C.String
import Foreign.C.Types

import Foreign.Concurrent
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr

import GHC.Base ()

import GHC.Exts

import GHC.Generics

import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS

#include <uhd/usrp/usrp.h>

-- | Get the foreign pointer out of a storable vector.
vsFP :: VS.Vector a -> ForeignPtr a
vsFP = fst . VS.unsafeToForeignPtr0

-- | Touch the foreign pointer inside a storable vector.
touchVS :: VS.Vector a -> IO ()
touchVS = touchForeignPtr . vsFP
    
-- | Errors originating from UHD itself. These will be thrown as Haskell
--   exceptions from higher-level functions.
data UHDError = InvalidDevice
              | IndexError
              | KeyError
              | NotImplemented
              | USBError
              | IOError
              | OSError
              | Assertion
              | LookupError
              | TypeError
              | ValueError
              | RuntimeError
              | EnvironmentError
              | SystemError
              | UHDExcept
              | BoostExcept
              | StdExcept
              | UnknownError
              deriving stock ( Eq
                             , Ord
                             , Enum
                             , Read
                             , Show
                             , Generic
                             )
              deriving anyclass (Exception)

newtype {-# CTYPE "uhd/error.h" "uhd_error" #-}
    UHDErrorEnum = UHDErrorEnum { fromUHDErrorEnum :: CInt }
        deriving newtype (Storable)

toUHDError :: CInt -> Maybe UHDError
toUHDError #{const UHD_ERROR_NONE} = Nothing
toUHDError #{const UHD_ERROR_INVALID_DEVICE} = Just InvalidDevice
toUHDError #{const UHD_ERROR_INDEX} = Just IndexError
toUHDError #{const UHD_ERROR_KEY} = Just KeyError
toUHDError #{const UHD_ERROR_NOT_IMPLEMENTED} = Just NotImplemented
toUHDError #{const UHD_ERROR_USB} = Just USBError
toUHDError #{const UHD_ERROR_IO} = Just IOError
toUHDError #{const UHD_ERROR_OS} = Just OSError
toUHDError #{const UHD_ERROR_ASSERTION} = Just Assertion
toUHDError #{const UHD_ERROR_LOOKUP} = Just LookupError
toUHDError #{const UHD_ERROR_TYPE} = Just TypeError
toUHDError #{const UHD_ERROR_VALUE} = Just ValueError
toUHDError #{const UHD_ERROR_RUNTIME} = Just RuntimeError
toUHDError #{const UHD_ERROR_ENVIRONMENT} = Just EnvironmentError
toUHDError #{const UHD_ERROR_SYSTEM} = Just SystemError
toUHDError #{const UHD_ERROR_EXCEPT} = Just UHDExcept
toUHDError #{const UHD_ERROR_BOOSTEXCEPT} = Just BoostExcept
toUHDError #{const UHD_ERROR_STDEXCEPT} = Just StdExcept
toUHDError _ = Just UnknownError

-- | Throw any UHD error as an exception.
throwOnUHDError :: IO UHDErrorEnum -> IO ()
throwOnUHDError m =
    ((toUHDError . fromUHDErrorEnum) <$> m) >>= traverse_ throwIO

-- | Get an ascii string from a UHD function of a max size. We have to copy
--   it to an intermediate buffer to ensure that it's null terminated. Why
--   doesn't base have this?
getUHDString :: Int -> (Ptr CChar -> CSize -> IO UHDErrorEnum) -> IO String
getUHDString bl _ | bl <= 0 = pure ""
getUHDString bl f =
    let go !l !n !i !o
          | l == n = poke o 0
          | otherwise = do
              x <- peek i
              poke o x
              if x == 0
              then pure ()
              else go l (n+1) (plusPtr i 1) (plusPtr o 1)
    in allocaBytes bl $ \ip ->
       allocaBytes bl $ \op -> do
          throwOnUHDError $ f ip (fromIntegral bl)
          go bl 0 ip op
          peekCString op

getUHDString_ :: Int -> (Ptr CChar -> CSize -> IO ()) -> IO String
getUHDString_ bl f = getUHDString bl
    (\p s -> f p s *> pure (UHDErrorEnum #{const UHD_ERROR_NONE}))

foreign import capi safe "uhd/error.h uhd_get_last_error"
    uhd_get_last_error :: Ptr CChar -> CSize -> IO UHDErrorEnum

-- | Get a human readable description of the last error that occured in UHD
--   itself. This is thread safe in the sense that UHD's updates to this string
--   are atomic (i.e. you won't get garbled text). However, if you're calling
--   UHD functions from multiple Haskell threads, it won't be straightforward to
--   work out which one caused the error observed here.
getLastErrorString :: IO String
getLastErrorString = getUHDString 2048 uhd_get_last_error

foreign import capi safe "uhd/version.h uhd_get_abi_string"
    uhd_get_abi_string :: Ptr CChar -> CSize -> IO UHDErrorEnum

uhdABIString :: IO String
uhdABIString = getUHDString 2048 uhd_get_abi_string

foreign import capi safe "uhd/version.h uhd_get_version_string"
    uhd_get_version_string :: Ptr CChar -> CSize -> IO UHDErrorEnum

uhdVersionString :: IO String
uhdVersionString = getUHDString 2048 uhd_get_version_string

staticStringPtr :: Addr## -> Ptr CChar
staticStringPtr = Ptr

-- | Data format that will be used by UHD's host side sample buffers.
class Storable a => SampleType a where
    cpuString :: Proxy a -> Addr##

instance SampleType (Complex Double) where
    cpuString _ = "fc64"##

instance SampleType (Complex Float) where
    cpuString _ = "fc32"##

instance SampleType (Complex Int16) where
    cpuString _ = "sc16"##

instance SampleType (Complex Int8) where
    cpuString _ = "sc8"##

instance SampleType Float where
    cpuString _ = "f32"##

instance SampleType Int16 where
    cpuString _ = "s16"##

instance SampleType Int8 where
    cpuString _ = "s8"##

data SomeSampleType = forall s. SampleType s => SomeSampleType (Proxy s)

-- | Data format for samples over the wire between host and device.
data WireFormat = -- | 16-bit Signed Complex
                  SC16
                  -- | 8-bit Signed Complex
                | SC8
                  -- | 12-bit Signed Complex
                | SC12
                  -- | 16-bit Signed
                | S16
                  -- | 8-bit Signed
                | S8
                deriving stock ( Eq
                               , Ord
                               , Enum
                               , Read
                               , Show
                               , Generic
                               )

toOTWString :: WireFormat -> Addr##
toOTWString SC16 = "sc16"##
toOTWString SC8  = "sc8"##
toOTWString SC12 = "sc12"##
toOTWString S16  = "s16"##
toOTWString S8   = "s8"##

-- | Parameters used to setup a sample stream.
data {-# CTYPE "uhd/usrp/usrp.h" "uhd_stream_args_t" #-}
    StreamArgs s = StreamArgs {
        -- | The order of channel indices in this list determines their position
        --   in the input\/output vector in send\/recv functions.
        saChanInds :: [ChanIndex]
      , saWireFormat :: WireFormat
        -- | Additional stream argument string, see the UHD manual.
      , saArgs :: String
      } deriving stock ( Eq
                       , Ord
                       , Read
                       , Show
                       , Generic
                       )

withStreamArgs :: forall s s1 a. SampleType s
               => StreamArgs s
               -> (Ptr (StreamArgs s1) -> IO a)
               -> IO a
withStreamArgs StreamArgs{..} f = allocaBytes #{size uhd_stream_args_t} $ \p ->
    withArrayLen saChanInds $ \cn cip ->
    withCString saArgs $ \sap -> do
        let ps :: Proxy s
            ps = Proxy
        #{poke uhd_stream_args_t,cpu_format} p (staticStringPtr (cpuString ps))
        #{poke uhd_stream_args_t,otw_format} p (staticStringPtr (toOTWString saWireFormat))
        #{poke uhd_stream_args_t,args} p sap
        #{poke uhd_stream_args_t,channel_list} p cip
        #{poke uhd_stream_args_t,n_channels} p (fromIntegral cn)
        f p

-- | Stream control command.
data StreamMode = -- | Begin streaming samples indefinitely.
                  StartContinuous
                  -- | Stop indefinite streaming.
                | StopContinuous
                  -- | Stream a certain number of samples, then stop.
                | NumSampsAndDone Word64
                  -- | Stream a certain number of samples, expecting a
                  --   subsequent sample-phase-contiguous chunk.
                | NumSampsAndMore Word64
                deriving stock ( Eq
                               , Ord
                               , Read
                               , Show
                               , Generic
                               )

fromStreamMode :: StreamMode -> CInt
fromStreamMode StartContinuous = #{const UHD_STREAM_MODE_START_CONTINUOUS}
fromStreamMode StopContinuous = #{const UHD_STREAM_MODE_STOP_CONTINUOUS}
fromStreamMode (NumSampsAndDone _) = #{const UHD_STREAM_MODE_NUM_SAMPS_AND_DONE}
fromStreamMode (NumSampsAndMore _) = #{const UHD_STREAM_MODE_NUM_SAMPS_AND_MORE}

toNumSamps :: StreamMode -> CSize
toNumSamps StartContinuous = 0
toNumSamps StopContinuous = 0
toNumSamps (NumSampsAndDone n) = fromIntegral $ n
toNumSamps (NumSampsAndMore n) = fromIntegral $ n

-- | UHD's time type. To what it is relative is arbitrary.
data TimeSpec = TimeSpec {
    fullSecs :: Int64
  , fracSecs :: Double
  } deriving stock ( Eq
                   , Ord
                   , Read
                   , Show
                   , Generic
                   )

toTimeSpec :: NominalDiffTime -> TimeSpec
toTimeSpec t =
    let (fullSecs, f) = divMod' t 1
        fracSecs = fromRational (toRational f)
    in TimeSpec{..}

fromTimeSpec :: TimeSpec -> NominalDiffTime
fromTimeSpec TimeSpec{..} =
    fromIntegral fullSecs + fromRational (toRational fracSecs)

-- | When a stream should start.
data StreamTime = -- | As soon as possible.
                  StreamNow
                  -- | At this time.
                | StreamStartTime TimeSpec
                deriving stock ( Eq
                               , Ord
                               , Read
                               , Show
                               , Generic
                               )

toStreamNow :: StreamTime -> CBool
toStreamNow StreamNow = 1
toStreamNow _         = 0

toStreamTimeSpec :: StreamTime -> TimeSpec
toStreamTimeSpec StreamNow = TimeSpec 0 0
toStreamTimeSpec (StreamStartTime ts) = ts

-- | Streamer control command.
data {-# CTYPE "uhd/usrp/usrp.h" "uhd_stream_cmd_t" #-}
    StreamCmd = StreamCmd {
        scStreamMode :: StreamMode
      , scStreamTime :: StreamTime
      } deriving stock ( Eq
                       , Ord
                       , Read
                       , Show
                       , Generic
                       )

withStreamCmd :: StreamCmd -> (Ptr StreamCmd -> IO a) -> IO a
withStreamCmd StreamCmd{..} f = allocaBytes #{size uhd_stream_cmd_t} $ \p -> do
    let streamMode = fromStreamMode scStreamMode
        numSamps = toNumSamps scStreamMode
        streamNow = toStreamNow scStreamTime
        TimeSpec{..} = toStreamTimeSpec scStreamTime
    #{poke uhd_stream_cmd_t,stream_mode} p streamMode
    #{poke uhd_stream_cmd_t,num_samps} p numSamps
    #{poke uhd_stream_cmd_t,stream_now} p streamNow
    #{poke uhd_stream_cmd_t,time_spec_full_secs} p fullSecs
    #{poke uhd_stream_cmd_t,time_spec_frac_secs} p fracSecs
    f p

data {-# CTYPE "uhd/types/metadata.h" "struct uhd_rx_metadata_t" #-}
    RxMetadata = RxMetadata (ForeignPtr RxMetadata)

-- | An error synchronously reported by receiving from an RX chain.
data RxMetadataError = RxMetadataTimeout
                     | RxMetadataLateCommand
                     | RxMetadataBrokenChain
                     | RxMetadataOverflow
                     | RxMetadataAlignment
                     | RxMetadataBadPacket
                     | RxMetadataUnknownError
                     deriving stock ( Eq
                                    , Ord
                                    , Enum
                                    , Read
                                    , Show
                                    , Generic
                                    )

newtype {-# CTYPE "uhd/types/metadata.h" "uhd_rx_metadata_error_code_t" #-}
    RxMetadataErrorEnum = RxMetadataErrorEnum { fromRxMetadataErrorEnum :: CInt }
    deriving newtype (Storable)

toRxMetadataError :: CInt -> Maybe RxMetadataError
toRxMetadataError #{const UHD_RX_METADATA_ERROR_CODE_NONE} = Nothing
toRxMetadataError #{const UHD_RX_METADATA_ERROR_CODE_TIMEOUT} = Just RxMetadataTimeout
toRxMetadataError #{const UHD_RX_METADATA_ERROR_CODE_LATE_COMMAND} = Just RxMetadataLateCommand
toRxMetadataError #{const UHD_RX_METADATA_ERROR_CODE_BROKEN_CHAIN} = Just RxMetadataBrokenChain
toRxMetadataError #{const UHD_RX_METADATA_ERROR_CODE_OVERFLOW} = Just RxMetadataOverflow
toRxMetadataError #{const UHD_RX_METADATA_ERROR_CODE_ALIGNMENT} = Just RxMetadataAlignment
toRxMetadataError #{const UHD_RX_METADATA_ERROR_CODE_BAD_PACKET} = Just RxMetadataBadPacket
toRxMetadataError _ = Just RxMetadataUnknownError

foreign import capi safe "uhd/types/metadata.h uhd_rx_metadata_make"
    uhd_rx_metadata_make :: Ptr (Ptr RxMetadata) -> IO UHDErrorEnum

foreign import capi safe "uhd/types/metadata.h uhd_rx_metadata_free"
    uhd_rx_metadata_free :: Ptr (Ptr RxMetadata) -> IO UHDErrorEnum

mkFreeByRef :: (Ptr (Ptr a) -> IO UHDErrorEnum) -> Ptr a -> IO ()
mkFreeByRef cf mp = alloca $ \p -> do
    poke p mp
    throwOnUHDError $ cf p

makeRxMetadata :: IO RxMetadata
makeRxMetadata = alloca $ \p -> do
    throwOnUHDError $ uhd_rx_metadata_make p
    mp <- peek p
    RxMetadata <$> newForeignPtr mp (mkFreeByRef uhd_rx_metadata_free mp)

foreign import capi safe "uhd/types/metadata.h uhd_rx_metadata_has_time_spec"
    uhd_rx_metadata_has_time_spec :: Ptr RxMetadata
                                  -> Ptr CBool
                                  -> IO UHDErrorEnum

rxMetadataHasTimeSpec :: RxMetadata -> IO Bool
rxMetadataHasTimeSpec (RxMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \bp -> do
        throwOnUHDError $ uhd_rx_metadata_has_time_spec p bp
        toBool <$> peek bp

foreign import capi safe "uhd/types/metadata.h uhd_rx_metadata_time_spec"
    uhd_rx_metadata_time_spec :: Ptr RxMetadata
                              -> Ptr Int64
                              -> Ptr Double
                              -> IO UHDErrorEnum

rxMetadataTimeSpec :: RxMetadata -> IO TimeSpec
rxMetadataTimeSpec (RxMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \sp ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_rx_metadata_time_spec p sp dp
        fullSecs <- peek sp
        fracSecs <- peek dp
        pure TimeSpec{..}

foreign import capi safe "uhd/types/metadata.h uhd_rx_metadata_more_fragments"
    uhd_rx_metadata_more_fragments :: Ptr a-> Ptr CBool-> IO UHDErrorEnum

rxMetadataMoreFragments :: RxMetadata -> IO Bool
rxMetadataMoreFragments (RxMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \bp -> do
        throwOnUHDError $ uhd_rx_metadata_more_fragments p bp
        toBool <$> peek bp

foreign import capi safe "uhd/types/metadata.h uhd_rx_metadata_fragment_offset"
    uhd_rx_metadata_fragment_offset :: Ptr RxMetadata
                                    -> Ptr CSize
                                    -> IO UHDErrorEnum

rxMetadataFragmentOffset :: RxMetadata -> IO Word64
rxMetadataFragmentOffset (RxMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \wp -> do
        throwOnUHDError $ uhd_rx_metadata_fragment_offset p wp
        fromIntegral <$> peek wp

foreign import capi safe "uhd/types/metadata.h uhd_rx_metadata_start_of_burst"
    uhd_rx_metadata_start_of_burst :: Ptr RxMetadata
                                   -> Ptr CBool
                                   -> IO UHDErrorEnum

rxMetadataBurstStart :: RxMetadata -> IO Bool
rxMetadataBurstStart (RxMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \bp -> do
        throwOnUHDError $ uhd_rx_metadata_start_of_burst p bp
        toBool <$> peek bp

foreign import capi safe "uhd/types/metadata.h uhd_rx_metadata_end_of_burst"
    uhd_rx_metadata_end_of_burst :: Ptr RxMetadata
                                 -> Ptr CBool
                                 -> IO UHDErrorEnum

rxMetadataBurstEnd :: RxMetadata -> IO Bool
rxMetadataBurstEnd (RxMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \bp -> do
        throwOnUHDError $ uhd_rx_metadata_end_of_burst p bp
        toBool <$> peek bp

foreign import capi safe "uhd/types/metadata.h uhd_rx_metadata_out_of_sequence"
    uhd_rx_metadata_out_of_sequence :: Ptr RxMetadata
                                    -> Ptr CBool
                                    -> IO UHDErrorEnum

rxMetadataOutOfSequence :: RxMetadata -> IO Bool
rxMetadataOutOfSequence (RxMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \bp -> do
        throwOnUHDError $ uhd_rx_metadata_out_of_sequence p bp
        toBool <$> peek bp

foreign import capi safe "uhd/types/metadata.h uhd_rx_metadata_to_pp_string"
    uhd_rx_metadata_to_pp_string :: Ptr RxMetadata
                                 -> Ptr CChar
                                 -> CSize
                                 -> IO UHDErrorEnum

rxMetadataPrettyPrint :: RxMetadata -> IO String
rxMetadataPrettyPrint (RxMetadata fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_rx_metadata_to_pp_string p

foreign import capi safe "uhd/types/metadata.h uhd_rx_metadata_error_code"
    uhd_rx_metadata_error_code :: Ptr RxMetadata
                               -> Ptr RxMetadataErrorEnum
                               -> IO UHDErrorEnum

rxMetadataError :: RxMetadata -> IO (Maybe RxMetadataError)
rxMetadataError (RxMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \ip -> do
        throwOnUHDError $ uhd_rx_metadata_error_code p ip
        (toRxMetadataError . fromRxMetadataErrorEnum) <$> peek ip

foreign import capi safe "uhd/types/metadata.h uhd_rx_metadata_strerror"
    uhd_rx_metadata_strerror :: Ptr RxMetadata
                             -> Ptr CChar
                             -> CSize
                             -> IO UHDErrorEnum

rxMetadataErrorString :: RxMetadata -> IO String
rxMetadataErrorString (RxMetadata fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_rx_metadata_strerror p

foreign import capi safe "uhd/types/metadata.h uhd_rx_metadata_last_error"
    uhd_rx_metadata_last_error :: Ptr RxMetadata
                               -> Ptr CChar
                               -> CSize
                               -> IO UHDErrorEnum

rxMetadataLastError :: RxMetadata -> IO String
rxMetadataLastError (RxMetadata fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_rx_metadata_last_error p

data {-# CTYPE "uhd/types/metadata.h" "struct uhd_tx_metadata_t" #-}
    TxMetadata = TxMetadata (ForeignPtr TxMetadata)

foreign import capi safe "uhd/types/metadata.h uhd_tx_metadata_make"
    uhd_tx_metadata_make :: Ptr (Ptr TxMetadata)
                         -> CBool
                         -> Int64
                         -> Double
                         -> CBool
                         -> CBool
                         -> IO UHDErrorEnum

foreign import capi safe "uhd/types/metadata.h uhd_tx_metadata_free"
    uhd_tx_metadata_free :: Ptr (Ptr TxMetadata) -> IO UHDErrorEnum


foreign import capi safe "uhd/types/metadata.h uhd_tx_metadata_has_time_spec"
    uhd_tx_metadata_has_time_spec :: Ptr TxMetadata
                                  -> Ptr CBool
                                  -> IO UHDErrorEnum

txMetadataHasTimeSpec :: TxMetadata -> IO Bool
txMetadataHasTimeSpec (TxMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \bp -> do
        throwOnUHDError $ uhd_tx_metadata_has_time_spec p bp
        toBool <$> peek bp

foreign import capi safe "uhd/types/metadata.h uhd_tx_metadata_time_spec"
    uhd_tx_metadata_time_spec :: Ptr TxMetadata
                              -> Ptr Int64
                              -> Ptr Double
                              -> IO UHDErrorEnum

txMetadataTimeSpec :: TxMetadata -> IO TimeSpec
txMetadataTimeSpec (TxMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \sp ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_tx_metadata_time_spec p sp dp
        fullSecs <- peek sp
        fracSecs <- peek dp
        pure TimeSpec{..}

foreign import capi safe "uhd/types/metadata.h uhd_tx_metadata_start_of_burst"
    uhd_tx_metadata_start_of_burst :: Ptr TxMetadata
                                   -> Ptr CBool
                                   -> IO UHDErrorEnum

txMetadataBurstStart :: TxMetadata -> IO Bool
txMetadataBurstStart (TxMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \bp -> do
        throwOnUHDError $ uhd_tx_metadata_start_of_burst p bp
        toBool <$> peek bp

foreign import capi safe "uhd/types/metadata.h uhd_tx_metadata_end_of_burst"
    uhd_tx_metadata_end_of_burst :: Ptr TxMetadata
                                 -> Ptr CBool
                                 -> IO UHDErrorEnum

txMetadataBurstEnd :: TxMetadata -> IO Bool
txMetadataBurstEnd (TxMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \bp -> do
        throwOnUHDError $ uhd_tx_metadata_end_of_burst p bp
        toBool <$> peek bp

foreign import capi safe "uhd/types/metadata.h uhd_tx_metadata_last_error"
    uhd_tx_metadata_last_error :: Ptr TxMetadata
                               -> Ptr CChar
                               -> CSize
                               -> IO UHDErrorEnum

txMetadataLastError :: TxMetadata -> IO String
txMetadataLastError (TxMetadata fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_tx_metadata_last_error p

data {-# CTYPE "uhd/types/metadata.h" "struct uhd_async_metadata_t" #-}
    AsyncMetadata = AsyncMetadata (ForeignPtr AsyncMetadata)

-- | An event reported asynchronously by an active TX chain.
data AsyncMetadataEvent = AsyncMetadataBurstAck
                        | AsyncMetadataUnderflow
                        | AsyncMetadataSeqError
                        | AsyncMetadataTimeError
                        | AsyncMetadataUnderflowInPacket
                        | AsyncMetadataSeqErrorInBurst
                        | AsyncMetadataUserPayload
                        | AsyncMetadataUnknownEvent
                        deriving stock ( Eq
                                       , Ord
                                       , Enum
                                       , Read
                                       , Show
                                       , Generic
                                       )

newtype {-# CTYPE "uhd/types/metadata.h" "uhd_async_metadata_event_code_t" #-}
    AsyncMetadataEventEnum =
        AsyncMetadataEventEnum { fromAsyncMetadataEventEnum :: CInt }
            deriving newtype (Storable)
                                
toAsyncMetadataEvent :: CInt -> AsyncMetadataEvent
toAsyncMetadataEvent #{const UHD_ASYNC_METADATA_EVENT_CODE_BURST_ACK} = AsyncMetadataBurstAck
toAsyncMetadataEvent #{const UHD_ASYNC_METADATA_EVENT_CODE_UNDERFLOW} = AsyncMetadataUnderflow
toAsyncMetadataEvent #{const UHD_ASYNC_METADATA_EVENT_CODE_SEQ_ERROR} = AsyncMetadataSeqError
toAsyncMetadataEvent #{const UHD_ASYNC_METADATA_EVENT_CODE_TIME_ERROR} = AsyncMetadataTimeError
toAsyncMetadataEvent #{const UHD_ASYNC_METADATA_EVENT_CODE_UNDERFLOW_IN_PACKET} = AsyncMetadataUnderflowInPacket
toAsyncMetadataEvent #{const UHD_ASYNC_METADATA_EVENT_CODE_SEQ_ERROR_IN_BURST} = AsyncMetadataSeqErrorInBurst
toAsyncMetadataEvent #{const UHD_ASYNC_METADATA_EVENT_CODE_USER_PAYLOAD} = AsyncMetadataUserPayload
toAsyncMetadataEvent _ = AsyncMetadataUnknownEvent

foreign import capi safe "uhd/types/metadata.h uhd_async_metadata_make"
    uhd_async_metadata_make :: Ptr (Ptr AsyncMetadata) -> IO UHDErrorEnum

foreign import capi safe "uhd/types/metadata.h uhd_async_metadata_free"
    uhd_async_metadata_free :: Ptr (Ptr AsyncMetadata) -> IO UHDErrorEnum

makeAsyncMetadata :: IO AsyncMetadata
makeAsyncMetadata = alloca $ \p -> do
    throwOnUHDError $ uhd_async_metadata_make p
    mp <- peek p
    AsyncMetadata <$> newForeignPtr mp (mkFreeByRef uhd_async_metadata_free mp)

foreign import capi safe "uhd/types/metadata.h uhd_async_metadata_channel"
    uhd_async_metadata_channel :: Ptr AsyncMetadata
                               -> Ptr CSize
                               -> IO UHDErrorEnum

asyncMetadataChanIndex :: AsyncMetadata -> IO ChanIndex
asyncMetadataChanIndex (AsyncMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \sp -> do
        throwOnUHDError $ uhd_async_metadata_channel p sp
        fromIntegral <$> peek sp

foreign import capi safe "uhd/types/metadata.h uhd_async_metadata_has_time_spec"
    uhd_async_metadata_has_time_spec :: Ptr AsyncMetadata
                                     -> Ptr CBool
                                     -> IO UHDErrorEnum

asyncMetadataHasTimeSpec :: AsyncMetadata -> IO Bool
asyncMetadataHasTimeSpec (AsyncMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \bp -> do
        throwOnUHDError $ uhd_async_metadata_has_time_spec p bp
        toBool <$> peek bp

foreign import capi safe "uhd/types/metadata.h uhd_async_metadata_time_spec"
    uhd_async_metadata_time_spec :: Ptr AsyncMetadata
                                 -> Ptr Int64
                                 -> Ptr Double
                                 -> IO UHDErrorEnum

asyncMetadataTimeSpec :: AsyncMetadata -> IO TimeSpec
asyncMetadataTimeSpec (AsyncMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \sp ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_async_metadata_time_spec p sp dp
        fullSecs <- peek sp
        fracSecs <- peek dp
        pure TimeSpec{..}

foreign import capi safe "uhd/types/metadata.h uhd_async_metadata_event_code"
    uhd_async_metadata_event_code :: Ptr AsyncMetadata
                                  -> Ptr AsyncMetadataEventEnum
                                  -> IO UHDErrorEnum

asyncMetadataEvent :: AsyncMetadata -> IO AsyncMetadataEvent
asyncMetadataEvent (AsyncMetadata fp) =
    withForeignPtr fp $ \p ->
    alloca $ \ip -> do
        throwOnUHDError $ uhd_async_metadata_event_code p ip
        (toAsyncMetadataEvent . fromAsyncMetadataEventEnum) <$> peek ip

foreign import capi safe "uhd/types/metadata.h uhd_async_metadata_user_payload"
    uhd_async_metadata_user_payload :: Ptr AsyncMetadata
                                    -> Ptr Word32
                                    -> IO UHDErrorEnum

-- TODO: use V4
asyncMetadataUserPayload :: AsyncMetadata -> IO (VS.Vector Word32)
asyncMetadataUserPayload (AsyncMetadata fp) =
    withForeignPtr fp $ \p -> do
        afp <- mallocForeignPtrArray 4
        withForeignPtr afp $ \ap -> do
            throwOnUHDError $ uhd_async_metadata_user_payload p ap
            pure $ VS.unsafeFromForeignPtr0 afp 4

foreign import capi safe "uhd/types/metadata.h uhd_async_metadata_last_error"
    uhd_async_metadata_last_error :: Ptr AsyncMetadata
                                  -> Ptr CChar
                                  -> CSize
                                  -> IO UHDErrorEnum

asyncMetadataLastError :: AsyncMetadata -> IO String
asyncMetadataLastError (AsyncMetadata fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_async_metadata_last_error p

data {-# CTYPE "uhd/types/ranges.h" "uhd_range_t" #-}
    Range = Range {
        rStart :: Double
      , rStop :: Double
      , rStep :: Double
      } deriving stock ( Eq
                       , Ord
                       , Read
                       , Show
                       , Generic
                       )

instance Storable Range where
    sizeOf _ = #{size uhd_range_t}
    alignment _ = #{alignment uhd_range_t}
    peek p = do
        rStart <- #{peek uhd_range_t, start} p
        rStop <- #{peek uhd_range_t, stop} p
        rStep <- #{peek uhd_range_t, step} p
        pure Range{..}
    poke p Range{..} = do
        #{poke uhd_range_t, start} p rStart
        #{poke uhd_range_t, stop} p rStop
        #{poke uhd_range_t, step} p rStep

data {-# CTYPE "uhd/types/ranges.h" "struct uhd_meta_range_t" #-}
    MetaRange = MetaRange (ForeignPtr MetaRange)

foreign import capi safe "uhd/types/ranges.h uhd_meta_range_make"
    uhd_meta_range_make :: Ptr (Ptr MetaRange) -> IO UHDErrorEnum

foreign import capi safe "uhd/types/ranges.h uhd_meta_range_free"
    uhd_meta_range_free :: Ptr (Ptr MetaRange) -> IO UHDErrorEnum

makeMetaRange :: IO MetaRange
makeMetaRange = alloca $ \p -> do
    throwOnUHDError $ uhd_meta_range_make p
    mp <- peek p
    MetaRange <$> newForeignPtr mp (mkFreeByRef uhd_meta_range_free mp)

foreign import capi safe "uhd/types/ranges.h uhd_meta_range_start"
    uhd_meta_range_start :: Ptr MetaRange -> Ptr Double -> IO UHDErrorEnum

metaRangeStart :: MetaRange -> IO Double
metaRangeStart (MetaRange fp) =
    withForeignPtr fp $ \p ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_meta_range_start p dp
        peek dp
   
foreign import capi safe "uhd/types/ranges.h uhd_meta_range_stop"
    uhd_meta_range_stop :: Ptr MetaRange -> Ptr Double -> IO UHDErrorEnum

metaRangeStop :: MetaRange -> IO Double
metaRangeStop (MetaRange fp) =
    withForeignPtr fp $ \p ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_meta_range_stop p dp
        peek dp

foreign import capi safe "uhd/types/ranges.h uhd_meta_range_step"
    uhd_meta_range_step :: Ptr MetaRange -> Ptr Double -> IO UHDErrorEnum

metaRangeStep :: MetaRange -> IO Double
metaRangeStep (MetaRange fp) =
    withForeignPtr fp $ \p ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_meta_range_step p dp
        peek dp

foreign import capi safe "uhd/types/ranges.h uhd_meta_range_clip"
    uhd_meta_range_clip :: Ptr MetaRange
                        -> Double
                        -> CBool
                        -> Ptr Double
                        -> IO UHDErrorEnum

metaRangeClip :: MetaRange -> Double -> Bool -> IO Double
metaRangeClip (MetaRange fp) x s =
    withForeignPtr fp $ \p ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_meta_range_clip p x (fromBool s) dp
        peek dp

foreign import capi safe "uhd/types/ranges.h uhd_meta_range_size"
    uhd_meta_range_size :: Ptr MetaRange -> Ptr CSize -> IO UHDErrorEnum

metaRangeSize :: MetaRange -> IO Int
metaRangeSize (MetaRange fp) =
    withForeignPtr fp $ \p ->
    alloca $ \wp -> do
        throwOnUHDError $ uhd_meta_range_size p wp
        fromIntegral <$> peek wp

foreign import capi safe "uhd/types/ranges.h uhd_meta_range_push_back"
    uhd_meta_range_push_back :: Ptr MetaRange -> Ptr Range -> IO UHDErrorEnum

metaRangePushBack :: MetaRange -> Range -> IO ()
metaRangePushBack (MetaRange fp) r =
    withForeignPtr fp $ \p ->
    alloca $ \rp -> do
        poke rp r
        throwOnUHDError $ uhd_meta_range_push_back p rp

foreign import capi safe "uhd/types/ranges.h uhd_meta_range_at"
    uhd_meta_range_at :: Ptr MetaRange -> CSize -> Ptr Range -> IO UHDErrorEnum

metaRangeAt :: MetaRange -> Int -> IO Range
metaRangeAt (MetaRange fp) i =
    withForeignPtr fp $ \p ->
    alloca $ \rp -> do
        throwOnUHDError $ uhd_meta_range_at p (fromIntegral i) rp
        peek rp

foreign import capi safe "uhd/types/ranges.h uhd_meta_range_to_pp_string"
    uhd_meta_range_to_pp_string :: Ptr MetaRange
                                -> Ptr CChar
                                -> CSize
                                -> IO UHDErrorEnum

metaRangePrettyPrint :: MetaRange -> IO String
metaRangePrettyPrint (MetaRange fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_meta_range_to_pp_string p

foreign import capi safe "uhd/types/ranges.h uhd_meta_range_last_error"
    uhd_meta_range_last_error :: Ptr MetaRange
                              -> Ptr CChar
                              -> CSize
                              -> IO UHDErrorEnum

metaRangeLastError :: MetaRange -> IO String
metaRangeLastError (MetaRange fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_meta_range_last_error p

fromMetaRange :: MetaRange -> IO [Range]
fromMetaRange mr = do
    s <- metaRangeSize mr
    traverse (metaRangeAt mr) [0..(s-1)]

fromMetaRangeF :: (MetaRange -> IO ()) -> IO [Range]
fromMetaRangeF f = do
    mr <- makeMetaRange
    f mr
    fromMetaRange mr

data {-# CTYPE "uhd/types/string_vector.h" "struct uhd_string_vector_t" #-}
    StringVector = StringVector (ForeignPtr StringVector)

foreign import capi safe "uhd/types/string_vector.h uhd_string_vector_make"
    uhd_string_vector_make :: Ptr (Ptr StringVector) -> IO UHDErrorEnum

foreign import capi safe "uhd/types/string_vector.h uhd_string_vector_free"
    uhd_string_vector_free :: Ptr (Ptr StringVector) -> IO UHDErrorEnum

makeStringVector :: IO StringVector
makeStringVector = alloca $ \p -> do
    throwOnUHDError $ uhd_string_vector_make p
    mp <- peek p
    print mp
    StringVector <$> newForeignPtr mp (mkFreeByRef uhd_string_vector_free mp)

foreign import capi safe "uhd/types/string_vector.h uhd_string_vector_push_back"
    uhd_string_vector_push_back :: Ptr (Ptr StringVector)
                                -> Ptr CChar
                                -> IO UHDErrorEnum

stringVectorPushBack :: StringVector -> String -> IO ()
stringVectorPushBack (StringVector fp) s =
    withForeignPtr fp $ \p ->
    withCString s $ \sp ->
    alloca $ \pp -> do
        poke pp p
        throwOnUHDError $ uhd_string_vector_push_back pp sp

foreign import capi safe "uhd/types/string_vector.h uhd_string_vector_at"
    uhd_string_vector_at :: Ptr StringVector
                         -> CSize
                         -> Ptr CChar
                         -> CSize
                         -> IO UHDErrorEnum

stringVectorAt :: StringVector -> Int -> IO String
stringVectorAt (StringVector fp) i =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_string_vector_at p (fromIntegral i)

foreign import capi safe "uhd/types/string_vector.h uhd_string_vector_size"
    uhd_string_vector_size :: Ptr StringVector -> Ptr CSize -> IO UHDErrorEnum

stringVectorSize :: StringVector -> IO Int
stringVectorSize (StringVector fp) =
    withForeignPtr fp $ \p ->
    alloca $ \sp -> do
        throwOnUHDError $ uhd_string_vector_size p sp
        fromIntegral <$> peek sp


foreign import capi safe "uhd/types/string_vector.h uhd_string_vector_last_error"
    uhd_string_vector_last_error :: Ptr StringVector
                                 -> Ptr CChar
                                 -> CSize
                                 -> IO UHDErrorEnum

stringVectorLastError :: StringVector -> IO String
stringVectorLastError (StringVector fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_string_vector_last_error p

fromStringVector :: StringVector -> IO [String]
fromStringVector sv = do
    s <- stringVectorSize sv
    traverse (stringVectorAt sv) [0..(s-1)]

fromStringVectorF :: (StringVector -> IO ()) -> IO [String]
fromStringVectorF f = do
    sv <- makeStringVector
    f sv
    fromStringVector sv

-- | The type of data reported by a sensor.
data SensorValueDataType = SensorValueBool
                         | SensorValueInt
                         | SensorValueReal
                         | SensorValueString
                         deriving stock ( Eq
                                        , Ord
                                        , Enum
                                        , Read
                                        , Show
                                        , Generic
                                        )

newtype {-# CTYPE "uhd/types/sensors.h" "uhd_sensor_value_data_type_t" #-}
    SensorValueDataTypeEnum = SensorValueDataTypeEnum { fromSensorValueDataTypeEnum :: CInt }
        deriving newtype (Storable)

toSensorValueDataType :: CInt -> Maybe SensorValueDataType
toSensorValueDataType #{const UHD_SENSOR_VALUE_BOOLEAN} = Just SensorValueBool 
toSensorValueDataType #{const UHD_SENSOR_VALUE_INTEGER} = Just SensorValueInt 
toSensorValueDataType #{const UHD_SENSOR_VALUE_REALNUM} = Just SensorValueReal 
toSensorValueDataType #{const UHD_SENSOR_VALUE_STRING} = Just SensorValueString 
toSensorValueDataType _ = Nothing

fromSensorValueDataType :: SensorValueDataType -> CInt
fromSensorValueDataType SensorValueBool = #{const UHD_SENSOR_VALUE_BOOLEAN}
fromSensorValueDataType SensorValueInt = #{const UHD_SENSOR_VALUE_INTEGER}
fromSensorValueDataType SensorValueReal = #{const UHD_SENSOR_VALUE_REALNUM}
fromSensorValueDataType SensorValueString = #{const UHD_SENSOR_VALUE_STRING}
                         
data {-# CTYPE "uhd/types/sensors.h" "struct uhd_sensor_value_t" #-}
    SensorValue = SensorValue (ForeignPtr SensorValue)

foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_make"
    uhd_sensor_value_make :: Ptr (Ptr SensorValue) -> IO UHDErrorEnum

foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_free"
    uhd_sensor_value_free :: Ptr (Ptr SensorValue) -> IO UHDErrorEnum

makeSensorValue :: IO SensorValue
makeSensorValue = alloca $ \p -> do
    throwOnUHDError $ uhd_sensor_value_make p
    mp <- peek p
    SensorValue <$> newForeignPtr mp (mkFreeByRef uhd_sensor_value_free mp)

foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_make_from_bool"
    uhd_sensor_value_make_from_bool :: Ptr (Ptr SensorValue)
                                    -> Ptr CChar
                                    -> CBool
                                    -> Ptr CChar
                                    -> Ptr CChar
                                    -> IO UHDErrorEnum

makeSensorValueFromBool :: String -> Bool -> String -> String -> IO SensorValue
makeSensorValueFromBool name b t f = alloca $ \p ->
    withCString name $ \np ->
    withCString t $ \tp ->
    withCString f $ \fp -> do
        throwOnUHDError $
            uhd_sensor_value_make_from_bool p np (fromBool b) tp fp
        mp <- peek p
        SensorValue <$> newForeignPtr mp (mkFreeByRef uhd_sensor_value_free mp)

foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_make_from_int"
    uhd_sensor_value_make_from_int :: Ptr (Ptr SensorValue)
                                   -> Ptr CChar
                                   -> CInt
                                   -> Ptr CChar
                                   -> Ptr CChar
                                   -> IO UHDErrorEnum

makeSensorValueFromInt :: Integral a
                       => String
                       -> a
                       -> String
                       -> String
                       -> IO SensorValue
makeSensorValueFromInt name i unit format = alloca $ \p ->
    withCString name $ \np ->
    withCString unit $ \up ->
    withCString format $ \fp -> do
        throwOnUHDError $
            uhd_sensor_value_make_from_int p np (fromIntegral i) up fp
        mp <- peek p
        SensorValue <$> newForeignPtr mp (mkFreeByRef uhd_sensor_value_free mp)
    
foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_make_from_realnum"
    uhd_sensor_value_make_from_realnum :: Ptr (Ptr SensorValue)
                                       -> Ptr CChar
                                       -> Double
                                       -> Ptr CChar
                                       -> Ptr CChar
                                       -> IO UHDErrorEnum

makeSensorValueFromDouble :: String
                          -> Double
                          -> String
                          -> String
                          -> IO SensorValue
makeSensorValueFromDouble name x unit format = alloca $ \p ->
    withCString name $ \np ->
    withCString unit $ \up ->
    withCString format $ \fp -> do
        throwOnUHDError $
            uhd_sensor_value_make_from_realnum p np x up fp
        mp <- peek p
        SensorValue <$> newForeignPtr mp (mkFreeByRef uhd_sensor_value_free mp)

foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_make_from_string"
    uhd_sensor_value_make_from_string :: Ptr (Ptr SensorValue)
                                      -> Ptr CChar
                                      -> Ptr CChar
                                      -> Ptr CChar
                                      -> IO UHDErrorEnum

makeSensorValueFromString :: String
                          -> String
                          -> String
                          -> IO SensorValue
makeSensorValueFromString name s unit = alloca $ \p ->
    withCString name $ \np ->
    withCString s $ \sp ->
    withCString unit $ \up -> do
        throwOnUHDError $
            uhd_sensor_value_make_from_string p np sp up
        mp <- peek p
        SensorValue <$> newForeignPtr mp (mkFreeByRef uhd_sensor_value_free mp)

foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_to_bool"
    uhd_sensor_value_to_bool :: Ptr SensorValue -> Ptr CBool -> IO UHDErrorEnum

sensorValueBool :: SensorValue -> IO Bool
sensorValueBool (SensorValue fp) =
    withForeignPtr fp $ \p ->
    alloca $ \bp -> do
        throwOnUHDError $ uhd_sensor_value_to_bool p bp
        toBool <$> peek bp

foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_to_int"
    uhd_sensor_value_to_int :: Ptr SensorValue -> Ptr CInt -> IO UHDErrorEnum

sensorValueInt :: Integral a => SensorValue -> IO a
sensorValueInt (SensorValue fp) =
    withForeignPtr fp $ \p ->
    alloca $ \ip -> do
        throwOnUHDError $ uhd_sensor_value_to_int p ip
        fromIntegral <$> peek ip

foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_to_realnum"
    uhd_sensor_value_to_realnum :: Ptr SensorValue
                                -> Ptr Double
                                -> IO UHDErrorEnum

sensorValueDouble :: SensorValue -> IO Double
sensorValueDouble (SensorValue fp) =
    withForeignPtr fp $ \p ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_sensor_value_to_realnum p dp
        peek dp

foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_name"
    uhd_sensor_value_name :: Ptr SensorValue
                          -> Ptr CChar
                          -> CSize
                          -> IO UHDErrorEnum

sensorValueName :: SensorValue -> IO String
sensorValueName (SensorValue fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_sensor_value_name p

foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_value"
    uhd_sensor_value_value :: Ptr SensorValue
                           -> Ptr CChar
                           -> CSize
                           -> IO UHDErrorEnum

sensorValueValue :: SensorValue -> IO String
sensorValueValue (SensorValue fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_sensor_value_value p

foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_unit"
    uhd_sensor_value_unit :: Ptr SensorValue
                          -> Ptr CChar
                          -> CSize
                          -> IO UHDErrorEnum

sensorValueUnit :: SensorValue -> IO String
sensorValueUnit (SensorValue fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_sensor_value_unit p

foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_data_type"
    uhd_sensor_value_data_type :: Ptr SensorValue
                               -> Ptr SensorValueDataTypeEnum
                               -> IO UHDErrorEnum

sensorValueDataType :: SensorValue -> IO (Maybe SensorValueDataType)
sensorValueDataType (SensorValue fp) =
    withForeignPtr fp $ \p ->
    alloca $ \tp -> do
        throwOnUHDError $ uhd_sensor_value_data_type p tp
        (toSensorValueDataType . fromSensorValueDataTypeEnum) <$> peek tp

foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_to_pp_string"
    uhd_sensor_value_to_pp_string :: Ptr SensorValue
                                  -> Ptr CChar
                                  -> CSize
                                  -> IO UHDErrorEnum

sensorValuePrettyPrint :: SensorValue -> IO String
sensorValuePrettyPrint (SensorValue fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_sensor_value_to_pp_string p

foreign import capi safe "uhd/types/sensors.h uhd_sensor_value_last_error"
    uhd_sensor_value_last_error :: Ptr SensorValue
                                -> Ptr CChar
                                -> CSize
                                -> IO UHDErrorEnum

sensorValueLastError :: SensorValue -> IO String
sensorValueLastError (SensorValue fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_sensor_value_last_error p

-- | The actual value reported by a USRP's sensor.
data SensorValueData = SensorValueDataBool Bool
                     | SensorValueDataInt Int
                     | SensorValueDataReal Double
                     | SensorValueDataString String
                     deriving stock ( Eq
                                    , Ord
                                    , Read
                                    , Show
                                    , Generic
                                    )

-- | Determines how tuning requests will deal with various frequency values in
--   the RF chain.
data TuneReqPolicy a = -- | Don't mutate this frequency setting, leaving it at the
                       --   current value.
                       TuneReqPolicyNone
                     | -- | Automatically choose this frequency setting.
                       TuneReqPolicyAuto
                     | -- | Manually specify this frequency setting.
                       TuneReqPolicyManual a
                     deriving stock ( Eq
                                    , Ord
                                    , Read
                                    , Show
                                    , Generic
                                    )

fromTuneReqPolicy :: TuneReqPolicy a -> CInt
fromTuneReqPolicy TuneReqPolicyNone = #{const UHD_TUNE_REQUEST_POLICY_NONE}
fromTuneReqPolicy TuneReqPolicyAuto = #{const UHD_TUNE_REQUEST_POLICY_AUTO}
fromTuneReqPolicy (TuneReqPolicyManual _) = #{const UHD_TUNE_REQUEST_POLICY_MANUAL}

tuneReqPolicyVal :: Num a => TuneReqPolicy a -> a
tuneReqPolicyVal (TuneReqPolicyManual x) = x
tuneReqPolicyVal _                       = 0

newtype {-# CTYPE "uhd/types/tune_request.h" "uhd_tune_request_policy_t" #-}
    TuneReqPolicyEnum = TuneReqPolicyEnum { fromTuneReqPolicyEnum :: CInt }
        deriving newtype (Storable)

-- | Request to set the tune frequency of the RF chain.
data {-# CTYPE "uhd/types/tune_request.h" "uhd_tune_request_t" #-}
    TuneReq = TuneReq {
        -- | Target frequency for the overall RF chain.
        tReqTargetFreq :: Double
        -- | How to set the RF frequency.
      , tReqRFPolicy :: TuneReqPolicy Double
        -- | How to set the DSP frequency.
      , tReqDSPPolicy :: TuneReqPolicy Double
        -- | Other arguments, see the UHD manual.
      , tReqArgs :: String
      } deriving stock ( Eq
                       , Ord
                       , Read
                       , Show
                       , Generic
                       )

withTuneReq :: TuneReq -> (Ptr TuneReq -> IO a) -> IO a
withTuneReq TuneReq{..} f =
    allocaBytes #{size uhd_tune_request_t} $ \p ->
    withCString tReqArgs $ \ap -> do
        #{poke uhd_tune_request_t,target_freq} p tReqTargetFreq
        #{poke uhd_tune_request_t,rf_freq_policy} p (fromTuneReqPolicy tReqRFPolicy)
        #{poke uhd_tune_request_t,rf_freq} p (tuneReqPolicyVal tReqRFPolicy)
        #{poke uhd_tune_request_t,dsp_freq_policy} p (fromTuneReqPolicy tReqDSPPolicy)
        #{poke uhd_tune_request_t,dsp_freq} p (tuneReqPolicyVal tReqDSPPolicy)
        #{poke uhd_tune_request_t,args} p ap
        f p

-- | The result of tuning the RF chain.
data {-# CTYPE "uhd/types/tune_result.h" "uhd_tune_result_t" #-}
    TuneRes = TuneRes {
        tResClippedRFFreq :: Double
      , tResTargetRFFreq :: Double
      , tResActualRFFreq :: Double
      , tResTargetDSPFreq :: Double
      , tResActualDSPFreq :: Double
      } deriving stock ( Eq
                       , Ord
                       , Read
                       , Show
                       , Generic
                       )

instance Storable TuneRes where
    sizeOf _ = #{size uhd_tune_result_t}
    alignment _ = #{alignment uhd_tune_result_t}
    peek p = do
        tResClippedRFFreq <- #{peek uhd_tune_result_t,clipped_rf_freq} p
        tResTargetRFFreq <- #{peek uhd_tune_result_t,target_rf_freq} p
        tResActualRFFreq <- #{peek uhd_tune_result_t,actual_rf_freq} p
        tResTargetDSPFreq <- #{peek uhd_tune_result_t,target_dsp_freq} p
        tResActualDSPFreq <- #{peek uhd_tune_result_t,actual_dsp_freq} p
        pure TuneRes{..}
    poke p TuneRes{..} = do
        #{poke uhd_tune_result_t,clipped_rf_freq} p tResClippedRFFreq 
        #{poke uhd_tune_result_t,target_rf_freq} p tResTargetRFFreq 
        #{poke uhd_tune_result_t,actual_rf_freq} p tResActualRFFreq 
        #{poke uhd_tune_result_t,target_dsp_freq} p tResTargetDSPFreq 
        #{poke uhd_tune_result_t,actual_dsp_freq} p tResActualDSPFreq 

foreign import capi safe "uhd/types/tune_result.h uhd_tune_result_to_pp_string"
    uhd_tune_result_to_pp_string :: Ptr TuneRes
                                 -> Ptr CChar
                                 -> CSize
                                 -> IO ()

tuneResPrettyPrint :: TuneRes -> IO String
tuneResPrettyPrint tr = with tr $ \p ->
    getUHDString_ 2048 $ uhd_tune_result_to_pp_string p

-- | Metadata related to an RX chain.
data {-# CTYPE "uhd/types/usrp_info.h" "uhd_usrp_rx_info_t" #-}
    RxInfo = RxInfo {
        riMotherboardId :: String
      , riMotherboardName :: String
      , riMotherboardSerial :: String
      , riRxId :: String
      , riRxSubDevName :: String
      , riRxSubDebSpec :: String
      , riRxSerial :: String
      , riRxAntenna :: String
      } deriving stock ( Eq
                       , Ord
                       , Read
                       , Show
                       , Generic
                       )

foreign import capi safe "uhd/types/usrp_info.h uhd_usrp_rx_info_free"
    uhd_usrp_rx_info_free :: Ptr RxInfo -> IO UHDErrorEnum

peekRxInfo :: Ptr RxInfo -> IO RxInfo
peekRxInfo p = do
    riMotherboardId <-
        (#{peek uhd_usrp_rx_info_t,mboard_id} p) >>= peekCString
    riMotherboardName <-
        (#{peek uhd_usrp_rx_info_t,mboard_name} p) >>= peekCString
    riMotherboardSerial <-
        (#{peek uhd_usrp_rx_info_t,mboard_serial} p) >>= peekCString
    riRxId <-
        (#{peek uhd_usrp_rx_info_t,rx_id} p) >>= peekCString
    riRxSubDevName <-
        (#{peek uhd_usrp_rx_info_t,rx_subdev_name} p) >>= peekCString
    riRxSubDebSpec <-
        (#{peek uhd_usrp_rx_info_t,rx_subdev_spec} p) >>= peekCString
    riRxSerial <-
        (#{peek uhd_usrp_rx_info_t,rx_serial} p) >>= peekCString
    riRxAntenna <-
        (#{peek uhd_usrp_rx_info_t,rx_antenna} p) >>= peekCString
    pure RxInfo{..}

-- | Metadata related to a TX chain.
data {-# CTYPE "uhd/types/usrp_info.h" "uhd_usrp_tx_info_t" #-}
    TxInfo = TxInfo {
        tiMotherboardId :: String
      , tiMotherboardName :: String
      , tiMotherboardSerial :: String
      , tiTxId :: String
      , tiTxSubDevName :: String
      , tiTxSubDebSpec :: String
      , tiTxSerial :: String
      , tiTxAntenna :: String
      } deriving stock ( Eq
                       , Ord
                       , Read
                       , Show
                       , Generic
                       )

foreign import capi safe "uhd/types/usrp_info.h uhd_usrp_tx_info_free"
    uhd_usrp_tx_info_free :: Ptr TxInfo -> IO UHDErrorEnum

peekTxInfo :: Ptr TxInfo -> IO TxInfo
peekTxInfo p = do
    tiMotherboardId <-
        (#{peek uhd_usrp_tx_info_t,mboard_id} p) >>= peekCString
    tiMotherboardName <-
        (#{peek uhd_usrp_tx_info_t,mboard_name} p) >>= peekCString
    tiMotherboardSerial <-
        (#{peek uhd_usrp_tx_info_t,mboard_serial} p) >>= peekCString
    tiTxId <-
        (#{peek uhd_usrp_tx_info_t,tx_id} p) >>= peekCString
    tiTxSubDevName <-
        (#{peek uhd_usrp_tx_info_t,tx_subdev_name} p) >>= peekCString
    tiTxSubDebSpec <-
        (#{peek uhd_usrp_tx_info_t,tx_subdev_spec} p) >>= peekCString
    tiTxSerial <-
        (#{peek uhd_usrp_tx_info_t,tx_serial} p) >>= peekCString
    tiTxAntenna <-
        (#{peek uhd_usrp_tx_info_t,tx_antenna} p) >>= peekCString
    pure TxInfo{..}

-- | A handle for UHD state related to streaming samples from an RX chain,
--   parameterized by host side sample type.
data {-# CTYPE "uhd/usrp/usrp.h" "struct uhd_rx_streamer" #-}
    RxStreamer s = RxStreamer (ForeignPtr (RxStreamer s))

foreign import capi safe "uhd/usrp/usrp.h uhd_rx_streamer_make"
    uhd_rx_streamer_make :: Ptr (Ptr (RxStreamer s)) -> IO UHDErrorEnum

foreign import capi safe "uhd/usrp/usrp.h uhd_rx_streamer_free"
    uhd_rx_streamer_free :: Ptr (Ptr (RxStreamer s)) -> IO UHDErrorEnum

foreign import capi safe "uhd/usrp/usrp.h uhd_rx_streamer_num_channels"
    uhd_rx_streamer_num_channels :: Ptr (RxStreamer s)
                                 -> Ptr CSize
                                 -> IO UHDErrorEnum

-- | The number of channels setup for this streamer.
rxStreamerNumChannels :: RxStreamer s -> IO Word64
rxStreamerNumChannels (RxStreamer fp) =
    withForeignPtr fp $ \p ->
    alloca $ \np -> do
        throwOnUHDError $ uhd_rx_streamer_num_channels p np
        fromIntegral <$> peek np

foreign import capi safe "uhd/usrp/usrp.h uhd_rx_streamer_max_num_samps"
    uhd_rx_streamer_max_num_samps :: Ptr (RxStreamer s) -> Ptr CSize -> IO UHDErrorEnum

-- | The maximum samples per buffer per channel for this streamer.
rxStreamerMaxNumSamps :: (RxStreamer s) -> IO Word64
rxStreamerMaxNumSamps (RxStreamer fp) =
    withForeignPtr fp $ \p ->
    alloca $ \np -> do
        throwOnUHDError $ uhd_rx_streamer_max_num_samps p np
        fromIntegral <$> peek np

foreign import capi safe "uhd/usrp/usrp.h uhd_rx_streamer_recv"
    uhd_rx_streamer_recv :: Ptr (RxStreamer s)
                         -> Ptr (Ptr a)
                         -> CSize
                         -> Ptr (Ptr RxMetadata)
                         -> Double
                         -> CBool
                         -> Ptr CSize
                         -> IO UHDErrorEnum

rxStreamerRecv :: forall s. SampleType s
               => RxStreamer s
               -> RxMetadata
               -> Word64
               -> Double
               -> Bool
               -> IO (V.Vector (VS.Vector s))
rxStreamerRecv rs@(RxStreamer rsfp) (RxMetadata rmfp) maxN tos oneP =
    withForeignPtr rsfp $ \rsp ->
    withForeignPtr rmfp $ \rmp ->
    alloca $ \rmpp -> do
        cn <- fromIntegral <$> rxStreamerNumChannels rs
        allocaArray cn $ \bsp ->
            allocaArray cn $ \osp -> do
                let imaxN = fromIntegral maxN
                topBuffFP <- mallocForeignPtrBytes (sizeOf (undefined :: s) * cn * imaxN)
                let buffFPs = V.generate cn (plusForeignPtr topBuffFP . (* imaxN))
                V.imapM (\ci p -> pokeElemOff bsp ci (unsafeForeignPtrToPtr p))
                        buffFPs
                poke rmpp rmp
                throwOnUHDError $
                    uhd_rx_streamer_recv rsp
                                         bsp
                                         (fromIntegral imaxN)
                                         rmpp
                                         tos
                                         (fromBool oneP)
                                         osp
                ls <- V.generateM cn (fmap fromIntegral . peekElemOff osp)
                -- use used unsafeForeignPtrToPtr. Not sure it's necessary though.
                touchForeignPtr topBuffFP
                pure $ V.zipWith VS.unsafeFromForeignPtr0 buffFPs ls

foreign import capi safe "uhd/usrp/usrp.h uhd_rx_streamer_issue_stream_cmd"
    uhd_rx_streamer_issue_stream_cmd :: Ptr (RxStreamer s)
                                     -> Ptr StreamCmd
                                     -> IO UHDErrorEnum

-- | Send a stream command to the provided streamer.
rxStreamerIssueCmd :: RxStreamer s -> StreamCmd -> IO ()
rxStreamerIssueCmd (RxStreamer fp) sc =
    withForeignPtr fp $ \p ->
    withStreamCmd sc $ \scp ->
    throwOnUHDError $ uhd_rx_streamer_issue_stream_cmd p scp

foreign import capi safe "uhd/usrp/usrp.h uhd_rx_streamer_last_error"
    uhd_rx_streamer_last_error :: Ptr (RxStreamer s)
                               -> Ptr CChar
                               -> CSize
                               -> IO UHDErrorEnum

-- | The last UHD error reported by this streamer.
rxStreamerLastError :: RxStreamer s -> IO String
rxStreamerLastError (RxStreamer fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_rx_streamer_last_error p

-- | A handle for UHD state related to streaming samples from an TX chain,
--   parameterized by host side sample type.
data {-# CTYPE "uhd/usrp/usrp.h" "struct uhd_tx_streamer" #-}
    TxStreamer s = TxStreamer (ForeignPtr (TxStreamer s))

foreign import capi safe "uhd/usrp/usrp.h uhd_tx_streamer_make"
    uhd_tx_streamer_make :: Ptr (Ptr (TxStreamer s)) -> IO UHDErrorEnum

foreign import capi safe "uhd/usrp/usrp.h uhd_tx_streamer_free"
    uhd_tx_streamer_free :: Ptr (Ptr (TxStreamer s)) -> IO UHDErrorEnum

foreign import capi safe "uhd/usrp/usrp.h uhd_tx_streamer_num_channels"
    uhd_tx_streamer_num_channels :: Ptr (TxStreamer s)
                                 -> Ptr CSize
                                 -> IO UHDErrorEnum

-- | The number of channels setup for this streamer.
txStreamerNumChannels :: TxStreamer s -> IO Word64
txStreamerNumChannels (TxStreamer fp) =
    withForeignPtr fp $ \p ->
    alloca $ \np -> do
        throwOnUHDError $ uhd_tx_streamer_num_channels p np
        fromIntegral <$> peek np

foreign import capi safe "uhd/usrp/usrp.h uhd_tx_streamer_max_num_samps"
    uhd_tx_streamer_max_num_samps :: Ptr (TxStreamer s) -> Ptr CSize -> IO UHDErrorEnum

-- | The maximum samples per buffer per packet for this streamer.
txStreamerMaxNumSamps :: TxStreamer s -> IO Word64
txStreamerMaxNumSamps (TxStreamer fp) =
    withForeignPtr fp $ \p ->
    alloca $ \np -> do
        throwOnUHDError $ uhd_tx_streamer_max_num_samps p np
        fromIntegral <$> peek np

foreign import capi safe "uhd/usrp/usrp.h uhd_tx_streamer_send"
    uhd_tx_streamer_send :: Ptr (TxStreamer s)
                         -> Ptr (ConstPtr a)
                         -> CSize
                         -> Ptr (Ptr TxMetadata)
                         -> Double
                         -> Ptr CSize
                         -> IO UHDErrorEnum

txStreamerSend :: forall s. SampleType s
               => TxStreamer s
               -> V.Vector (VS.Vector s)
               -> TxMetadata
               -> Double
               -> IO (V.Vector Int)
txStreamerSend ts@(TxStreamer tsfp) cs (TxMetadata tmfp) tos =
    withForeignPtr tsfp $ \tsp ->
    withForeignPtr tmfp $ \tmp ->
    alloca $ \tmpp -> do
        cn <- fromIntegral <$> txStreamerNumChannels ts
        when (cn /= V.length cs) (throwIO IndexError)
        allocaArray cn $ \bsp ->
            allocaArray cn $ \osp -> do
                let imaxN = fromIntegral $ maximum $ fmap VS.length cs
                V.imapM
                    (\ci p -> pokeElemOff bsp ci
                        (unsafeForeignPtrToPtr (vsFP p))
                    )
                    cs
                poke tmpp tmp
                throwOnUHDError $
                    uhd_tx_streamer_send tsp
                                         (coerce bsp)
                                         imaxN
                                         tmpp
                                         tos
                                         osp
                ls <- V.generateM cn (fmap fromIntegral . peekElemOff osp)
                -- use used unsafeForeignPtrToPtr. Not sure it's necessary though.
                traverse_ touchVS cs
                pure ls
            
foreign import capi safe "uhd/usrp/usrp.h uhd_tx_streamer_recv_async_msg"
    uhd_tx_streamer_recv_async_msg :: Ptr (TxStreamer s)
                                   -> Ptr (Ptr AsyncMetadata)
                                   -> Double
                                   -> Ptr CBool
                                   -> IO UHDErrorEnum

txStreamerRecvAsyncMsg :: TxStreamer s
                       -> AsyncMetadata
                       -> Double
                       -> IO Bool
txStreamerRecvAsyncMsg (TxStreamer fp) (AsyncMetadata afp) tos =
    withForeignPtr fp $ \p ->
    withForeignPtr afp $ \ap ->
    alloca $ \app ->
    alloca $ \bp -> do
        poke app ap
        throwOnUHDError $ uhd_tx_streamer_recv_async_msg p app tos bp
        toBool <$> peek bp

foreign import capi safe "uhd/usrp/usrp.h uhd_tx_streamer_last_error"
    uhd_tx_streamer_last_error :: Ptr (TxStreamer s)
                               -> Ptr CChar
                               -> CSize
                               -> IO UHDErrorEnum

-- | The last UHD error reported by this streamer.
txStreamerLastError :: TxStreamer s -> IO String
txStreamerLastError (TxStreamer fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_tx_streamer_last_error p

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_find"
    uhd_usrp_find :: Ptr CChar
                  -> Ptr (Ptr StringVector)
                  -> IO UHDErrorEnum

findUSRPs :: String -> StringVector -> IO ()
findUSRPs args (StringVector sfp) =
    withCString args $ \argp ->
    withForeignPtr sfp $ \sp ->
    alloca $ \spp -> do
        poke spp sp
        throwOnUHDError $ uhd_usrp_find argp spp

-- | USRP device handle.
data {-# CTYPE "uhd/usrp/usrp.h" "struct uhd_usrp" #-}
    USRP = USRP (ForeignPtr USRP)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_make"
    uhd_usrp_make :: Ptr (Ptr USRP) -> Ptr CChar -> IO UHDErrorEnum

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_free"
    uhd_usrp_free :: Ptr (Ptr USRP) -> IO UHDErrorEnum

-- | Connect to a USRP.
makeUSRP :: String -- ^ Device address string.
         -> IO USRP
makeUSRP args =
    withCString args $ \argp ->
    alloca $ \p -> do
        throwOnUHDError $ uhd_usrp_make p argp
        mp <- peek p
        USRP <$> newForeignPtr mp (mkFreeByRef uhd_usrp_free mp)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_last_error"
    uhd_usrp_last_error :: Ptr USRP
                        -> Ptr CChar
                        -> CSize
                        -> IO UHDErrorEnum

-- | The last UHD error reported by this USRP.
usrpLastError :: USRP -> IO String
usrpLastError (USRP fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_usrp_last_error p

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_stream"
    uhd_usrp_get_rx_stream :: Ptr USRP
                           -> Ptr (StreamArgs s)
                           -> Ptr (RxStreamer s1)
                           -> IO UHDErrorEnum

-- | Setup an 'RxStreamer' with this USRP.
usrpGetRxStream :: forall s. SampleType s
                => USRP
                -> StreamArgs s
                -> IO (RxStreamer s)
usrpGetRxStream (USRP fp) sa =
    withForeignPtr fp $ \p ->
    withStreamArgs sa $ \sap ->
    alloca $ \rsp -> do
        throwOnUHDError $ uhd_rx_streamer_make rsp
        rsmp <- peek rsp
        rs@(RxStreamer rsfp) <-
            RxStreamer <$>
                newForeignPtr rsmp (mkFreeByRef uhd_rx_streamer_free rsmp)
        withForeignPtr rsfp $ \rsp' -> do
            throwOnUHDError $ uhd_usrp_get_rx_stream p sap rsp'
            pure rs

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_stream"
    uhd_usrp_get_tx_stream :: Ptr USRP
                           -> Ptr (StreamArgs s)
                           -> Ptr (TxStreamer s1)
                           -> IO UHDErrorEnum

-- | Setup a 'TxStreamer' with this USRP.
usrpGetTxStream :: forall s. SampleType s
                => USRP
                -> StreamArgs s
                -> IO (TxStreamer s)
usrpGetTxStream (USRP fp) sa =
    withForeignPtr fp $ \p ->
    withStreamArgs sa $ \sap ->
    alloca $ \tsp -> do
        throwOnUHDError $ uhd_tx_streamer_make tsp
        tsmp <- peek tsp
        ts@(TxStreamer tsfp) <-
            TxStreamer <$>
                newForeignPtr tsmp (mkFreeByRef uhd_tx_streamer_free tsmp)
        withForeignPtr tsfp $ \tsp' -> do
            throwOnUHDError $ uhd_usrp_get_tx_stream p sap tsp'
            pure ts

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_info"
    uhd_usrp_get_rx_info :: Ptr USRP -> CSize -> Ptr RxInfo -> IO UHDErrorEnum

-- | Get the provided channel's RX chain information.
usrpRxInfo :: USRP -> ChanIndex -> IO RxInfo
usrpRxInfo (USRP fp) ci =
    withForeignPtr fp $ \p ->
    allocaBytes #{size uhd_usrp_rx_info_t} $ \rip -> do
        throwOnUHDError $ uhd_usrp_get_rx_info p (fromIntegral ci) rip
        ri <- peekRxInfo rip
        throwOnUHDError $ uhd_usrp_rx_info_free rip
        pure ri

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_info"
    uhd_usrp_get_tx_info :: Ptr USRP -> CSize -> Ptr TxInfo -> IO UHDErrorEnum

-- | Get the provided channel's TX chain information.
usrpTxInfo :: USRP -> ChanIndex -> IO TxInfo
usrpTxInfo (USRP fp) ci =
    withForeignPtr fp $ \p ->
    allocaBytes #{size uhd_usrp_tx_info_t} $ \tip -> do
        throwOnUHDError $ uhd_usrp_get_tx_info p (fromIntegral ci) tip
        ti <- peekTxInfo tip
        throwOnUHDError $ uhd_usrp_tx_info_free tip
        pure ti

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_master_clock_rate"
    uhd_usrp_set_master_clock_rate :: Ptr USRP
                                   -> Double
                                   -> CSize
                                   -> IO UHDErrorEnum

type MotherboardIndex = Int

-- | Set the given USRP motherboard's master clock rate.
usrpSetMasterClockRate :: USRP
                       -> MotherboardIndex
                       -> Double -- ^ Rate in Hz.
                       -> IO ()
usrpSetMasterClockRate (USRP fp) mi f =
    withForeignPtr fp $ \p ->
    throwOnUHDError $ uhd_usrp_set_master_clock_rate p f (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_master_clock_rate"
    uhd_usrp_get_master_clock_rate :: Ptr USRP
                                   -> CSize
                                   -> Ptr Double
                                   -> IO UHDErrorEnum

-- | Get the given USRP motherboard's master clock rate.
usrpGetMasterClockRate :: USRP
                       -> MotherboardIndex
                       -> IO Double -- Rate in Hz.
usrpGetMasterClockRate (USRP fp) mi =
    withForeignPtr fp $ \p ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_get_master_clock_rate p (fromIntegral mi) dp
        peek dp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_pp_string"
    uhd_usrp_get_pp_string :: Ptr USRP -> Ptr CChar -> CSize -> IO UHDErrorEnum

-- | Get a string description of the provided USRP's properties.
usrpPrettyPrint :: USRP -> IO String
usrpPrettyPrint (USRP fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_usrp_get_pp_string p

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_mboard_name"
    uhd_usrp_get_mboard_name :: Ptr USRP
                             -> CSize
                             -> Ptr CChar
                             -> CSize
                             -> IO UHDErrorEnum

-- | Get the given motherboard's name.
usrpGetMotherboardName :: USRP -> MotherboardIndex -> IO String
usrpGetMotherboardName (USRP fp) mi =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_usrp_get_mboard_name p (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_time_now"
    uhd_usrp_get_time_now :: Ptr USRP
                          -> CSize
                          -> Ptr Int64
                          -> Ptr Double
                          -> IO UHDErrorEnum

-- | Query the current time of a given USRP motherboard.
usrpGetTimeNow :: USRP -> MotherboardIndex -> IO TimeSpec
usrpGetTimeNow (USRP fp) mi =
    withForeignPtr fp $ \p ->
    alloca $ \ip ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_get_time_now p (fromIntegral mi) ip dp
        fullSecs <- peek ip
        fracSecs <- peek dp
        pure TimeSpec{..}

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_time_last_pps"
    uhd_usrp_get_time_last_pps :: Ptr USRP
                               -> CSize
                               -> Ptr Int64
                               -> Ptr Double
                               -> IO UHDErrorEnum

-- | Query the time of the last PPS rising edge of a given USRP motherboard.
usrpGetLastPPS :: USRP -> MotherboardIndex -> IO TimeSpec
usrpGetLastPPS (USRP fp) mi =
    withForeignPtr fp $ \p ->
    alloca $ \ip ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_get_time_last_pps p (fromIntegral mi) ip dp
        fullSecs <- peek ip
        fracSecs <- peek dp
        pure TimeSpec{..}

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_time_now"
    uhd_usrp_set_time_now :: Ptr USRP
                          -> Int64
                          -> Double
                          -> CSize
                          -> IO UHDErrorEnum

usrpSetTimeNow :: USRP -> MotherboardIndex -> TimeSpec -> IO ()
usrpSetTimeNow (USRP fp) mi TimeSpec{..} =
    withForeignPtr fp $ \p ->
    throwOnUHDError $
        uhd_usrp_set_time_now p fullSecs fracSecs (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_time_next_pps"
    uhd_usrp_set_time_next_pps :: Ptr USRP
                               -> Int64
                               -> Double
                               -> CSize
                               -> IO UHDErrorEnum

usrpSetTimeNextPPS :: USRP -> MotherboardIndex -> TimeSpec -> IO ()
usrpSetTimeNextPPS (USRP fp) mi TimeSpec{..} =
    withForeignPtr fp $ \p ->
    throwOnUHDError $
        uhd_usrp_set_time_next_pps p fullSecs fracSecs (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_time_unknown_pps"
    uhd_usrp_set_time_unknown_pps :: Ptr USRP
                                  -> Int64
                                  -> Double
                                  -> IO UHDErrorEnum

usrpSetTimeUnknownPPS :: USRP -> TimeSpec -> IO ()
usrpSetTimeUnknownPPS (USRP fp) TimeSpec{..} =
    withForeignPtr fp $ \p ->
    throwOnUHDError $
        uhd_usrp_set_time_unknown_pps p fullSecs fracSecs

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_time_synchronized"
    uhd_usrp_get_time_synchronized :: Ptr USRP -> Ptr CBool -> IO UHDErrorEnum

-- | Whether or not all of this USRP's motherboard timers are synchronized.
usrpGetTimeSync :: USRP -> IO Bool
usrpGetTimeSync (USRP fp) =
    withForeignPtr fp $ \p ->
    alloca $ \bp -> do
        throwOnUHDError $ uhd_usrp_get_time_synchronized p bp
        toBool <$> peek bp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_command_time"
    uhd_usrp_set_command_time :: Ptr USRP
                              -> Int64
                              -> Double
                              -> CSize
                              -> IO UHDErrorEnum

-- | Set the time at which the next timed command will take place.
usrpSetCommandTime :: USRP -> MotherboardIndex -> TimeSpec -> IO ()
usrpSetCommandTime (USRP fp) mi TimeSpec{..} =
    withForeignPtr fp $ \p ->
    throwOnUHDError $
        uhd_usrp_set_command_time p fullSecs fracSecs (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_clear_command_time"
    uhd_usrp_clear_command_time :: Ptr USRP
                                -> CSize
                                -> IO UHDErrorEnum

-- | Clear any set command time. Following this, any timed commands will be
--   executed as soon as possible instead.
usrpClearCommandTime :: USRP -> MotherboardIndex -> IO ()
usrpClearCommandTime (USRP fp) mi =
    withForeignPtr fp $ \p ->
    throwOnUHDError $
        uhd_usrp_clear_command_time p (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_time_source"
    uhd_usrp_set_time_source :: Ptr USRP
                             -> Ptr CChar
                             -> CSize
                             -> IO UHDErrorEnum

-- | Set the time source for the given USRP motherboard.
usrpSetTimeSource :: USRP -> MotherboardIndex -> String -> IO ()
usrpSetTimeSource (USRP fp) mi ts =
    withForeignPtr fp $ \p ->
    withCString ts $ \tsp ->
    throwOnUHDError $ uhd_usrp_set_time_source p tsp (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_time_source"
    uhd_usrp_get_time_source :: Ptr USRP
                             -> CSize
                             -> Ptr CChar
                             -> CSize
                             -> IO UHDErrorEnum

-- | Get the current time source for the given USRP motherboard.
usrpGetTimeSource :: USRP -> MotherboardIndex -> IO String
usrpGetTimeSource (USRP fp) mi =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_usrp_get_time_source p (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_time_sources"
    uhd_usrp_get_time_sources :: Ptr USRP
                              -> CSize
                              -> Ptr (Ptr StringVector)
                              -> IO UHDErrorEnum

usrpGetTimeSources :: USRP -> MotherboardIndex -> StringVector -> IO ()
usrpGetTimeSources (USRP fp) mi (StringVector svfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr svfp $ \svp ->
    alloca $ \svpp -> do
        poke svpp svp
        throwOnUHDError $ uhd_usrp_get_time_sources p (fromIntegral mi) svpp


foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_clock_source"
    uhd_usrp_set_clock_source :: Ptr USRP
                              -> Ptr CChar
                              -> CSize
                              -> IO UHDErrorEnum

-- | Set the clock source for the given USRP motherboard.
usrpSetClockSource :: USRP -> MotherboardIndex -> String -> IO ()
usrpSetClockSource (USRP fp) mi ts =
    withForeignPtr fp $ \p ->
    withCString ts $ \tsp ->
    throwOnUHDError $ uhd_usrp_set_clock_source p tsp (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_clock_source"
    uhd_usrp_get_clock_source :: Ptr USRP
                              -> CSize
                              -> Ptr CChar
                              -> CSize
                              -> IO UHDErrorEnum

-- | Get the given USRP motherboard's clock source.
usrpGetClockSource :: USRP -> MotherboardIndex -> IO String
usrpGetClockSource (USRP fp) mi =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_usrp_get_clock_source p (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_clock_sources"
    uhd_usrp_get_clock_sources :: Ptr USRP
                               -> CSize
                               -> Ptr (Ptr StringVector)
                               -> IO UHDErrorEnum

usrpGetClockSourcesSV :: USRP -> MotherboardIndex -> StringVector -> IO ()
usrpGetClockSourcesSV (USRP fp) mi (StringVector svfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr svfp $ \svp ->
    alloca $ \svpp -> do
        poke svpp svp
        throwOnUHDError $ uhd_usrp_get_clock_sources p (fromIntegral mi) svpp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_clock_source_out"
    uhd_usrp_set_clock_source_out :: Ptr USRP
                                  -> CBool
                                  -> CSize
                                  -> IO UHDErrorEnum

-- | Whether or not the given USRP motherboard should output a clock reference
--   signal.
usrpSetClockSourceOutput :: USRP -> MotherboardIndex -> Bool -> IO ()
usrpSetClockSourceOutput (USRP fp) mi b =
    withForeignPtr fp $ \p ->
    throwOnUHDError $
        uhd_usrp_set_clock_source_out p (fromBool b) (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_time_source_out"
    uhd_usrp_set_time_source_out :: Ptr USRP
                                 -> CBool
                                 -> CSize
                                 -> IO UHDErrorEnum

-- | Whether or not the given USRP motherboard should output a time reference
--   signal.
usrpSetTimeSourceOutput :: USRP -> MotherboardIndex -> Bool -> IO ()
usrpSetTimeSourceOutput (USRP fp) mi b =
    withForeignPtr fp $ \p ->
    throwOnUHDError $
        uhd_usrp_set_time_source_out p (fromBool b) (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_num_mboards"
    uhd_usrp_get_num_mboards :: Ptr USRP -> Ptr CSize -> IO UHDErrorEnum

-- | Get the number of motherboards in the USRP. Many configuration options are
--   set per motherboard index.
usrpGetNumMotherboards :: USRP -> IO Int
usrpGetNumMotherboards (USRP fp) =
    withForeignPtr fp $ \p ->
    alloca $ \sp -> do
        throwOnUHDError $ uhd_usrp_get_num_mboards p sp
        fromIntegral <$> peek sp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_mboard_sensor"
    uhd_usrp_get_mboard_sensor :: Ptr USRP
                               -> Ptr CChar
                               -> CSize
                               -> Ptr (Ptr SensorValue)
                               -> IO UHDErrorEnum

usrpGetMotherboardSensor :: USRP
                         -> MotherboardIndex
                         -> String
                         -> SensorValue
                         -> IO ()
usrpGetMotherboardSensor (USRP fp) mi name (SensorValue svfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr svfp $ \svp ->
    withCString name $ \np ->
    alloca $ \svpp -> do
        poke svpp svp
        throwOnUHDError $
            uhd_usrp_get_mboard_sensor p np (fromIntegral mi) svpp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_mboard_sensor_names"
    uhd_usrp_get_mboard_sensor_names :: Ptr USRP
                                     -> CSize
                                     -> Ptr (Ptr StringVector)
                                     -> IO UHDErrorEnum

usrpGetMotherboardSensorNamesSV :: USRP
                                -> MotherboardIndex
                                -> StringVector
                                -> IO ()
usrpGetMotherboardSensorNamesSV (USRP fp) mi (StringVector svfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr svfp $ \svp ->
    alloca $ \svpp -> do
        poke svpp svp
        throwOnUHDError $
            uhd_usrp_get_mboard_sensor_names p (fromIntegral mi) svpp


foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_user_register"
    uhd_usrp_set_user_register :: Ptr USRP
                               -> Word8
                               -> Word32
                               -> CSize
                               -> IO UHDErrorEnum

-- | Perform a write on a user configuration register bus. See the UHD manual
--   for details.
usrpSetUserReg :: USRP
               -> MotherboardIndex
               -> Word8 -- ^ Address.
               -> Word32 -- ^ Data.
               -> IO ()
usrpSetUserReg (USRP fp) mi a d =
    withForeignPtr fp $ \p ->
    throwOnUHDError $ uhd_usrp_set_user_register p a d (fromIntegral mi)

-- | USRP motherboard EEPROM key/value dictionary.
data {-# CTYPE "uhd/usrp/mboard_eeprom.h" "struct uhd_mboard_eeprom_t" #-}
    MotherboardEEPROM = MotherboardEEPROM (ForeignPtr MotherboardEEPROM)

foreign import capi safe "uhd/usrp/mboard_eeprom.h uhd_mboard_eeprom_make"
    uhd_mboard_eeprom_make :: Ptr (Ptr MotherboardEEPROM) -> IO UHDErrorEnum

foreign import capi safe "uhd/usrp/mboard_eeprom.h uhd_mboard_eeprom_free"
    uhd_mboard_eeprom_free :: Ptr (Ptr MotherboardEEPROM) -> IO UHDErrorEnum

makeMotherboardEEPROM :: IO MotherboardEEPROM
makeMotherboardEEPROM = alloca $ \p -> do
    throwOnUHDError $ uhd_mboard_eeprom_make p
    mp <- peek p
    MotherboardEEPROM <$>
        newForeignPtr mp (mkFreeByRef uhd_mboard_eeprom_free mp)

foreign import capi safe "uhd/usrp/mboard_eeprom.h uhd_mboard_eeprom_get_value"
    uhd_mboard_eeprom_get_value :: Ptr MotherboardEEPROM
                                -> Ptr CChar
                                -> Ptr CChar
                                -> CSize
                                -> IO UHDErrorEnum

-- | Get this EEPROM dictionary key's value.
motherboardEEPROMGetValue :: MotherboardEEPROM
                          -> String -- ^ Key.
                          -> IO String
motherboardEEPROMGetValue (MotherboardEEPROM fp) key =
    withForeignPtr fp $ \p ->
    withCString key $ \kp ->
    getUHDString 2048 $ uhd_mboard_eeprom_get_value p kp

foreign import capi safe "uhd/usrp/mboard_eeprom.h uhd_mboard_eeprom_set_value"
    uhd_mboard_eeprom_set_value :: Ptr MotherboardEEPROM
                                -> Ptr CChar
                                -> Ptr CChar
                                -> IO UHDErrorEnum

-- | Set this EEPROM dictionary key's value.
motherboardEEPROMSetValue :: MotherboardEEPROM
                          -> String -- ^ Key.
                          -> String -- ^ Value.
                          -> IO ()
motherboardEEPROMSetValue (MotherboardEEPROM fp) key val =
    withForeignPtr fp $ \p ->
    withCString key $ \kp ->
    withCString val $ \vp ->
    throwOnUHDError $ uhd_mboard_eeprom_set_value p kp vp

foreign import capi safe "uhd/usrp/mboard_eeprom.h uhd_mboard_eeprom_last_error"
    uhd_mboard_eeprom_last_error :: Ptr MotherboardEEPROM
                                 -> Ptr CChar
                                 -> CSize
                                 -> IO UHDErrorEnum

motherboardEEPROMLastError :: MotherboardEEPROM -> IO String
motherboardEEPROMLastError (MotherboardEEPROM fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_mboard_eeprom_last_error p

-- | USRP daughterboard EEPROM key/value dictionary.
data {-# CTYPE "uhd/usrp/dboard_eeprom.h" "struct uhd_dboard_eeprom_t" #-}
    DaughterboardEEPROM = DaughterboardEEPROM (ForeignPtr DaughterboardEEPROM)

foreign import capi safe "uhd/usrp/dboard_eeprom.h uhd_dboard_eeprom_make"
    uhd_dboard_eeprom_make :: Ptr (Ptr DaughterboardEEPROM) -> IO UHDErrorEnum

foreign import capi safe "uhd/usrp/dboard_eeprom.h uhd_dboard_eeprom_free"
    uhd_dboard_eeprom_free :: Ptr (Ptr DaughterboardEEPROM) -> IO UHDErrorEnum

makeDaughterboardEEPROM :: IO DaughterboardEEPROM
makeDaughterboardEEPROM = alloca $ \p -> do
    throwOnUHDError $ uhd_dboard_eeprom_make p
    mp <- peek p
    DaughterboardEEPROM <$>
        newForeignPtr mp (mkFreeByRef uhd_dboard_eeprom_free mp)

foreign import capi safe "uhd/usrp/dboard_eeprom.h uhd_dboard_eeprom_get_id"
    uhd_dboard_eeprom_get_id :: Ptr DaughterboardEEPROM
                             -> Ptr CChar
                             -> CSize
                             -> IO UHDErrorEnum

daughterboardEEPROMGetId :: DaughterboardEEPROM -> IO String
daughterboardEEPROMGetId (DaughterboardEEPROM fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_dboard_eeprom_get_id p

foreign import capi safe "uhd/usrp/dboard_eeprom.h uhd_dboard_eeprom_set_id"
    uhd_dboard_eeprom_set_id :: Ptr DaughterboardEEPROM
                             -> Ptr CChar
                             -> IO UHDErrorEnum

daughterboardEEPROMSetId :: DaughterboardEEPROM -> String -> IO ()
daughterboardEEPROMSetId (DaughterboardEEPROM fp) i =
    withForeignPtr fp $ \p ->
    withCString i $ \ip ->
    throwOnUHDError $ uhd_dboard_eeprom_set_id p ip

foreign import capi safe "uhd/usrp/dboard_eeprom.h uhd_dboard_eeprom_get_serial"
    uhd_dboard_eeprom_get_serial :: Ptr DaughterboardEEPROM
                                 -> Ptr CChar
                                 -> CSize
                                 -> IO UHDErrorEnum

daughterboardEEPROMGetSerial :: DaughterboardEEPROM -> IO String
daughterboardEEPROMGetSerial (DaughterboardEEPROM fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_dboard_eeprom_get_serial p

foreign import capi safe "uhd/usrp/dboard_eeprom.h uhd_dboard_eeprom_set_serial"
    uhd_dboard_eeprom_set_serial :: Ptr DaughterboardEEPROM
                                 -> Ptr CChar
                                 -> IO UHDErrorEnum

daughterboardEEPROMSetSerial :: DaughterboardEEPROM -> String -> IO ()
daughterboardEEPROMSetSerial (DaughterboardEEPROM fp) s =
    withForeignPtr fp $ \p ->
    withCString s $ \sp ->
    throwOnUHDError $ uhd_dboard_eeprom_set_serial p sp

foreign import capi safe "uhd/usrp/dboard_eeprom.h uhd_dboard_eeprom_get_revision"
    uhd_dboard_eeprom_get_revision :: Ptr DaughterboardEEPROM
                                   -> Ptr CInt
                                   -> IO UHDErrorEnum

daughterboardEEPROMGetRevision :: DaughterboardEEPROM -> IO Int
daughterboardEEPROMGetRevision (DaughterboardEEPROM fp) =
    withForeignPtr fp $ \p ->
    alloca $ \ip -> do
        throwOnUHDError $ uhd_dboard_eeprom_get_revision p ip
        fromIntegral <$> peek ip

foreign import capi safe "uhd/usrp/dboard_eeprom.h uhd_dboard_eeprom_set_revision"
    uhd_dboard_eeprom_set_revision :: Ptr DaughterboardEEPROM
                                   -> CInt
                                   -> IO UHDErrorEnum

daughterboardEEPROMSetRevision :: DaughterboardEEPROM -> Int -> IO ()
daughterboardEEPROMSetRevision (DaughterboardEEPROM fp) i =
    withForeignPtr fp $ \p ->
    throwOnUHDError $ uhd_dboard_eeprom_set_revision p (fromIntegral i)

foreign import capi safe "uhd/usrp/dboard_eeprom.h uhd_dboard_eeprom_last_error"
    uhd_dboard_eeprom_last_error :: Ptr DaughterboardEEPROM
                                 -> Ptr CChar
                                 -> CSize
                                 -> IO UHDErrorEnum

daughterboardEEPROMLastError :: DaughterboardEEPROM -> IO String
daughterboardEEPROMLastError (DaughterboardEEPROM fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_dboard_eeprom_last_error p

data {-# CTYPE "uhd/usrp/subdev_spec.h" "uhd_subdev_spec_pair_t" #-}
    SubdevSpecPair = SubdevSpecPair {
        sspDaughterboardName :: String
      , sspSubDeviceName :: String
      } deriving stock ( Eq
                       , Ord
                       , Read
                       , Show
                       , Generic
                       )

foreign import capi safe "uhd/usrp/subdev_spec.h uhd_subdev_spec_pair_free"
    uhd_subdev_spec_pair_free :: Ptr SubdevSpecPair -> IO UHDErrorEnum

peekSubdevSpecPair :: Ptr SubdevSpecPair -> IO SubdevSpecPair
peekSubdevSpecPair p = do
    sspDaughterboardName <- peekCString (#{ptr uhd_subdev_spec_pair_t,db_name} p)
    sspSubDeviceName <- peekCString (#{ptr uhd_subdev_spec_pair_t,sd_name} p)
    pure SubdevSpecPair{..}

foreign import capi safe "uhd/usrp/subdev_spec.h uhd_subdev_spec_pairs_equal"
    uhd_subdev_spec_pairs_equal :: Ptr SubdevSpecPair
                                -> Ptr SubdevSpecPair
                                -> Ptr CBool
                                -> IO UHDErrorEnum

data {-# CTYPE "uhd/usrp/subdev_spec.h" "struct uhd_subdev_spec_t" #-}
    SubdevSpec = SubdevSpec (ForeignPtr SubdevSpec)

foreign import capi safe "uhd/usrp/subdev_spec.h uhd_subdev_spec_make"
    uhd_subdev_spec_make :: Ptr (Ptr SubdevSpec)
                         -> Ptr CChar
                         -> IO UHDErrorEnum

foreign import capi safe "uhd/usrp/subdev_spec.h uhd_subdev_spec_free"
    uhd_subdev_spec_free :: Ptr (Ptr SubdevSpec) -> IO UHDErrorEnum

-- | Make a 'SubdevSpec' from a markup string.
makeSubdevSpec :: String -> IO SubdevSpec
makeSubdevSpec mk =
    withCString mk $ \mkp ->
    alloca $ \p -> do
        throwOnUHDError $ uhd_subdev_spec_make p mkp
        mp <- peek p
        SubdevSpec <$> newForeignPtr mp (mkFreeByRef uhd_subdev_spec_free mp)

foreign import capi safe "uhd/usrp/subdev_spec.h uhd_subdev_spec_size"
    uhd_subdev_spec_size :: Ptr SubdevSpec -> Ptr CSize -> IO UHDErrorEnum

subdevSpecSize :: SubdevSpec -> IO Int
subdevSpecSize (SubdevSpec fp) =
    withForeignPtr fp $ \p ->
    alloca $ \sp -> do
        throwOnUHDError $ uhd_subdev_spec_size p sp
        fromIntegral <$> peek sp

foreign import capi safe "uhd/usrp/subdev_spec.h uhd_subdev_spec_push_back"
    uhd_subdev_spec_push_back :: Ptr SubdevSpec
                              -> Ptr CChar
                              -> IO UHDErrorEnum

subdevSpecPushBack :: SubdevSpec -> String -> IO ()
subdevSpecPushBack (SubdevSpec fp) mk =
    withForeignPtr fp $ \p ->
    withCString mk $ \mkp ->
    throwOnUHDError $ uhd_subdev_spec_push_back p mkp

foreign import capi safe "uhd/usrp/subdev_spec.h uhd_subdev_spec_at"
    uhd_subdev_spec_at :: Ptr SubdevSpec
                       -> CSize
                       -> Ptr SubdevSpecPair
                       -> IO UHDErrorEnum

subdevSpecAt :: SubdevSpec -> Int -> IO SubdevSpecPair
subdevSpecAt (SubdevSpec fp) i =
    withForeignPtr fp $ \p ->
    allocaBytes #{size uhd_subdev_spec_pair_t} $ \ssp -> do
        throwOnUHDError $ uhd_subdev_spec_at p (fromIntegral i) ssp
        ss <- peekSubdevSpecPair ssp
        throwOnUHDError $ uhd_subdev_spec_pair_free ssp
        pure ss

foreign import capi safe "uhd/usrp/subdev_spec.h uhd_subdev_spec_to_pp_string"
    uhd_subdev_spec_to_pp_string :: Ptr SubdevSpec
                                 -> Ptr CChar
                                 -> CSize
                                 -> IO UHDErrorEnum

subdevSpecPrettyPrint :: SubdevSpec -> IO String
subdevSpecPrettyPrint (SubdevSpec fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_subdev_spec_to_pp_string p

foreign import capi safe "uhd/usrp/subdev_spec.h uhd_subdev_spec_to_string"
    uhd_subdev_spec_to_string :: Ptr SubdevSpec
                              -> Ptr CChar
                              -> CSize
                              -> IO UHDErrorEnum

-- | Turn a 'SubdevSpec' back into a markup string.
subdevSpecMarkupString :: SubdevSpec -> IO String
subdevSpecMarkupString (SubdevSpec fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_subdev_spec_to_string p

foreign import capi safe "uhd/usrp/subdev_spec.h uhd_subdev_spec_last_error"
    uhd_subdev_spec_last_error :: Ptr SubdevSpec
                               -> Ptr CChar
                               -> CSize
                               -> IO UHDErrorEnum

subdevSpecLastError :: SubdevSpec -> IO String
subdevSpecLastError (SubdevSpec fp) =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_subdev_spec_last_error p

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_mboard_eeprom"
    uhd_usrp_get_mboard_eeprom :: Ptr USRP
                               -> Ptr MotherboardEEPROM
                               -> CSize
                               -> IO UHDErrorEnum

-- | Get the subdevice pairs out of a `SubdevSpec`.
fromSubdevSpec :: SubdevSpec -> IO [SubdevSpecPair]
fromSubdevSpec ss = do
    s <- subdevSpecSize ss
    traverse (subdevSpecAt ss) [0..(s-1)]

fromSubdevSpecF :: (SubdevSpec -> IO ()) -> IO [SubdevSpecPair]
fromSubdevSpecF f = do
    ss <- makeSubdevSpec ""
    f ss
    fromSubdevSpec ss

usrpGetMotherboardEEPROM :: USRP
                         -> MotherboardIndex
                         -> MotherboardEEPROM
                         -> IO ()
usrpGetMotherboardEEPROM (USRP fp) mi (MotherboardEEPROM mfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr mfp $ \mp ->
    throwOnUHDError $ uhd_usrp_get_mboard_eeprom p mp (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_mboard_eeprom"
    uhd_usrp_set_mboard_eeprom :: Ptr USRP
                               -> Ptr MotherboardEEPROM
                               -> CSize
                               -> IO UHDErrorEnum

-- | Set this motherboard's EEPROM dictionary
usrpSetMotherboardEEPROM :: USRP
                         -> MotherboardIndex
                         -> MotherboardEEPROM
                         -> IO ()
usrpSetMotherboardEEPROM (USRP fp) mi (MotherboardEEPROM mfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr mfp $ \mp ->
    throwOnUHDError $ uhd_usrp_set_mboard_eeprom p mp (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_dboard_eeprom"
    uhd_usrp_get_dboard_eeprom :: Ptr USRP
                               -> Ptr DaughterboardEEPROM
                               -> Ptr CChar
                               -> Ptr CChar
                               -> CSize
                               -> IO UHDErrorEnum

-- | Get this daughterboard's EEPROM dictionary.
usrpGetDaughterboardEEPROM :: USRP
                           -> MotherboardIndex
                           -> String -- ^ Unit name.
                           -> String -- ^ Slot name.
                           -> DaughterboardEEPROM
                           -> IO ()
usrpGetDaughterboardEEPROM (USRP fp) mi unit slot (DaughterboardEEPROM dfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr dfp $ \dp ->
    withCString unit $ \up ->
    withCString slot $ \sp ->
    throwOnUHDError $ uhd_usrp_get_dboard_eeprom p dp up sp (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_dboard_eeprom"
    uhd_usrp_set_dboard_eeprom :: Ptr USRP
                               -> Ptr DaughterboardEEPROM
                               -> Ptr CChar
                               -> Ptr CChar
                               -> CSize
                               -> IO UHDErrorEnum

-- | Set this daughterboard's EEPROM dictionary.
usrpSetDaughterboardEEPROM :: USRP
                           -> MotherboardIndex
                           -> String -- ^ Unit name.
                           -> String -- ^ Slot name.
                           -> DaughterboardEEPROM
                           -> IO ()
usrpSetDaughterboardEEPROM (USRP fp) mi unit slot (DaughterboardEEPROM dfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr dfp $ \dp ->
    withCString unit $ \up ->
    withCString slot $ \sp ->
    throwOnUHDError $ uhd_usrp_set_dboard_eeprom p dp up sp (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_rx_subdev_spec"
    uhd_usrp_set_rx_subdev_spec :: Ptr USRP
                                -> Ptr SubdevSpec
                                -> CSize
                                -> IO UHDErrorEnum

usrpSetRxSubdevSpec :: USRP -> MotherboardIndex -> SubdevSpec -> IO ()
usrpSetRxSubdevSpec (USRP fp) mi (SubdevSpec sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    throwOnUHDError $ uhd_usrp_set_rx_subdev_spec p sp (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_subdev_spec"
    uhd_usrp_get_rx_subdev_spec :: Ptr USRP
                                -> CSize
                                -> Ptr SubdevSpec
                                -> IO UHDErrorEnum

usrpGetRxSubdevSpec :: USRP -> MotherboardIndex -> SubdevSpec -> IO ()
usrpGetRxSubdevSpec (USRP fp) mi (SubdevSpec sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    throwOnUHDError $ uhd_usrp_get_rx_subdev_spec p (fromIntegral mi) sp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_num_channels"
    uhd_usrp_get_rx_num_channels :: Ptr USRP
                                 -> Ptr CSize
                                 -> IO UHDErrorEnum

-- | Get the number of Rx channels available on the given USRP.
usrpGetRxNumChannels :: USRP -> IO Int
usrpGetRxNumChannels (USRP fp) =
    withForeignPtr fp $ \p ->
    alloca $ \sp -> do
        throwOnUHDError $ uhd_usrp_get_rx_num_channels p sp
        fromIntegral <$> peek sp

type ChanIndex = Int

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_subdev_name"
    uhd_usrp_get_rx_subdev_name :: Ptr USRP
                                -> CSize
                                -> Ptr CChar
                                -> CSize
                                -> IO UHDErrorEnum

-- | Get the name of the subdevice providing the given Rx channel on the given
--   USRP.
usrpGetRxSubdevName :: USRP -> ChanIndex -> IO String
usrpGetRxSubdevName (USRP fp) ci =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_usrp_get_rx_subdev_name p (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_rx_rate"
    uhd_usrp_set_rx_rate :: Ptr USRP -> Double -> CSize -> IO UHDErrorEnum

-- | Set this USRP's Rx channel's sampling rate in Hz.
usrpSetRxRate :: USRP -> ChanIndex -> Double -> IO ()
usrpSetRxRate (USRP fp) ci r =
    withForeignPtr fp $ \p ->
    throwOnUHDError $ uhd_usrp_set_rx_rate p r (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_rate"
    uhd_usrp_get_rx_rate :: Ptr USRP -> CSize -> Ptr Double -> IO UHDErrorEnum

-- | Get this USRP's Rx channel's sampling rate in Hz.
usrpGetRxRate :: USRP -> ChanIndex -> IO Double
usrpGetRxRate (USRP fp) ci =
    withForeignPtr fp $ \p ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_get_rx_rate p (fromIntegral ci) dp
        peek dp


foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_rates"
    uhd_usrp_get_rx_rates :: Ptr USRP
                          -> CSize
                          -> Ptr MetaRange
                          -> IO UHDErrorEnum

usrpGetRxRates :: USRP -> ChanIndex -> MetaRange -> IO ()
usrpGetRxRates (USRP fp) ci (MetaRange mfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr mfp $ \mp ->
    throwOnUHDError $ uhd_usrp_get_rx_rates p (fromIntegral ci) mp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_rx_freq"
    uhd_usrp_set_rx_freq :: Ptr USRP
                         -> Ptr TuneReq
                         -> CSize
                         -> Ptr TuneRes
                         -> IO UHDErrorEnum

-- | Tune this Rx channel's receive chain according to the given tune request.
usrpSetRxFreq :: USRP
              -> ChanIndex
              -> TuneReq
              -> IO TuneRes
usrpSetRxFreq (USRP fp) ci tq =
    withForeignPtr fp $ \p ->
    withTuneReq tq $ \tqp ->
    alloca $ \tsp -> do
        throwOnUHDError $ uhd_usrp_set_rx_freq p tqp (fromIntegral ci) tsp
        peek tsp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_freq"
    uhd_usrp_get_rx_freq :: Ptr USRP
                         -> CSize
                         -> Ptr Double
                         -> IO UHDErrorEnum

-- | Get this Rx channel's tune frequency in Hz.
usrpGetRxFreq :: USRP -> ChanIndex -> IO Double
usrpGetRxFreq (USRP fp) ci =
    withForeignPtr fp $ \p ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_get_rx_freq p (fromIntegral ci) dp
        peek dp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_freq_range"
    uhd_usrp_get_rx_freq_range :: Ptr USRP
                               -> CSize
                               -> Ptr MetaRange
                               -> IO UHDErrorEnum

usrpGetRxFreqRange :: USRP -> ChanIndex -> MetaRange -> IO ()
usrpGetRxFreqRange (USRP fp) ci (MetaRange mfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr mfp $ \mp ->
    throwOnUHDError $ uhd_usrp_get_rx_freq_range p (fromIntegral ci) mp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_fe_rx_freq_range"
    uhd_usrp_get_fe_rx_freq_range :: Ptr USRP
                                  -> CSize
                                  -> Ptr MetaRange
                                  -> IO UHDErrorEnum

usrpGetFrontEndRxFreqRange :: USRP -> ChanIndex -> MetaRange -> IO ()
usrpGetFrontEndRxFreqRange (USRP fp) ci (MetaRange mfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr mfp $ \mp ->
    throwOnUHDError $ uhd_usrp_get_fe_rx_freq_range p (fromIntegral ci) mp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_lo_names"
    uhd_usrp_get_rx_lo_names :: Ptr USRP
                             -> CSize
                             -> Ptr (Ptr StringVector)
                             -> IO UHDErrorEnum

usrpGetRxLONamesSV :: USRP -> ChanIndex -> StringVector -> IO ()
usrpGetRxLONamesSV (USRP fp) ci (StringVector sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    alloca $ \spp -> do
        poke spp sp
        throwOnUHDError $ uhd_usrp_get_rx_lo_names p (fromIntegral ci) spp


foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_rx_lo_source"
    uhd_usrp_set_rx_lo_source :: Ptr USRP
                              -> Ptr CChar
                              -> Ptr CChar
                              -> CSize
                              -> IO UHDErrorEnum

-- | Set this Rx channel's named LO source.
usrpSetRxLOSource :: USRP
                  -> ChanIndex
                  -> String -- ^ Name.
                  -> String -- ^ Source.
                  -> IO ()
usrpSetRxLOSource (USRP fp) ci name src =
    withForeignPtr fp $ \p ->
    withCString src $ \sp ->
    withCString name $ \np ->
    throwOnUHDError $ uhd_usrp_set_rx_lo_source p sp np (fromIntegral ci)

foreign import capi "uhd/usrp/usrp.h uhd_usrp_get_rx_lo_source"
    uhd_usrp_get_rx_lo_source :: Ptr USRP
                              -> Ptr CChar
                              -> CSize
                              -> Ptr CChar
                              -> CSize
                              -> IO UHDErrorEnum


-- | Get this channel's named LO source.
usrpGetRxLOSource :: USRP
                  -> ChanIndex
                  -> String -- ^ Name.
                  -> IO String
usrpGetRxLOSource (USRP fp) ci name =
    withForeignPtr fp $ \p ->
    withCString name $ \np ->
    getUHDString 2048 $ uhd_usrp_get_rx_lo_source p np (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_lo_sources"
    uhd_usrp_get_rx_lo_sources :: Ptr USRP
                               -> Ptr CChar
                               -> CSize
                               -> Ptr (Ptr StringVector)
                               -> IO UHDErrorEnum

usrpGetRxLOSourcesSV :: USRP -> ChanIndex -> String -> StringVector -> IO ()
usrpGetRxLOSourcesSV (USRP fp) ci name (StringVector sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    withCString name $ \np -> 
    alloca $ \spp -> do
        poke spp sp
        throwOnUHDError $ uhd_usrp_get_rx_lo_sources p np (fromIntegral ci) spp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_rx_lo_export_enabled"
    uhd_usrp_set_rx_lo_export_enabled :: Ptr USRP
                                      -> CBool
                                      -> Ptr CChar
                                      -> CSize
                                      -> IO UHDErrorEnum

-- | Set whether or not this LO slot on this Rx channel is exported.
usrpSetRxLOExport :: USRP
                  -> ChanIndex
                  -> String -- ^ Name.
                  -> Bool
                  -> IO ()
usrpSetRxLOExport (USRP fp) ci name en =
    withForeignPtr fp $ \p -> 
    withCString name $ \np ->
    throwOnUHDError $
        uhd_usrp_set_rx_lo_export_enabled p (fromBool en) np (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_lo_export_enabled"
    uhd_usrp_get_rx_lo_export_enabled :: Ptr USRP
                                      -> Ptr CChar
                                      -> CSize
                                      -> Ptr CBool
                                      -> IO UHDErrorEnum

-- | Get whether or not this LO slot on this Rx channel is exported.
usrpGetRxLOExport :: USRP
                  -> ChanIndex
                  -> String -- ^ Name.
                  -> IO Bool
usrpGetRxLOExport (USRP fp) ci name =
    withForeignPtr fp $ \p ->
    withCString name $ \np ->
    alloca $ \bp -> do
        throwOnUHDError $
            uhd_usrp_get_rx_lo_export_enabled p np (fromIntegral ci) bp
        toBool <$> peek bp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_rx_lo_freq"
    uhd_usrp_set_rx_lo_freq :: Ptr USRP
                            -> Double
                            -> Ptr CChar
                            -> CSize
                            -> Ptr Double
                            -> IO UHDErrorEnum

-- | Set this Rx channel's named LO slot's frequency in Hz, returning the
--   actually realized frequency.
usrpSetRxLOFreq :: USRP
                -> ChanIndex
                -> String -- ^ Name.
                -> Double
                -> IO Double
usrpSetRxLOFreq (USRP fp) ci name f =
    withForeignPtr fp $ \p ->
    withCString name $ \np ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_set_rx_lo_freq p f np (fromIntegral ci) dp
        peek dp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_lo_freq"
    uhd_usrp_get_rx_lo_freq :: Ptr USRP
                            -> Ptr CChar
                            -> CSize
                            -> Ptr Double
                            -> IO UHDErrorEnum

-- | Get this Rx channel's named LO slot's frequency in Hz, returning the
--   actually realized frequency.
usrpGetRxLOFreq :: USRP
                -> ChanIndex
                -> String -- ^ Name.
                -> IO Double
usrpGetRxLOFreq (USRP fp) ci name =
    withForeignPtr fp $ \p ->
    withCString name $ \np ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_get_rx_lo_freq p np (fromIntegral ci) dp
        peek dp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_rx_gain"
    uhd_usrp_set_rx_gain :: Ptr USRP
                         -> Double
                         -> CSize
                         -> Ptr CChar
                         -> IO UHDErrorEnum

-- | Set this Rx channel's named gain stage in dB. If there is only one gain
--   stage, the empty string may be used.
usrpSetRxGain :: USRP
              -> ChanIndex
              -> String -- ^ Name.
              -> Double -- ^ Gain in dB.
              -> IO ()
usrpSetRxGain (USRP fp) ci name g =
    withForeignPtr fp $ \p ->
    withCString name $ \np ->
    throwOnUHDError $ uhd_usrp_set_rx_gain p g (fromIntegral ci) np

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_normalized_rx_gain"
    uhd_usrp_set_normalized_rx_gain :: Ptr USRP
                                    -> Double
                                    -> CSize
                                    -> IO UHDErrorEnum

-- | Set this Rx channel's normalized gain (in the range [0 .. 1]) for the
--   entire RF chain.
usrpSetRxGainNormalized :: USRP -> ChanIndex -> Double -> IO ()
usrpSetRxGainNormalized (USRP fp) ci ng =
    withForeignPtr fp $ \p ->
    throwOnUHDError $
        uhd_usrp_set_normalized_rx_gain p ng (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_rx_agc"
    uhd_usrp_set_rx_agc :: Ptr USRP -> CBool -> CSize -> IO UHDErrorEnum

-- | Enable or disable this Rx channel's AGC.
usrpSetRxAGC :: USRP -> ChanIndex -> Bool -> IO ()
usrpSetRxAGC (USRP fp) ci en =
    withForeignPtr fp $ \p ->
    throwOnUHDError $ uhd_usrp_set_rx_agc p (fromBool en) (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_gain"
    uhd_usrp_get_rx_gain :: Ptr USRP
                         -> CSize
                         -> Ptr CChar
                         -> Ptr Double
                         -> IO UHDErrorEnum

-- | Get this Rx channel's gain stage setting in dB.
usrpGetRxGain :: USRP
              -> ChanIndex
              -> String -- ^ Name.
              -> IO Double
usrpGetRxGain (USRP fp) ci name =
    withForeignPtr fp $ \p ->
    withCString name $ \np ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_get_rx_gain p (fromIntegral ci) np dp
        peek dp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_normalized_rx_gain"
    uhd_usrp_get_normalized_rx_gain :: Ptr USRP
                                    -> CSize
                                    -> Ptr Double
                                    -> IO UHDErrorEnum

-- | Get this Rx channel's normalized gain (in the range [0 .. 1]) for the
--   entire RF chain.
usrpGetRxGainNormalized :: USRP -> ChanIndex -> IO Double
usrpGetRxGainNormalized (USRP fp) ci =
    withForeignPtr fp $ \p ->
    alloca $ \dp -> do
        throwOnUHDError $
            uhd_usrp_get_normalized_rx_gain p (fromIntegral ci) dp
        peek dp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_gain_range"
    uhd_usrp_get_rx_gain_range :: Ptr USRP
                               -> Ptr CChar
                               -> CSize
                               -> Ptr MetaRange
                               -> IO UHDErrorEnum

usrpGetRxGainRange :: USRP -> ChanIndex -> String -> MetaRange -> IO ()
usrpGetRxGainRange (USRP fp) ci name (MetaRange mfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr mfp $ \mp ->
    withCString name $ \np ->
    throwOnUHDError $ uhd_usrp_get_rx_gain_range p np (fromIntegral ci) mp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_gain_names"
    uhd_usrp_get_rx_gain_names :: Ptr USRP
                               -> CSize
                               -> Ptr (Ptr StringVector)
                               -> IO UHDErrorEnum

usrpGetRxGainStageNamesSV :: USRP -> ChanIndex -> StringVector -> IO ()
usrpGetRxGainStageNamesSV (USRP fp) ci (StringVector sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    alloca $ \spp -> do
        poke spp sp
        throwOnUHDError $ uhd_usrp_get_rx_gain_names p (fromIntegral ci) spp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_rx_antenna"
    uhd_usrp_set_rx_antenna :: Ptr USRP
                            -> Ptr CChar
                            -> CSize
                            -> IO UHDErrorEnum

-- | Set this Rx channel's antenna.
usrpSetRxAntenna :: USRP
                 -> ChanIndex
                 -> String -- ^ Antenna name.
                 -> IO ()
usrpSetRxAntenna (USRP fp) ci name =
    withForeignPtr fp $ \p ->
    withCString name $ \np ->
    throwOnUHDError $ uhd_usrp_set_rx_antenna p np (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_antenna"
    uhd_usrp_get_rx_antenna :: Ptr USRP
                            -> CSize
                            -> Ptr CChar
                            -> CSize
                            -> IO UHDErrorEnum

-- | Get this Rx channel's antenna.
usrpGetRxAntenna :: USRP -> ChanIndex -> IO String
usrpGetRxAntenna (USRP fp) ci =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_usrp_get_rx_antenna p (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_antennas"
    uhd_usrp_get_rx_antennas :: Ptr USRP
                             -> CSize
                             -> Ptr (Ptr StringVector)
                             -> IO UHDErrorEnum

usrpGetRxAntennasSV :: USRP -> ChanIndex -> StringVector -> IO ()
usrpGetRxAntennasSV (USRP fp) ci (StringVector sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    alloca $ \spp -> do
        poke spp sp
        throwOnUHDError $ uhd_usrp_get_rx_antennas p (fromIntegral ci) spp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_sensor_names"
    uhd_usrp_get_rx_sensor_names :: Ptr USRP
                                 -> CSize
                                 -> Ptr (Ptr StringVector)
                                 -> IO UHDErrorEnum

usrpGetRxSensorNamesSV :: USRP -> ChanIndex -> StringVector -> IO ()
usrpGetRxSensorNamesSV (USRP fp) ci (StringVector sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    alloca $ \spp -> do
        poke spp sp
        throwOnUHDError $ uhd_usrp_get_rx_sensor_names p (fromIntegral ci) spp


foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_rx_bandwidth"
    uhd_usrp_set_rx_bandwidth :: Ptr USRP
                              -> Double
                              -> CSize
                              -> IO UHDErrorEnum

-- | Set this Rx channel's bandwidth in Hz.
usrpSetRxBandwidth :: USRP
                   -> ChanIndex
                   -> Double -- ^ Hz.
                   -> IO ()
usrpSetRxBandwidth (USRP fp) ci bw =
    withForeignPtr fp $ \p ->
    throwOnUHDError $ uhd_usrp_set_rx_bandwidth p bw (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_bandwidth"
    uhd_usrp_get_rx_bandwidth :: Ptr USRP
                              -> CSize
                              -> Ptr Double
                              -> IO UHDErrorEnum

-- | Get this Rx channel's bandwidth in Hz.
usrpGetRxBandwidth :: USRP -> ChanIndex -> IO Double
usrpGetRxBandwidth (USRP fp) ci =
    withForeignPtr fp $ \p ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_get_rx_bandwidth p (fromIntegral ci) dp
        peek dp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_bandwidth_range"
    uhd_usrp_get_rx_bandwidth_range :: Ptr USRP
                                    -> CSize
                                    -> Ptr MetaRange
                                    -> IO UHDErrorEnum

usrpGetRxBandwidthRange :: USRP -> ChanIndex -> MetaRange -> IO ()
usrpGetRxBandwidthRange (USRP fp) ci (MetaRange mfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr mfp $ \mp ->
    throwOnUHDError $ uhd_usrp_get_rx_bandwidth_range p (fromIntegral ci) mp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_rx_sensor"
    uhd_usrp_get_rx_sensor :: Ptr USRP
                           -> Ptr CChar
                           -> CSize
                           -> Ptr (Ptr SensorValue)
                           -> IO UHDErrorEnum

usrpGetRxSensorValue :: USRP -> ChanIndex -> String -> SensorValue -> IO ()
usrpGetRxSensorValue (USRP fp) ci name (SensorValue sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    withCString name $ \np ->
    alloca $ \spp -> do
        poke spp sp
        throwOnUHDError $ uhd_usrp_get_rx_sensor p np (fromIntegral ci) spp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_rx_dc_offset_enabled"
    uhd_usrp_set_rx_dc_offset_enabled :: Ptr USRP
                                      -> CBool
                                      -> CSize
                                      -> IO UHDErrorEnum

-- | Enable/disable DC offset correction for this Rx channel.
usrpSetRxDCOffsetEnable :: USRP -> ChanIndex -> Bool -> IO ()
usrpSetRxDCOffsetEnable (USRP fp) ci en =
    withForeignPtr fp $ \p ->
    throwOnUHDError $
        uhd_usrp_set_rx_dc_offset_enabled p (fromBool en) (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_rx_iq_balance_enabled"
    uhd_usrp_set_rx_iq_balance_enabled :: Ptr USRP
                                       -> CBool
                                       -> CSize
                                       -> IO UHDErrorEnum

-- | Enable/disable IQ balance correction for this Rx channel.
usrpSetRxIQBalanceEnable :: USRP -> ChanIndex -> Bool -> IO ()
usrpSetRxIQBalanceEnable (USRP fp) ci en =
    withForeignPtr fp $ \p ->
    throwOnUHDError $
        uhd_usrp_set_rx_iq_balance_enabled p (fromBool en) (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_tx_subdev_spec"
    uhd_usrp_set_tx_subdev_spec :: Ptr USRP
                                -> Ptr SubdevSpec
                                -> CSize
                                -> IO UHDErrorEnum

usrpSetTxSubdevSpec :: USRP -> MotherboardIndex -> SubdevSpec -> IO ()
usrpSetTxSubdevSpec (USRP fp) mi (SubdevSpec sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    throwOnUHDError $ uhd_usrp_set_tx_subdev_spec p sp (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_subdev_spec"
    uhd_usrp_get_tx_subdev_spec :: Ptr USRP
                                -> CSize
                                -> Ptr SubdevSpec
                                -> IO UHDErrorEnum

usrpGetTxSubdevSpec :: USRP -> MotherboardIndex -> SubdevSpec -> IO ()
usrpGetTxSubdevSpec (USRP fp) mi (SubdevSpec sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    throwOnUHDError $ uhd_usrp_get_tx_subdev_spec p (fromIntegral mi) sp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_num_channels"
    uhd_usrp_get_tx_num_channels :: Ptr USRP
                                 -> Ptr CSize
                                 -> IO UHDErrorEnum

usrpGetTxNumChannels :: USRP -> IO Int
usrpGetTxNumChannels (USRP fp) =
    withForeignPtr fp $ \p ->
    alloca $ \sp -> do
        throwOnUHDError $ uhd_usrp_get_tx_num_channels p sp
        fromIntegral <$> peek sp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_subdev_name"
    uhd_usrp_get_tx_subdev_name :: Ptr USRP
                                -> CSize
                                -> Ptr CChar
                                -> CSize
                                -> IO UHDErrorEnum

usrpGetTxSubdevName :: USRP -> ChanIndex -> IO String
usrpGetTxSubdevName (USRP fp) ci =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_usrp_get_tx_subdev_name p (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_tx_rate"
    uhd_usrp_set_tx_rate :: Ptr USRP -> Double -> CSize -> IO UHDErrorEnum

usrpSetTxRate :: USRP -> ChanIndex -> Double -> IO ()
usrpSetTxRate (USRP fp) ci r =
    withForeignPtr fp $ \p ->
    throwOnUHDError $ uhd_usrp_set_tx_rate p r (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_rate"
    uhd_usrp_get_tx_rate :: Ptr USRP -> CSize -> Ptr Double -> IO UHDErrorEnum

usrpGetTxRate :: USRP -> ChanIndex -> IO Double
usrpGetTxRate (USRP fp) ci =
    withForeignPtr fp $ \p ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_get_tx_rate p (fromIntegral ci) dp
        peek dp


foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_rates"
    uhd_usrp_get_tx_rates :: Ptr USRP
                          -> CSize
                          -> Ptr MetaRange
                          -> IO UHDErrorEnum

usrpGetTxRates :: USRP -> ChanIndex -> MetaRange -> IO ()
usrpGetTxRates (USRP fp) ci (MetaRange mfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr mfp $ \mp ->
    throwOnUHDError $ uhd_usrp_get_tx_rates p (fromIntegral ci) mp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_tx_freq"
    uhd_usrp_set_tx_freq :: Ptr USRP
                         -> Ptr TuneReq
                         -> CSize
                         -> Ptr TuneRes
                         -> IO UHDErrorEnum

usrpSetTxFreq :: USRP
              -> ChanIndex
              -> TuneReq
              -> IO TuneRes
usrpSetTxFreq (USRP fp) ci tq =
    withForeignPtr fp $ \p ->
    withTuneReq tq $ \tqp ->
    alloca $ \tsp -> do
        throwOnUHDError $ uhd_usrp_set_tx_freq p tqp (fromIntegral ci) tsp
        peek tsp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_freq"
    uhd_usrp_get_tx_freq :: Ptr USRP
                         -> CSize
                         -> Ptr Double
                         -> IO UHDErrorEnum

usrpGetTxFreq :: USRP -> ChanIndex -> IO Double
usrpGetTxFreq (USRP fp) ci =
    withForeignPtr fp $ \p ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_get_tx_freq p (fromIntegral ci) dp
        peek dp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_freq_range"
    uhd_usrp_get_tx_freq_range :: Ptr USRP
                               -> CSize
                               -> Ptr MetaRange
                               -> IO UHDErrorEnum

usrpGetTxFreqRange :: USRP -> ChanIndex -> MetaRange -> IO ()
usrpGetTxFreqRange (USRP fp) ci (MetaRange mfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr mfp $ \mp ->
    throwOnUHDError $ uhd_usrp_get_tx_freq_range p (fromIntegral ci) mp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_fe_tx_freq_range"
    uhd_usrp_get_fe_tx_freq_range :: Ptr USRP
                                  -> CSize
                                  -> Ptr MetaRange
                                  -> IO UHDErrorEnum

usrpGetFrontEndTxFreqRange :: USRP -> ChanIndex -> MetaRange -> IO ()
usrpGetFrontEndTxFreqRange (USRP fp) ci (MetaRange mfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr mfp $ \mp ->
    throwOnUHDError $ uhd_usrp_get_fe_tx_freq_range p (fromIntegral ci) mp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_lo_names"
    uhd_usrp_get_tx_lo_names :: Ptr USRP
                             -> CSize
                             -> Ptr (Ptr StringVector)
                             -> IO UHDErrorEnum

usrpGetTxLONamesSV :: USRP -> ChanIndex -> StringVector -> IO ()
usrpGetTxLONamesSV (USRP fp) ci (StringVector sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    alloca $ \spp -> do
        poke spp sp
        throwOnUHDError $ uhd_usrp_get_tx_lo_names p (fromIntegral ci) spp

usrpGetTxLONames :: USRP -> ChanIndex -> IO [String]
usrpGetTxLONames u ci = fromStringVectorF $ usrpGetTxLONamesSV u ci

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_tx_lo_source"
    uhd_usrp_set_tx_lo_source :: Ptr USRP
                              -> Ptr CChar
                              -> Ptr CChar
                              -> CSize
                              -> IO UHDErrorEnum

usrpSetTxLOSource :: USRP -> ChanIndex -> String -> String -> IO ()
usrpSetTxLOSource (USRP fp) ci name src =
    withForeignPtr fp $ \p ->
    withCString src $ \sp ->
    withCString name $ \np ->
    throwOnUHDError $ uhd_usrp_set_tx_lo_source p sp np (fromIntegral ci)

foreign import capi "uhd/usrp/usrp.h uhd_usrp_get_tx_lo_source"
    uhd_usrp_get_tx_lo_source :: Ptr USRP
                              -> Ptr CChar
                              -> CSize
                              -> Ptr CChar
                              -> CSize
                              -> IO UHDErrorEnum

usrpGetTxLOSource :: USRP -> ChanIndex -> String -> IO String
usrpGetTxLOSource (USRP fp) ci name =
    withForeignPtr fp $ \p ->
    withCString name $ \np ->
    getUHDString 2048 $ uhd_usrp_get_tx_lo_source p np (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_lo_sources"
    uhd_usrp_get_tx_lo_sources :: Ptr USRP
                               -> Ptr CChar
                               -> CSize
                               -> Ptr (Ptr StringVector)
                               -> IO UHDErrorEnum

usrpGetTxLOSourcesSV :: USRP -> ChanIndex -> String -> StringVector -> IO ()
usrpGetTxLOSourcesSV (USRP fp) ci name (StringVector sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    withCString name $ \np -> 
    alloca $ \spp -> do
        poke spp sp
        throwOnUHDError $ uhd_usrp_get_tx_lo_sources p np (fromIntegral ci) spp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_tx_lo_export_enabled"
    uhd_usrp_set_tx_lo_export_enabled :: Ptr USRP
                                      -> CBool
                                      -> Ptr CChar
                                      -> CSize
                                      -> IO UHDErrorEnum

usrpSetTxLOExport :: USRP -> ChanIndex -> String -> Bool -> IO ()
usrpSetTxLOExport (USRP fp) ci name en =
    withForeignPtr fp $ \p -> 
    withCString name $ \np ->
    throwOnUHDError $
        uhd_usrp_set_tx_lo_export_enabled p (fromBool en) np (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_lo_export_enabled"
    uhd_usrp_get_tx_lo_export_enabled :: Ptr USRP
                                      -> Ptr CChar
                                      -> CSize
                                      -> Ptr CBool
                                      -> IO UHDErrorEnum

usrpGetTxLOExport :: USRP -> ChanIndex -> String -> IO Bool
usrpGetTxLOExport (USRP fp) ci name =
    withForeignPtr fp $ \p ->
    withCString name $ \np ->
    alloca $ \bp -> do
        throwOnUHDError $
            uhd_usrp_get_tx_lo_export_enabled p np (fromIntegral ci) bp
        toBool <$> peek bp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_tx_lo_freq"
    uhd_usrp_set_tx_lo_freq :: Ptr USRP
                            -> Double
                            -> Ptr CChar
                            -> CSize
                            -> Ptr Double
                            -> IO UHDErrorEnum

usrpSetTxLOFreq :: USRP -> ChanIndex -> String -> Double -> IO Double
usrpSetTxLOFreq (USRP fp) ci name f =
    withForeignPtr fp $ \p ->
    withCString name $ \np ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_set_tx_lo_freq p f np (fromIntegral ci) dp
        peek dp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_lo_freq"
    uhd_usrp_get_tx_lo_freq :: Ptr USRP
                            -> Ptr CChar
                            -> CSize
                            -> Ptr Double
                            -> IO UHDErrorEnum

usrpGetTxLOFreq :: USRP -> ChanIndex -> String -> IO Double
usrpGetTxLOFreq (USRP fp) ci name =
    withForeignPtr fp $ \p ->
    withCString name $ \np ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_get_tx_lo_freq p np (fromIntegral ci) dp
        peek dp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_tx_gain"
    uhd_usrp_set_tx_gain :: Ptr USRP
                         -> Double
                         -> CSize
                         -> Ptr CChar
                         -> IO UHDErrorEnum

usrpSetTxGain :: USRP
              -> ChanIndex
              -> String -- ^ Name.
              -> Double -- ^ Gain in dB.
              -> IO ()
usrpSetTxGain (USRP fp) ci name g =
    withForeignPtr fp $ \p ->
    withCString name $ \np ->
    throwOnUHDError $ uhd_usrp_set_tx_gain p g (fromIntegral ci) np


foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_gain"
    uhd_usrp_get_tx_gain :: Ptr USRP
                         -> CSize
                         -> Ptr CChar
                         -> Ptr Double
                         -> IO UHDErrorEnum

-- | Get this Tx channel's gain stage setting in dB.
usrpGetTxGain :: USRP
              -> ChanIndex
              -> String -- ^ Name.
              -> IO Double
usrpGetTxGain (USRP fp) ci name =
    withForeignPtr fp $ \p ->
    withCString name $ \np ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_get_tx_gain p (fromIntegral ci) np dp
        peek dp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_normalized_tx_gain"
    uhd_usrp_get_normalized_tx_gain :: Ptr USRP
                                    -> CSize
                                    -> Ptr Double
                                    -> IO UHDErrorEnum

-- | Get this Tx channel's normalized gain (in the range [0 .. 1]) for the
--   entire RF chain.
usrpGetTxGainNormalized :: USRP -> ChanIndex -> IO Double
usrpGetTxGainNormalized (USRP fp) ci =
    withForeignPtr fp $ \p ->
    alloca $ \dp -> do
        throwOnUHDError $
            uhd_usrp_get_normalized_tx_gain p (fromIntegral ci) dp
        peek dp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_gain_range"
    uhd_usrp_get_tx_gain_range :: Ptr USRP
                               -> Ptr CChar
                               -> CSize
                               -> Ptr MetaRange
                               -> IO UHDErrorEnum

usrpGetTxGainRange :: USRP -> ChanIndex -> String -> MetaRange -> IO ()
usrpGetTxGainRange (USRP fp) ci name (MetaRange mfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr mfp $ \mp ->
    withCString name $ \np ->
    throwOnUHDError $ uhd_usrp_get_tx_gain_range p np (fromIntegral ci) mp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_normalized_tx_gain"
    uhd_usrp_set_normalized_tx_gain :: Ptr USRP
                                    -> Double
                                    -> CSize
                                    -> IO UHDErrorEnum

-- | Set this Tx channel's normalized gain (in the range [0 .. 1]) for the
--   entire RF chain.
usrpSetTxGainNormalized :: USRP -> ChanIndex -> Double -> IO ()
usrpSetTxGainNormalized (USRP fp) ci ng =
    withForeignPtr fp $ \p ->
    throwOnUHDError $
        uhd_usrp_set_normalized_tx_gain p ng (fromIntegral ci)
        
foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_gain_names"
    uhd_usrp_get_tx_gain_names :: Ptr USRP
                               -> CSize
                               -> Ptr (Ptr StringVector)
                               -> IO UHDErrorEnum

usrpGetTxGainStageNamesSV :: USRP -> ChanIndex -> StringVector -> IO ()
usrpGetTxGainStageNamesSV (USRP fp) ci (StringVector sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    alloca $ \spp -> do
        poke spp sp
        throwOnUHDError $ uhd_usrp_get_tx_gain_names p (fromIntegral ci) spp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_tx_antenna"
    uhd_usrp_set_tx_antenna :: Ptr USRP
                            -> Ptr CChar
                            -> CSize
                            -> IO UHDErrorEnum

usrpSetTxAntenna :: USRP
                 -> ChanIndex
                 -> String -- ^ Antenna name.
                 -> IO ()
usrpSetTxAntenna (USRP fp) ci name =
    withForeignPtr fp $ \p ->
    withCString name $ \np ->
    throwOnUHDError $ uhd_usrp_set_tx_antenna p np (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_antenna"
    uhd_usrp_get_tx_antenna :: Ptr USRP
                            -> CSize
                            -> Ptr CChar
                            -> CSize
                            -> IO UHDErrorEnum

usrpGetTxAntenna :: USRP -> ChanIndex -> IO String
usrpGetTxAntenna (USRP fp) ci =
    withForeignPtr fp $ \p ->
    getUHDString 2048 $ uhd_usrp_get_tx_antenna p (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_antennas"
    uhd_usrp_get_tx_antennas :: Ptr USRP
                             -> CSize
                             -> Ptr (Ptr StringVector)
                             -> IO UHDErrorEnum

usrpGetTxAntennasSV :: USRP -> ChanIndex -> StringVector -> IO ()
usrpGetTxAntennasSV (USRP fp) ci (StringVector sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    alloca $ \spp -> do
        poke spp sp
        throwOnUHDError $ uhd_usrp_get_tx_antennas p (fromIntegral ci) spp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_sensor_names"
    uhd_usrp_get_tx_sensor_names :: Ptr USRP
                                 -> CSize
                                 -> Ptr (Ptr StringVector)
                                 -> IO UHDErrorEnum

usrpGetTxSensorNamesSV :: USRP -> ChanIndex -> StringVector -> IO ()
usrpGetTxSensorNamesSV (USRP fp) ci (StringVector sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    alloca $ \spp -> do
        poke spp sp
        throwOnUHDError $ uhd_usrp_get_tx_sensor_names p (fromIntegral ci) spp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_tx_bandwidth"
    uhd_usrp_set_tx_bandwidth :: Ptr USRP
                              -> Double
                              -> CSize
                              -> IO UHDErrorEnum

usrpSetTxBandwidth :: USRP
                   -> ChanIndex
                   -> Double -- ^ Hz.
                   -> IO ()
usrpSetTxBandwidth (USRP fp) ci bw =
    withForeignPtr fp $ \p ->
    throwOnUHDError $ uhd_usrp_set_tx_bandwidth p bw (fromIntegral ci)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_bandwidth"
    uhd_usrp_get_tx_bandwidth :: Ptr USRP
                              -> CSize
                              -> Ptr Double
                              -> IO UHDErrorEnum

usrpGetTxBandwidth :: USRP -> ChanIndex -> IO Double
usrpGetTxBandwidth (USRP fp) ci =
    withForeignPtr fp $ \p ->
    alloca $ \dp -> do
        throwOnUHDError $ uhd_usrp_get_tx_bandwidth p (fromIntegral ci) dp
        peek dp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_bandwidth_range"
    uhd_usrp_get_tx_bandwidth_range :: Ptr USRP
                                    -> CSize
                                    -> Ptr MetaRange
                                    -> IO UHDErrorEnum

usrpGetTxBandwidthRange :: USRP -> ChanIndex -> MetaRange -> IO ()
usrpGetTxBandwidthRange (USRP fp) ci (MetaRange mfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr mfp $ \mp ->
    throwOnUHDError $ uhd_usrp_get_tx_bandwidth_range p (fromIntegral ci) mp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_tx_sensor"
    uhd_usrp_get_tx_sensor :: Ptr USRP
                           -> Ptr CChar
                           -> CSize
                           -> Ptr (Ptr SensorValue)
                           -> IO UHDErrorEnum

usrpGetTxSensorValue :: USRP -> ChanIndex -> String -> SensorValue -> IO ()
usrpGetTxSensorValue (USRP fp) ci name (SensorValue sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    withCString name $ \np ->
    alloca $ \spp -> do
        poke spp sp
        throwOnUHDError $ uhd_usrp_get_tx_sensor p np (fromIntegral ci) spp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_gpio_banks"
    uhd_usrp_get_gpio_banks :: Ptr USRP
                            -> CSize
                            -> Ptr (Ptr StringVector)
                            -> IO UHDErrorEnum

usrpGetGPIOBanksSV :: USRP -> MotherboardIndex -> StringVector -> IO ()
usrpGetGPIOBanksSV (USRP fp) mi (StringVector sfp) =
    withForeignPtr fp $ \p ->
    withForeignPtr sfp $ \sp ->
    alloca $ \spp -> do
        poke spp sp
        throwOnUHDError $ uhd_usrp_get_gpio_banks p (fromIntegral mi) spp

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_set_gpio_attr"
    uhd_usrp_set_gpio_attr :: Ptr USRP
                           -> Ptr CChar
                           -> Ptr CChar
                           -> Word32
                           -> Word32
                           -> CSize
                           -> IO UHDErrorEnum

-- | Set GPIO attributes.
usrpSetGPIOAttr :: USRP
                -> MotherboardIndex
                -> String -- ^ Bank name.
                -> String -- ^ Attr name.
                -> Word32 -- ^ Attr value.
                -> Word32 -- ^ Mask.
                -> IO ()
usrpSetGPIOAttr (USRP fp) mi bank attr val mask =
    withForeignPtr fp $ \p ->
    withCString bank $ \bp ->
    withCString attr $ \ap ->
    throwOnUHDError $ uhd_usrp_set_gpio_attr p bp ap val mask (fromIntegral mi)

foreign import capi safe "uhd/usrp/usrp.h uhd_usrp_get_gpio_attr"
    uhd_usrp_get_gpio_attr :: Ptr USRP
                           -> Ptr CChar
                           -> Ptr CChar
                           -> CSize
                           -> Ptr Word32
                           -> IO UHDErrorEnum

-- | Get GPIO attributes.
usrpGetGPIOAttr :: USRP
                -> MotherboardIndex
                -> String -- ^ Bank name.
                -> String -- ^ Attr name.
                -> IO Word32
usrpGetGPIOAttr (USRP fp) mi bank attr =
    withForeignPtr fp $ \p ->
    withCString bank $ \bp ->
    withCString attr $ \ap ->
    alloca $ \wp -> do
        throwOnUHDError $ uhd_usrp_get_gpio_attr p bp ap (fromIntegral mi) wp
        peek wp
