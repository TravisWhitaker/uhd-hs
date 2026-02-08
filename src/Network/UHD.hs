{-|
Module      : Network.UHD
Copyright   : Travis Whitaker 2026
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable (Windows, POSIX)

This module provides a Haskell interface to the USRP hardware driver.
Discovering and configuring devices, receive, and transmit are all supported.

The API presented here is somewhat Haskell-centric and hides some of the
underlying details for convenience. If you want to directly manipulate the
underlying UHD objects, use the "Network.UHD.Internal" module.
-}

{-# LANGUAGE DeriveGeneric
           , DerivingStrategies
           , LambdaCase
           , RecordWildCards
           #-}

module Network.UHD (
    -- * UHD
    I.getLastErrorString
  , I.uhdABIString
  , I.uhdVersionString
  , I.UHDError(..)
    -- * USRP Discovery and Configuration
  , I.USRP()
  , findUSRPs
  , I.makeUSRP
  , I.usrpLastError
  , I.usrpPrettyPrint
  , I.MotherboardIndex
  , I.usrpGetNumMotherboards
  , I.usrpGetMotherboardName
  , I.usrpSetUserReg
    -- ** Time
    -- | Note that as far as UHD is concerned, "time" refers to an absolute time
    --   reference while "clock" refers to a frequeny reference.
  , I.TimeSpec
  , I.toTimeSpec
  , I.fromTimeSpec
  , usrpGetTimeSources
  , I.usrpGetTimeSource
  , I.usrpSetTimeSource
  , I.usrpSetTimeSourceOutput
  , I.usrpGetTimeNow
  , I.usrpGetLastPPS
  , I.usrpGetTimeSync
  , usrpSetPPSTimeFromSystemTime
  , usrpSetUnknownPPSTimeFromSystemTime
  , usrpSetTimeNow
  , I.usrpSetCommandTime
  , I.usrpClearCommandTime
  -- ** Clocks
  -- | Note that as far as UHD is concerned, "time" refers to an absolute time
  --   reference while "clock" refers to a frequeny reference.
  , usrpGetClockSources
  , I.usrpGetClockSource
  , I.usrpSetClockSource
  , I.usrpSetClockSourceOutput
  , I.usrpGetMasterClockRate
  , I.usrpSetMasterClockRate
  -- ** Sensors
  , I.Range(..)
  , I.SensorValueDataType(..)
  , I.SensorValueData(..)
  , SensorValue(..)
  , usrpGetMotherboardSensorNames
  , usrpGetMotherboardSensor
  -- ** EEPROMs
  , I.MotherboardEEPROM()
  , usrpGetMotherboardEEPROM
  , I.usrpSetMotherboardEEPROM
  , I.motherboardEEPROMGetValue
  , I.motherboardEEPROMSetValue
  , I.DaughterboardEEPROM()
  , usrpGetDaughterboardEEPROM
  , I.usrpSetDaughterboardEEPROM
  , I.daughterboardEEPROMGetId
  , I.daughterboardEEPROMSetId
  , I.daughterboardEEPROMGetSerial
  , I.daughterboardEEPROMSetSerial
  , I.daughterboardEEPROMGetRevision
  , I.daughterboardEEPROMSetRevision
  -- ** Subdevices
  , I.SubdevSpecPair(..)
  , I.SubdevSpec()
  , I.fromSubdevSpec
  , I.makeSubdevSpec
  , I.subdevSpecMarkupString
  -- ** Streams
  , I.SampleType(..)
  , I.SomeSampleType(..)
  , I.WireFormat(..)
  , I.StreamArgs(..)
  , I.StreamMode(..)
  , I.StreamTime(..)
  , I.StreamCmd(..)
  -- ** Tuning
  , I.TuneReqPolicy(..)
  , I.TuneReq(..)
  , I.TuneRes(..)
  -- ** RX
  , I.RxInfo(..)
  , I.usrpRxInfo
  , usrpGetRxSubdevSpec
  , I.usrpSetRxSubdevSpec
  , I.usrpGetRxNumChannels
  , I.usrpGetRxSubdevName
  , usrpGetRxRates
  , I.usrpGetRxRate
  , I.usrpSetRxRate
  , usrpGetRxFreqRange
  , usrpGetFrontEndRxFreqRange
  , I.usrpGetRxFreq
  , I.usrpSetRxFreq
  , usrpGetRxLONames
  , usrpGetRxLOSources
  , I.usrpGetRxLOSource
  , I.usrpSetRxLOSource
  , I.usrpGetRxLOExport
  , I.usrpSetRxLOExport
  , I.usrpGetRxLOFreq
  , I.usrpSetRxLOFreq
  , usrpGetRxGainStageNames
  , usrpGetRxGainRange
  , I.usrpGetRxGain
  , I.usrpGetRxGainNormalized
  , I.usrpSetRxGain
  , I.usrpSetRxGainNormalized
  , I.usrpSetRxAGC
  , usrpGetRxAntennas
  , I.usrpGetRxAntenna
  , I.usrpSetRxAntenna
  , usrpGetRxBandwidthRange
  , I.usrpGetRxBandwidth
  , I.usrpSetRxBandwidth
  , usrpGetRxSensorNames
  , usrpGetRxSensorValue
  , I.usrpSetRxDCOffsetEnable
  , I.usrpSetRxIQBalanceEnable
  , RxMetadata(..)
  , I.RxMetadataError(..)
  , I.RxStreamer()
  , I.usrpGetRxStream
  , I.rxStreamerNumChannels
  , I.rxStreamerMaxNumSamps
  , I.rxStreamerIssueCmd
  , rxStreamerRecv
  , I.rxStreamerLastError
  -- ** TX
  , I.TxInfo(..)
  , I.usrpTxInfo
  , usrpGetTxSubdevSpec
  , I.usrpSetTxSubdevSpec
  , I.usrpGetTxNumChannels
  , I.usrpGetTxSubdevName
  , usrpGetTxRates
  , I.usrpGetTxRate
  , I.usrpSetTxRate
  , usrpGetTxFreqRange
  , usrpGetFrontEndTxFreqRange
  , I.usrpGetTxFreq
  , I.usrpSetTxFreq
  , usrpGetTxLONames
  , usrpGetTxLOSources
  , I.usrpGetTxLOSource
  , I.usrpSetTxLOSource
  , I.usrpGetTxLOExport
  , I.usrpSetTxLOExport
  , I.usrpGetTxLOFreq
  , I.usrpSetTxLOFreq
  , usrpGetTxGainStageNames
  , usrpGetTxGainRange
  , I.usrpGetTxGain
  , I.usrpGetTxGainNormalized
  , I.usrpSetTxGain
  , I.usrpSetTxGainNormalized
  , usrpGetTxAntennas
  , I.usrpGetTxAntenna
  , I.usrpSetTxAntenna
  , usrpGetTxBandwidthRange
  , I.usrpGetTxBandwidth
  , I.usrpSetTxBandwidth
  , usrpGetTxSensorNames
  , usrpGetTxSensorValue
  , TxMetadata(..)
  , I.TxStreamer()
  , I.usrpGetTxStream
  , I.txStreamerNumChannels
  , I.txStreamerMaxNumSamps
  , txStreamerSend
  , I.AsyncMetadataEvent(..)
  , AsyncMetadata(..)
  , txStreamerRecvAsyncMsg
  , I.txStreamerLastError
  -- ** GPIO
  , usrpGetGPIOBanks
  , I.usrpGetGPIOAttr
  , I.usrpSetGPIOAttr
  ) where
  
import Control.Concurrent

import Data.Maybe

import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS

import Data.Time.Clock.POSIX

import Data.Traversable

import Data.Word

import Foreign.Concurrent
-- import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable

import GHC.Generics (Generic)

import qualified Network.UHD.Internal as I

-- | Extra metadata reported synchronously by an active RX chain while
--   streaming.
data RxMetadata = RxMetadata {
    rmTime :: Maybe I.TimeSpec
  , rmMoreFragments :: Bool
  , rmFragmentOffset :: Word64
  , rmBurstStart :: Bool
  , rmBurstEnd :: Bool
  , rmOutOfSequence :: Bool
  , rmError :: Maybe I.RxMetadataError
  } deriving stock ( Eq
                   , Ord
                   , Read
                   , Show
                   , Generic
                   )

rxMetadataContents :: I.RxMetadata -> IO RxMetadata
rxMetadataContents rm = do
    hasTime <- I.rxMetadataHasTimeSpec rm
    rmTime <- if hasTime
              then Just <$> I.rxMetadataTimeSpec rm
              else pure Nothing
    rmMoreFragments <- I.rxMetadataMoreFragments rm
    rmFragmentOffset <- I.rxMetadataFragmentOffset rm
    rmBurstStart <- I.rxMetadataBurstStart rm
    rmBurstEnd <- I.rxMetadataBurstEnd rm
    rmOutOfSequence <- I.rxMetadataOutOfSequence rm
    rmError <- I.rxMetadataError rm
    pure RxMetadata{..}

-- | Extra metadata that a sender must provide synchronously to an active TX
--   chain.
data TxMetadata = TxMetadata {
    txmTimeSpec :: Maybe I.TimeSpec
  , txmStartOfBurst :: Bool
  , txmEndOfBurst :: Bool
  } deriving stock ( Eq
                   , Ord
                   , Read
                   , Show
                   , Generic
                   )

makeTxMetadata :: TxMetadata -> IO I.TxMetadata
makeTxMetadata TxMetadata{..} = alloca $ \p -> do
    let has_time_spec = isJust txmTimeSpec
        full_secs = maybe 0 I.fullSecs txmTimeSpec
        frac_secs = maybe 0 I.fracSecs txmTimeSpec
    I.throwOnUHDError $ I.uhd_tx_metadata_make p
                                           (fromBool has_time_spec)
                                           full_secs
                                           frac_secs
                                           (fromBool txmStartOfBurst)
                                           (fromBool txmEndOfBurst)
    mp <- peek p
    I.TxMetadata <$> newForeignPtr mp (I.mkFreeByRef I.uhd_tx_metadata_free mp)

-- | Metadata reported asynchronously by an active TX chain.
data AsyncMetadata = AsyncMetadata {
    amChanIndex :: I.ChanIndex
  , amTime :: Maybe I.TimeSpec
  , amEvent :: I.AsyncMetadataEvent
  , amUserPayload :: VS.Vector Word32
  } deriving stock ( Eq
                   , Ord
                   , Read
                   , Show
                   , Generic
                   )

asyncMetadataContents :: I.AsyncMetadata -> IO AsyncMetadata
asyncMetadataContents am = do
    amChanIndex <- I.asyncMetadataChanIndex am
    hasTime <- I.asyncMetadataHasTimeSpec am
    amTime <- if hasTime
              then Just <$> I.asyncMetadataTimeSpec am
              else pure Nothing
    amEvent <- I.asyncMetadataEvent am
    amUserPayload <- I.asyncMetadataUserPayload am
    pure AsyncMetadata{..}

-- | The data reported by a USRP's sensor.
data SensorValue = SensorValue {
    svName :: String
  , svUnit :: String
  , svData :: Maybe I.SensorValueData
  } deriving  ( Eq
              , Ord
              , Read
              , Show
              , Generic
              )

sensorValueContents :: I.SensorValue -> IO SensorValue
sensorValueContents sv = do
    svName <- I.sensorValueName sv
    svUnit <- I.sensorValueUnit sv
    mdt <- I.sensorValueDataType sv
    svData <- for mdt (\case
        { I.SensorValueBool -> I.SensorValueDataBool <$> I.sensorValueBool sv
        ; I.SensorValueInt -> I.SensorValueDataInt <$> I.sensorValueInt sv
        ; I.SensorValueReal -> I.SensorValueDataReal <$> I.sensorValueDouble sv
        ; I.SensorValueString -> I.SensorValueDataString <$> I.sensorValueValue sv
        })
    pure SensorValue{..}

withSensorValue :: (I.SensorValue -> IO ()) -> IO SensorValue
withSensorValue f = do
    sv <- I.makeSensorValue
    f sv
    sensorValueContents sv

-- | Receive samples from te provided streamer. The returned outer vector has
--   one inner sample vector per configured streamer channel.
rxStreamerRecv :: I.SampleType s
               => I.RxStreamer s
               -> Double -- ^ Timeout in seconds.
               -> Bool -- ^ Whether or not to handle only a single packet.
               -> IO (RxMetadata, V.Vector (VS.Vector s))
rxStreamerRecv rs tos oneP = do
    rxm <- I.makeRxMetadata
    maxN <- I.rxStreamerMaxNumSamps rs
    r <- I.rxStreamerRecv rs rxm maxN tos oneP
    rxmc <- rxMetadataContents rxm
    pure (rxmc, r)

-- | Send samples to the provided streamer. The argument outer vector should
--   have one inner sample vector per configured streamer channel. The returned
--   vector indicates how many samples per channel were actually sent to the
--   TX chain.
txStreamerSend :: I.SampleType s
               => I.TxStreamer s
               -> Double -- ^ Timeout in seconds.
               -> TxMetadata
               -> V.Vector (VS.Vector s)
               -> IO (V.Vector Int)
txStreamerSend ts tos txmc r = do
    txm <- makeTxMetadata txmc
    I.txStreamerSend ts r txm tos

-- | Receive an asynchronous message from the provided streamer. If the timeout
--   elapses and there is no message available, 'Nothing' is returned.
txStreamerRecvAsyncMsg :: I.TxStreamer s
                       -> Double
                       -> IO (Maybe AsyncMetadata)
txStreamerRecvAsyncMsg ts tos = do
    am <- I.makeAsyncMetadata
    r <- I.txStreamerRecvAsyncMsg ts am tos
    if r
    then Just <$> asyncMetadataContents am
    else pure Nothing

-- | Find all connected USRP devices.
findUSRPs :: String -- ^ Full or partial address string, used as a hint to narrow
                    --   the set of devices returned.
          -> IO [String]
findUSRPs args = I.fromStringVectorF $ I.findUSRPs args

waitForNearSecStart :: IO ()
waitForNearSecStart = do
    t <- getPOSIXTime
    let tn = toRational $ ceiling t
        to = tn - toRational t
    if to > 0.5
    then pure ()
    else threadDelay (round (to * 1000000))

-- | Set the given USRP motherboard timer to the system time when the next PPS
--   edge occurs. This can take up to 1.5 seconds in the worst case:
--
--   - In order to leave plenty of time for IO, this function will wait until
--     the start of the next POSIX second if it is called in the second half of
--     the current POSIX second.
--
--   - The hardware will wait up to one second for the PPS edge to arrive.
usrpSetPPSTimeFromSystemTime :: I.USRP -> I.MotherboardIndex -> IO ()
usrpSetPPSTimeFromSystemTime usrp mi = do
    waitForNearSecStart
    ts <- fromIntegral . ceiling <$> getPOSIXTime
    I.usrpSetTimeNextPPS usrp mi (I.toTimeSpec ts)

-- | Works like `usrpSetPPSTimeFromSystemTime', but the hardware will take up to
--   one extra second to synchronize all of the motherboards within the USRP (so
--   the worst case runtime is 2.5 seconds).
usrpSetUnknownPPSTimeFromSystemTime :: I.USRP -> IO ()
usrpSetUnknownPPSTimeFromSystemTime usrp  = do
    waitForNearSecStart
    ts <- fromIntegral . ceiling <$> getPOSIXTime
    I.usrpSetTimeUnknownPPS usrp (I.toTimeSpec ts)

-- | Set the provided USRP motherboard's timer to the current system time on a
--   best-effort basis. Timing error due to IO latency is not accounted for. If
--   you need accurate timing for each sample, connect a PPS source and use one
--   of the PPS time setting functions instead.
usrpSetTimeNow :: I.USRP -> I.MotherboardIndex -> IO ()
usrpSetTimeNow usrp mi =
    getPOSIXTime >>= I.usrpSetTimeNow usrp mi . I.toTimeSpec

-- | Get the time sources available on the provided USRP motherboard.
usrpGetTimeSources :: I.USRP -> I.MotherboardIndex -> IO [String]
usrpGetTimeSources u mi = I.fromStringVectorF $ I.usrpGetTimeSources u mi

-- | Get the clock sources available on the provided USRP motherboard.
usrpGetClockSources :: I.USRP -> I.MotherboardIndex -> IO [String]
usrpGetClockSources u mi = I.fromStringVectorF $ I.usrpGetClockSourcesSV u mi

-- | Get the given sensor's current value.
usrpGetMotherboardSensor :: I.USRP
                         -> I.MotherboardIndex
                         -> String -- ^ Sensor name.
                         -> IO SensorValue
usrpGetMotherboardSensor usrp mi name =
    withSensorValue $ I.usrpGetMotherboardSensor usrp mi name

-- | Get the sensors present on the motherboard at the given index.
usrpGetMotherboardSensorNames :: I.USRP -> I.MotherboardIndex -> IO [String]
usrpGetMotherboardSensorNames u mi =
    I.fromStringVectorF $ I.usrpGetMotherboardSensorNamesSV u mi

usrpGetMotherboardEEPROM :: I.USRP
                         -> I.MotherboardIndex
                         -> IO I.MotherboardEEPROM
usrpGetMotherboardEEPROM usrp mbi = do
    mbe <- I.makeMotherboardEEPROM
    I.usrpGetMotherboardEEPROM usrp mbi mbe
    pure mbe

usrpGetDaughterboardEEPROM :: I.USRP
                           -> I.MotherboardIndex
                           -> String -- ^ Unit name.
                           -> String -- ^ Slot name.
                           -> IO I.DaughterboardEEPROM
usrpGetDaughterboardEEPROM usrp mbi unit slot = do
    dbe <- I.makeDaughterboardEEPROM
    I.usrpGetDaughterboardEEPROM usrp mbi unit slot dbe
    pure dbe

usrpGetRxSubdevSpec :: I.USRP
                    -> I.MotherboardIndex
                    -> IO [I.SubdevSpecPair]
usrpGetRxSubdevSpec usrp mbi =
    I.fromSubdevSpecF $ I.usrpGetRxSubdevSpec usrp mbi

-- | Get the ranges of available sampling rates for the given Rx channel.
usrpGetRxRates :: I.USRP -> I.ChanIndex -> IO [I.Range]
usrpGetRxRates usrp ci = I.fromMetaRangeF $ I.usrpGetRxRates usrp ci

-- | Get the ranges of available tune frequencies for the given Rx channel.
usrpGetRxFreqRange :: I.USRP -> I.ChanIndex -> IO [I.Range]
usrpGetRxFreqRange usrp ci = I.fromMetaRangeF $ I.usrpGetRxFreqRange usrp ci

-- | Get the ranges of available frequencies for this Rx channel's front end.
usrpGetFrontEndRxFreqRange :: I.USRP -> I.ChanIndex -> IO [I.Range]
usrpGetFrontEndRxFreqRange usrp ci =
    I.fromMetaRangeF $ I.usrpGetFrontEndRxFreqRange usrp ci

-- | Get the names of the available LO slots for this Rx channel.
usrpGetRxLONames :: I.USRP -> I.ChanIndex -> IO [String]
usrpGetRxLONames u ci = I.fromStringVectorF $ I.usrpGetRxLONamesSV u ci

-- | Get the available LO sources for this named LO slot on this Rx channel.
usrpGetRxLOSources :: I.USRP
                   -> I.ChanIndex
                   -> String -- ^ Name.
                   -> IO [String]
usrpGetRxLOSources u ci n = I.fromStringVectorF $ I.usrpGetRxLOSourcesSV u ci n

-- | Get the available gain ranges for this Rx channel's named gain stage.
usrpGetRxGainRange :: I.USRP
                   -> I.ChanIndex
                   -> String -- ^ Name.
                   -> IO [I.Range]
usrpGetRxGainRange usrp ci n = I.fromMetaRangeF $ I.usrpGetRxGainRange usrp ci n

-- | Get the names of the gain stages available for this Rx channel.
usrpGetRxGainStageNames :: I.USRP -> I.ChanIndex -> IO [String]
usrpGetRxGainStageNames usrp ci =
    I.fromStringVectorF $ I.usrpGetRxGainStageNamesSV usrp ci

usrpGetRxAntennas :: I.USRP -> I.ChanIndex -> IO [String]
usrpGetRxAntennas u ci = I.fromStringVectorF $ I.usrpGetRxAntennasSV u ci

-- | Get the sensors available for this Rx channel.
usrpGetRxSensorNames :: I.USRP -> I.ChanIndex -> IO [String]
usrpGetRxSensorNames u ci = I.fromStringVectorF $ I.usrpGetRxSensorNamesSV u ci

-- | Get the available bandwidth ranges for this Rx channel.
usrpGetRxBandwidthRange :: I.USRP -> I.ChanIndex -> IO [I.Range]
usrpGetRxBandwidthRange usrp ci = I.fromMetaRangeF $ I.usrpGetRxBandwidthRange usrp ci

-- | Get this Rx channel sensor value.
usrpGetRxSensorValue :: I.USRP -> I.ChanIndex -> String -> IO SensorValue
usrpGetRxSensorValue usrp ci n =
    withSensorValue $ I.usrpGetRxSensorValue usrp ci n

usrpGetTxSubdevSpec :: I.USRP
                    -> I.MotherboardIndex
                    -> IO [I.SubdevSpecPair]
usrpGetTxSubdevSpec usrp mbi =
    I.fromSubdevSpecF $ I.usrpGetTxSubdevSpec usrp mbi

-- | Get the ranges of available sampling rates for the given Tx channel.
usrpGetTxRates :: I.USRP -> I.ChanIndex -> IO [I.Range]
usrpGetTxRates usrp ci = I.fromMetaRangeF $ I.usrpGetTxRates usrp ci

-- | Get the ranges of available tune frequencies for the given Tx channel.
usrpGetTxFreqRange :: I.USRP -> I.ChanIndex -> IO [I.Range]
usrpGetTxFreqRange usrp ci = I.fromMetaRangeF $ I.usrpGetTxFreqRange usrp ci

-- | Get the ranges of available frequencies for this Tx channel's front end.
usrpGetFrontEndTxFreqRange :: I.USRP -> I.ChanIndex -> IO [I.Range]
usrpGetFrontEndTxFreqRange usrp ci =
    I.fromMetaRangeF $ I.usrpGetFrontEndTxFreqRange usrp ci

-- | Get the names of the available LO slots for this Tx channel.
usrpGetTxLONames :: I.USRP -> I.ChanIndex -> IO [String]
usrpGetTxLONames u ci = I.fromStringVectorF $ I.usrpGetTxLONamesSV u ci

-- | Get the available LO sources for this named LO slot on this Tx channel.
usrpGetTxLOSources :: I.USRP
                   -> I.ChanIndex
                   -> String -- ^ Name.
                   -> IO [String]
usrpGetTxLOSources u ci n = I.fromStringVectorF $ I.usrpGetTxLOSourcesSV u ci n

-- | Get the available gain ranges for this Tx channel's named gain stage.
usrpGetTxGainRange :: I.USRP
                   -> I.ChanIndex
                   -> String -- ^ Name.
                   -> IO [I.Range]
usrpGetTxGainRange usrp ci n = I.fromMetaRangeF $ I.usrpGetTxGainRange usrp ci n

-- | Get the names of the gain stages available for this Tx channel.
usrpGetTxGainStageNames :: I.USRP -> I.ChanIndex -> IO [String]
usrpGetTxGainStageNames usrp ci =
    I.fromStringVectorF $ I.usrpGetTxGainStageNamesSV usrp ci

usrpGetTxAntennas :: I.USRP -> I.ChanIndex -> IO [String]
usrpGetTxAntennas u ci = I.fromStringVectorF $ I.usrpGetTxAntennasSV u ci

-- | Get the sensors available for this Tx channel.
usrpGetTxSensorNames :: I.USRP -> I.ChanIndex -> IO [String]
usrpGetTxSensorNames u ci = I.fromStringVectorF $ I.usrpGetTxSensorNamesSV u ci

-- | Get the available bandwidth ranges for this Tx channel.
usrpGetTxBandwidthRange :: I.USRP -> I.ChanIndex -> IO [I.Range]
usrpGetTxBandwidthRange usrp ci = I.fromMetaRangeF $ I.usrpGetTxBandwidthRange usrp ci

-- | Get this Tx channel sensor value.
usrpGetTxSensorValue :: I.USRP -> I.ChanIndex -> String -> IO SensorValue
usrpGetTxSensorValue usrp ci n =
    withSensorValue $ I.usrpGetTxSensorValue usrp ci n

usrpGetGPIOBanks :: I.USRP -> I.MotherboardIndex -> IO [String]
usrpGetGPIOBanks u mi = I.fromStringVectorF $ I.usrpGetGPIOBanksSV u mi
