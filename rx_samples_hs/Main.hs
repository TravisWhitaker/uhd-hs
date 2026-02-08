{-# LANGUAGE RecordWildCards
           , TypeApplications
           #-}

module Main where

import Control.Monad

import Data.Complex

import Data.Int

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import Foreign.ForeignPtr

import Options.Applicative

import System.IO

import Network.UHD

writeVS :: VS.Vector (Complex Int16) -> Handle -> Int -> IO Int
writeVS vs h left =
    let (fp, len) = VS.unsafeToForeignPtr0 vs
        n = min left len
    in withForeignPtr fp $ \p -> hPutBuf h p (n * 4) *> pure n

data Cfg = Cfg {
    devArgs :: String
  , tfHz :: Double
  , srHz :: Double
  , gain :: Double
  , nsamps :: Int
  , outfile :: FilePath
  }

cfg :: Parser Cfg
cfg = Cfg
    <$> strOption (  short 'a'
                  <> metavar "DEV_ARGS"
                  <> help "Device args"
                  <> value "type=b200,recv_frame_size=9000,num_recv_frames=1024"
                  )
    <*> option auto
        (  short 'f'
        <> metavar "TUNE_FREQ_HZ"
        <> help "Tune frequency in Hz"
        )
    <*> option auto
        (  short 'r'
        <> metavar "SAMPLE_RATE_HZ"
        <> help "Sample rate in Hz"
        )
    <*> option auto
        (  short 'g'
        <> metavar "GAIN_dB"
        <> help "Gain in dB"
        )
    <*> option auto
        (  short 'n'
        <> metavar "NUM_SAMPS"
        <> help "Number of samples to capture."
        )
    <*> strOption (  short 'o'
                  <> metavar "OUTPUT_FILE"
                  <> help "Output file path."
                  <> value "out.dat"
                  )

runCfg :: Cfg -> IO ()
runCfg Cfg{..} = do
    when (nsamps <= 0) $
        fail "Sample count must be >= 0."

    usrp <- makeUSRP devArgs
    usrpSetRxRate usrp 0 srHz
    setSR <- usrpGetRxRate usrp 0
    putStrLn $ concat [ "Set sample rate "
                      , show setSR
                      , " Hz"
                      ]

    usrpSetRxGain usrp 0 "" gain
    setGain <- usrpGetRxGain usrp 0 ""
    putStrLn $ concat [ "Set gain "
                      , show setGain
                      , " db"
                      ]

    usrpSetRxFreq usrp 0 (TuneReq tfHz TuneReqPolicyAuto TuneReqPolicyAuto "")
    setTF <- usrpGetRxFreq usrp 0
    putStrLn $ concat [ "Set tune freq "
                      , show setTF
                      , " Hz"
                      ]

    rxs <- usrpGetRxStream usrp (StreamArgs @(Complex Int16) [0] SC16 "")
    rxStreamerIssueCmd rxs (StreamCmd StartContinuous StreamNow)

    let go 0 _ = pure ()
        go n h = do
            (RxMetadata{..}, cs) <- rxStreamerRecv rxs 1.0 False
            when rmOutOfSequence $ fail "Out of sequence!"
            when (V.length cs /= 1) $ fail "Didn't get exactly one channel!"
            case rmError of
                Nothing -> writeVS (V.head cs) h n >>= (\w -> go (n - w) h)
                Just e -> fail $ show e

    withBinaryFile outfile WriteMode $ go nsamps

main :: IO ()
main = execParser opts >>= runCfg
    where opts = info (cfg <**> helper)
            (  progDesc "Receive sample program"
            <> header "rx_samples_hx - Just like UHD's rx_samples_c"
            )

