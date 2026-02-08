{-# LANGUAGE DataKinds
           , RecordWildCards
           , TypeApplications
           #-}

module Main where

import Control.Monad

import Data.Complex

import Data.Int

import Data.Maybe

import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS

import System.Environment

import Network.UHD

main :: IO ()
main = do
    let p s = putStrLn . (s <>) . show

    [sr] <- fmap read <$> getArgs

    findUSRPs "type=b200"
        >>= p "found USRPs: "

    bmini <- makeUSRP "recv_frame_size=9000,num_recv_frames=1024"
    usrpLastError bmini >>= p "bmini error: "
    usrpPrettyPrint bmini >>= putStrLn

    -- usrpSetRxRate bmini 0 56000000
    usrpSetRxRate bmini 0 sr
    usrpGetRxRate bmini 0
        >>= p "rx rate: "

    usrpSetRxGain bmini 0 "" 5.0
    usrpGetRxGain bmini 0 ""
        >>= p "rx gain: "

    usrpSetRxFreq bmini 0 (TuneReq 2.4e9 TuneReqPolicyAuto TuneReqPolicyAuto "")
    usrpGetRxFreq bmini 0
        >>= p "rx freq: "

    putStrLn "making rx streamer..."
    rxstr <- usrpGetRxStream bmini (StreamArgs @(Complex Int16) [0] S16 "")
    putStrLn "made rx streamer"
    rxStreamerLastError rxstr
        >>= p "rx streamer last error: "
    rxStreamerNumChannels rxstr
        >>= p "rx streamer num chans: "
    rxStreamerMaxNumSamps rxstr
        >>= p "rx streamer max samps: "

    putStrLn "rx streamer issue cmd..."
    rxStreamerIssueCmd rxstr (StreamCmd StartContinuous StreamNow)
    putStrLn "rx streamer issued cmd"

    let go t = do
            (RxMetadata{..}, cs) <- rxStreamerRecv rxstr 1.0 False
            print $ VS.length $ V.head cs
            when (isNothing rmTime) $ putStrLn "No time!"
            when rmOutOfSequence $ putStrLn "Out of sequence!"
            case rmError of
                Nothing -> go (t + VS.length (V.head cs))
                Just e -> print e
    go 0

