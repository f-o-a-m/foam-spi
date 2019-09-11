{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module SPI where
import Clash.Prelude hiding (writeFile)
import qualified Prelude as P
import Data.Maybe
import Debug.Trace
import Data.Text.IO  (writeFile)
import Clash.Signal.Internal (Signal(..))

type WordSize = 8
type HalfSpiPeriod = 2
type SpiWord = BitVector WordSize
type ClkCounter = Index HalfSpiPeriod
type BitCounter = Index (WordSize+1)

clkIdle :: Bit
clkIdle = 0

-- topEntity = spiMaster @System
topEntity = spiSlave @System

spiSlave
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe SpiWord)
  -> Signal dom _
  -> (Signal dom _, Signal dom _)
spiSlave internal external = mealyB spiSlaveMealy slave_initState (internal,external)

spiMaster
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe SpiWord)
  -> "miso" ::: Signal dom Bit
  -> ( Signal dom (Maybe SpiWord)
     , Signal dom
       ( "sck"  ::: Bit
       , "mosi" ::: Bit
       , "ss#"  ::: Bit)
     )
spiMaster wordIn miso = mealyB spiMasterMealy initState (wordIn,miso)

data MasterState
  = Idle
  | Transfering Bit ClkCounter BitCounter SpiWord SpiWord
  | Done SpiWord
  deriving (Generic,NFDataX,Show)
initState :: MasterState
initState = Idle


shiftIn :: (KnownNat n, 1 <= n) => BitVector n -> Bit -> BitVector n
shiftIn bv b = replaceBit (0::Int) b (shiftL bv 1)

shiftOut :: (KnownNat n, 1 <= n) => BitVector n -> (Bit, BitVector n)
shiftOut bv = (msb bv, shiftL bv 1)

spiMasterMealy :: MasterState -> (Maybe SpiWord, Bit) -> (MasterState, (Maybe SpiWord,(Bit,Bit,Bit)))
spiMasterMealy state (inM, miso) = (state', (outM, (sck,mosi,ssN)))
  where
    state' = case (state,inM) of
      (Idle, Nothing)   -> Idle
      (Idle, Just outw) -> Transfering clkIdle maxBound maxBound outw 0
      (Transfering clkSt cntr 0     outw inw, _) -> Done inw
      (Transfering clkSt cntr bitNr outw inw, _)
        | cntr == 0 -> Transfering (complement clkSt) maxBound bitNr' outw' inw'
                         where
                          clkRising = clkSt == 0
                          outw' | clkRising = outw
                                | otherwise = shiftL outw 1
                          inw' | clkRising = shiftIn inw miso
                               | otherwise = inw
                          bitNr' | not clkRising = bitNr - 1
                                 | otherwise = bitNr

      (Transfering clkSt cntr bitNr outw inw, _)
        -> Transfering clkSt (cntr-1) bitNr outw inw

      (Done _, _) -> Idle

    outM = case state of
      Done inw -> Just inw
      _        -> Nothing

    mosi = case state of
      Transfering _ _ _ outw _ -> msb outw
      _                        -> 0

    sck = case state of
      Transfering clkState _ _ _ _ -> clkState
      _                             -> clkIdle

    ssN = case state of
      Transfering {} -> low
      _              -> high

data SlaveState = SlaveState {slv_prev_clk :: Bit, slv_inw :: SpiWord, slv_outw :: SpiWord, slv_state :: SlaveState'} deriving (Generic,NFDataX)
data SlaveState'
  = Slave_Idle
  | Slave_Transfering BitCounter deriving (Generic,NFDataX)

slave_initState = SlaveState clkIdle 0 0 Slave_Idle

spiSlaveMealy :: SlaveState -> (Maybe SpiWord,  (Bit,Bit,Bit)) -> (SlaveState, (Maybe SpiWord, Bit))
spiSlaveMealy state (inM, (sck,mosi,ssN)) = (state', (outM, miso))
  where
    state' = ...

    outM = Nothing
    miso = 0


data Change = NoChange | Rising | Falling deriving (Eq,Generic,NFDataX)
getChange :: Bit -> Bit -> Change
getChange prev now = case (prev,now) of
  (0,1) -> Rising
  (1,0) -> Fallig
  _     -> NoChange



testSpi = withClockResetEnable clockGen resetGen enableGen (spiMaster @System)

testInputInternal :: Signal System (Maybe SpiWord)
testInputInternal = fromList (P.replicate 3 Nothing <> (Just 0xac : P.replicate 80 Nothing <> [Just 0xaa] <> P.repeat Nothing))

testInputInternal' = sample testInputInternal
mosiIn = P.replicate 200 1

input = P.zip testInputInternal' mosiIn

testSpi2 :: Signal System (Maybe SpiWord, (Bit,Bit,Bit))
testSpi2 = bundle $ testSpi testInputInternal (pure 1)

testInput = catMaybes (fst <$> sampleN 200 testSpi2) == [0b1111_1111]
-- geeft nu [0b0111_1111]

simulateMealy :: (s -> i -> (s,o)) -> s -> [i] -> [o]
simulateMealy f = go
  where
    go _ [] = []
    go s (i:is) = let (s',o) = f s i in o : go s' is

simulateMealyS :: (s -> i -> (s,o)) -> s -> [i] -> [(s,o)]
simulateMealyS f = go
  where
    go _ [] = []
    go s (i:is) = let (s',o) = f s i in (s,o) : go s' is

testme = putStrLn $ unlines $ fmap show $ simulateMealyS spiMasterMealy initState input



-- | Collect traces, and dump them to a VCD file.
main :: IO ()
main = do
  let
    miso = traceSignal1 "miso" $ (pure 1)
    (outw, unbundle -> (sck,mosi,ss)) = testSpi testInputInternal miso
    sck' = traceSignal1 "sck" sck
    mosi' = traceSignal1 "mosi" mosi
    ss' = traceSignal1 "ss#" ss
    combined = bundle (sck',mosi',ss')
  vcd <- dumpVCD (0, 400) combined ["sck",  "miso",  "mosi", "ss#"]
  case vcd of
    Left msg ->
      error msg
    Right contents -> do
      writeFile "spi.vcd" contents
      putStrLn "written output vcd file"
