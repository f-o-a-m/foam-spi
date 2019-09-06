module SPI where
import Clash.Prelude


type SpiWord = BitVector 8
type Counter = Index 100

spiMaster
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe SpiWord)
  -> "miso" ::: Signal dom Bit
  -> ( Signal dom (Maybe SpiWord)
     , ( "mosi" ::: Signal dom Bit
       , "sck"  ::: Signal dom Bit
       , "ss#"  ::: Signal dom Bit)
     )
spiMaster wordIn miso = unbundle <$> mealyB spiMasterMealy initState (wordIn,miso)

data MasterState = Idle | Transfering Counter SpiWord SpiWord | Done SpiWord deriving (Generic,NFDataX)
initState :: MasterState
initState = Idle


shiftIn :: (KnownNat n, 1 <= n) => BitVector n -> Bit -> BitVector n
shiftIn bv b = replaceBit 0 b (shiftL bv 1)

shiftOut :: (KnownNat n, 1 <= n) => BitVector n -> (Bit,BitVector n)
shiftOut bv = (msb bv, shiftL bv 1)

spiMasterMealy :: MasterState -> (Maybe SpiWord, Bit) -> (MasterState,(Maybe SpiWord,(Bit,Bit,Bit)))
spiMasterMealy state (inM, miso) = (state', (outM, (mosi,sck,ssN)))
  where
    state' = case (state,inM) of
      (Idle, Just outw) -> Transfering maxBound outw 0
      (Transfering cntr outw inw, _) -> undefined
    outM = Nothing
    mosi = 0
    sck = 0
    ssN = 0
