module Clash.Cores.SPI where

import Clash.Prelude

spiSlave
  :: forall dom n
   . (HiddenClockResetEnable dom, KnownNat n)
  => Signal dom Bool
  -- ^ Slave select
  -> Signal dom Bit
  -- ^ MOSI
  -> Signal dom Bit
  -- ^ SCK
  -> Signal dom (BitVector n)
  -- ^ DIN
  -> Signal dom ( Bit -- MISO
                , Bool -- done
                , BitVector n -- DOUT
                )
spiSlave ss mosi sck din =
  moore go snd (0 :: Index n,(0,False,0)) (bundle (ss,mosi,sck,din))
 where
  go (bitCntQ,(misoQ,doneQ,doutQ)) (ssI,mosiI,sckI,dinI)
    | ssI
    = (0,(msb dataQ,False,doutQ))
    | otherwise
    = undefined
