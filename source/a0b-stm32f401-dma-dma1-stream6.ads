--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  STM32F401 DMA

--  pragma Restrictions (No_Elaboration_Code);

--  private with Interfaces;
--
--  with A0B.ARMv7M;
--  with A0B.STM32F401.SVD.I2C;

--  private with A0B.STM32F401.SVD.DMA;

package A0B.STM32F401.DMA.DMA1.Stream6
  with Preelaborate
is

   DMA1_Stream6 : aliased DMA_Stream
     (Controller => A0B.STM32F401.DMA.DMA1.DMA1'Access,
      Stream     => 6,
      Interrupt  => A0B.STM32F401.DMA1_Stream6);

end A0B.STM32F401.DMA.DMA1.Stream6;
