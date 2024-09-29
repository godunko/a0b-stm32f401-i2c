--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  pragma Restrictions (No_Elaboration_Code);

with A0B.STM32F401.DMA.DMA1.Stream0;
with A0B.STM32F401.DMA.DMA1.Stream6;

package A0B.I2C.STM32F401_I2C.I2C1
  with Preelaborate
is

   pragma Elaborate_Body;

   I2C1 : aliased A0B.I2C.STM32F401_I2C.Master_Controller
    (Peripheral      => A0B.STM32F401.SVD.I2C.I2C1_Periph'Access,
     Event_Interrupt => A0B.STM32F401.I2C1_EV,
     Error_Interrupt => A0B.STM32F401.I2C1_ER,
     Transmit_Stream => A0B.STM32F401.DMA.DMA1.Stream6.DMA1_Stream6'Access,
     Receive_Stream  => A0B.STM32F401.DMA.DMA1.Stream0.DMA1_Stream0'Access);

end A0B.I2C.STM32F401_I2C.I2C1;
