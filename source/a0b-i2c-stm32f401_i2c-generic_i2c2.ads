--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

generic
   Transmit_Stream : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
   Receive_Stream  : not null access A0B.STM32F401.DMA.DMA_Stream'Class;

package A0B.I2C.STM32F401_I2C.Generic_I2C2
  with Preelaborate
is

   pragma Elaborate_Body;

   I2C2 : aliased A0B.I2C.STM32F401_I2C.Master_Controller
    (Peripheral       => A0B.STM32F401.SVD.I2C.I2C2_Periph'Access,
     Event_Interrupt  => A0B.STM32F401.I2C2_EV,
     Error_Interrupt  => A0B.STM32F401.I2C2_ER,
     Transmit_Stream  => Transmit_Stream,
     Transmit_Channel => 7,
     Receive_Stream   => Receive_Stream,
     Receive_Channel  => 7);

end A0B.I2C.STM32F401_I2C.Generic_I2C2;
