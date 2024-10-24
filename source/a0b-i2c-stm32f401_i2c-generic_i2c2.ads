--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

with A0B.STM32F401.GPIO.PIOB;

with A0B.STM32F401.I2C_Function_Lines;

generic
   Transmit_Stream : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
   Receive_Stream  : not null access A0B.STM32F401.DMA.DMA_Stream'Class;

package A0B.I2C.STM32F401_I2C.Generic_I2C2
  with Preelaborate
is

   pragma Elaborate_Body;

   I2C2 : aliased A0B.I2C.STM32F401_I2C.Master_Controller
     (Peripheral       => A0B.STM32F401.SVD.I2C.I2C2_Periph'Access,
      Controller       => 2,
      Event_Interrupt  => A0B.STM32F401.I2C2_EV,
      Error_Interrupt  => A0B.STM32F401.I2C2_ER,
      Transmit_Stream  => Transmit_Stream,
      Transmit_Channel => 7,
      Receive_Stream   => Receive_Stream,
      Receive_Channel  => 7,
      SCL_Pin          => A0B.STM32F401.GPIO.PIOB.PB10'Access,
      SCL_Line         => A0B.STM32F401.I2C_Function_Lines.I2C2_SCL'Access,
      SDA_Pin          => A0B.STM32F401.GPIO.PIOB.PB3'Access,
      SDA_Line         => A0B.STM32F401.I2C_Function_Lines.I2C2_SDA'Access);

end A0B.I2C.STM32F401_I2C.Generic_I2C2;
