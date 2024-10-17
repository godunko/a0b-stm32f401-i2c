--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

with A0B.STM32F401.GPIO.PIOA;

with A0B.STM32F401.I2C_Function_Lines;

generic
   Transmit_Stream  : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
   Transmit_Channel : A0B.STM32F401.DMA.Channel_Number;
   Receive_Stream   : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
   Receive_Channel  : A0B.STM32F401.DMA.Channel_Number;
   SDA_Pin          : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;

package A0B.I2C.STM32F401_I2C.Generic_I2C3
  with Preelaborate
is

   pragma Elaborate_Body;

   I2C3 : aliased A0B.I2C.STM32F401_I2C.Master_Controller
     (Peripheral       => A0B.STM32F401.SVD.I2C.I2C3_Periph'Access,
      Controller       => 3,
      Event_Interrupt  => A0B.STM32F401.I2C3_EV,
      Error_Interrupt  => A0B.STM32F401.I2C3_ER,
      Transmit_Stream  => Transmit_Stream,
      Transmit_Channel => Transmit_Channel,
      Receive_Stream   => Receive_Stream,
      Receive_Channel  => Receive_Channel,
      SCL_Pin          => A0B.STM32F401.GPIO.PIOA.PA8'Access,
      SCL_Line         => A0B.STM32F401.I2C_Function_Lines.I2C3_SCL'Access,
      SDA_Pin          => SDA_Pin,
      SDA_Line         => A0B.STM32F401.I2C_Function_Lines.I2C3_SDA'Access);

end A0B.I2C.STM32F401_I2C.Generic_I2C3;
