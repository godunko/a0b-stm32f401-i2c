--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

generic
   Transmit_Stream : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
   Receive_Stream  : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
   SCL_Pin         : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
   SDA_Pin         : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;

package A0B.I2C.STM32F401_I2C.Generic_I2C1
  with Preelaborate
is

   pragma Elaborate_Body;

   I2C1 : aliased A0B.I2C.STM32F401_I2C.Master_Controller
     (Peripheral       => A0B.STM32F401.SVD.I2C.I2C1_Periph'Access,
      Controller       => 1,
      Event_Interrupt  => A0B.STM32F401.I2C1_EV,
      Error_Interrupt  => A0B.STM32F401.I2C1_ER,
      Transmit_Stream  => Transmit_Stream,
      Transmit_Channel => 1,
      Receive_Stream   => Receive_Stream,
      Receive_Channel  => 1,
      SCL_Pin          => SCL_Pin,
      SCL_Line         => A0B.STM32F401.I2C1_SCL,
      SDA_Pin          => SDA_Pin,
      SDA_Line         => A0B.STM32F401.I2C1_SDA);

end A0B.I2C.STM32F401_I2C.Generic_I2C1;
