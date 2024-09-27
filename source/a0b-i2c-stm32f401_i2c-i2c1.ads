--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  pragma Restrictions (No_Elaboration_Code);

package A0B.I2C.STM32F401_I2C.I2C1
  with Preelaborate
is

   pragma Elaborate_Body;

   I2C1 : aliased A0B.I2C.STM32F401_I2C.I2C1_Controller;

end A0B.I2C.STM32F401_I2C.I2C1;
