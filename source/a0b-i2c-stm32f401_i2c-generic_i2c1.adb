--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

package body A0B.I2C.STM32F401_I2C.Generic_I2C1 is

   pragma Warnings
     (Off, "ll instances of ""*"" will have the same external name");
   --  It is by design to prevent multiple instanses of the package.

   procedure I2C1_EV_Handler
     with Export, Convention => C, External_Name => "I2C1_EV_Handler";

   procedure I2C1_ER_Handler
     with Export, Convention => C, External_Name => "I2C1_ER_Handler";

   ---------------------
   -- I2C1_ER_Handler --
   ---------------------

   procedure I2C1_ER_Handler is
   begin
      I2C1.On_Error_Interrupt;
   end I2C1_ER_Handler;

   ---------------------
   -- I2C1_EV_Handler --
   ---------------------

   procedure I2C1_EV_Handler is
   begin
      I2C1.On_Event_Interrupt;
   end I2C1_EV_Handler;

end A0B.I2C.STM32F401_I2C.Generic_I2C1;
