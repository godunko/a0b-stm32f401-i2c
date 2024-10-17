--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  STM32F401 I2C function line descriptors.

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2022;

package A0B.STM32F401.I2C_Function_Lines
  with Preelaborate
is

   I2C1_SCL  : aliased constant A0B.STM32F401.Function_Line_Descriptor;
   I2C1_SDA  : aliased constant A0B.STM32F401.Function_Line_Descriptor;
   I2C1_SMBA : aliased constant A0B.STM32F401.Function_Line_Descriptor;
   I2C2_SCL  : aliased constant A0B.STM32F401.Function_Line_Descriptor;
   I2C2_SDA  : aliased constant A0B.STM32F401.Function_Line_Descriptor;
   I2C2_SMBA : aliased constant A0B.STM32F401.Function_Line_Descriptor;
   I2C3_SCL  : aliased constant A0B.STM32F401.Function_Line_Descriptor;
   I2C3_SDA  : aliased constant A0B.STM32F401.Function_Line_Descriptor;
   I2C3_SMBA : aliased constant A0B.STM32F401.Function_Line_Descriptor;

private

   I2C1_SCL  : aliased constant A0B.STM32F401.Function_Line_Descriptor :=
     [(B, 6, 4), (B, 8, 4)];
   I2C1_SDA  : aliased constant A0B.STM32F401.Function_Line_Descriptor :=
     [(B, 7, 4), (B, 9, 4)];
   I2C1_SMBA : aliased constant A0B.STM32F401.Function_Line_Descriptor :=
     [(B, 5, 4)];
   I2C2_SCL  : aliased constant A0B.STM32F401.Function_Line_Descriptor :=
     [(B, 10, 4)];
   I2C2_SDA  : aliased constant A0B.STM32F401.Function_Line_Descriptor :=
     [(B, 3, 9)];
   I2C2_SMBA : aliased constant A0B.STM32F401.Function_Line_Descriptor :=
     [(B, 12, 4)];
   I2C3_SCL  : aliased constant A0B.STM32F401.Function_Line_Descriptor :=
     [(A, 8, 4)];
   I2C3_SDA  : aliased constant A0B.STM32F401.Function_Line_Descriptor :=
     [(B, 4, 9), (C, 9, 4)];
   I2C3_SMBA : aliased constant A0B.STM32F401.Function_Line_Descriptor :=
     [(A, 9, 4)];

end A0B.STM32F401.I2C_Function_Lines;
