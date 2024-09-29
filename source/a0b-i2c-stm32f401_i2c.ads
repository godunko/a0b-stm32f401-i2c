--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Implementation of the I2C bus master for the STM32F401 controller.
--  It use DMA for data transfer.

pragma Restrictions (No_Elaboration_Code);

with A0B.ARMv7M;
with A0B.STM32F401.DMA;
with A0B.STM32F401.GPIO;
with A0B.STM32F401.SVD.I2C;

package A0B.I2C.STM32F401_I2C
  with Preelaborate
is

   type Controller_Number is range 1 .. 3;

   type Master_Controller
     (Peripheral       : not null access A0B.STM32F401.SVD.I2C.I2C_Peripheral;
      Controller       : Controller_Number;
      Event_Interrupt  : A0B.ARMv7M.External_Interrupt_Number;
      Error_Interrupt  : A0B.ARMv7M.External_Interrupt_Number;
      Transmit_Stream  : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
      Transmit_Channel : A0B.STM32F401.DMA.Channel_Number;
      Receive_Stream   : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
      Receive_Channel  : A0B.STM32F401.DMA.Channel_Number;
      SCL_Pin          : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
      SCL_Line         : A0B.STM32F401.Function_Line;
      SDA_Pin          : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
      SDA_Line         : A0B.STM32F401.Function_Line) is
        limited new I2C_Bus_Master with private
          with Preelaborable_Initialization;

   procedure Configure (Self : in out Master_Controller'Class);

private

   package Device_Locks is

      type Lock is limited private with Preelaborable_Initialization;

      procedure Acquire
        (Self    : in out Lock;
         Device  : not null I2C_Device_Driver_Access;
         Success : in out Boolean);

      procedure Release
        (Self    : in out Lock;
         Device  : not null I2C_Device_Driver_Access;
         Success : in out Boolean);

      function Device (Self : Lock) return I2C_Device_Driver_Access;

   private

      type Lock is limited record
         Device : I2C_Device_Driver_Access;
      end record;

      function Device (Self : Lock) return I2C_Device_Driver_Access is
        (Self.Device);

   end Device_Locks;

   type Operation_Kind is (Read, Write);

   type Master_Controller
     (Peripheral       : not null access A0B.STM32F401.SVD.I2C.I2C_Peripheral;
      Controller       : Controller_Number;
      Event_Interrupt  : A0B.ARMv7M.External_Interrupt_Number;
      Error_Interrupt  : A0B.ARMv7M.External_Interrupt_Number;
      Transmit_Stream  : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
      Transmit_Channel : A0B.STM32F401.DMA.Channel_Number;
      Receive_Stream   : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
      Receive_Channel  : A0B.STM32F401.DMA.Channel_Number;
      SCL_Pin          : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
      SCL_Line         : A0B.STM32F401.Function_Line;
      SDA_Pin          : not null access A0B.STM32F401.GPIO.GPIO_Line'Class;
      SDA_Line         : A0B.STM32F401.Function_Line) is
   limited new I2C_Bus_Master with record
      Device_Lock : Device_Locks.Lock;
      Device      : Device_Address;
      Operation   : Operation_Kind;
      Buffers     : access Buffer_Descriptor_Array;
      Active      : A0B.Types.Unsigned_32;
      Stream      : access A0B.STM32F401.DMA.DMA_Stream'Class;
      Stop        : Boolean;
      --  Send of STOP condition is requested after completion of the current
      --  operation. This flag is used to reject erroneous Read/Write request
      --  after completion of the transfer and before release of the bus.

      BTF_Enabled : Boolean;
      --  This flag is used to "mask" BTF flag when write operation is
      --  completed, but transaction is not closed. In particular, it "masks"
      --  event interrupt till START condition of the next operation is
      --  processed.
   end record;

   overriding procedure Start
     (Self    : in out Master_Controller;
      Device  : not null I2C_Device_Driver_Access;
      Success : in out Boolean);

   overriding procedure Write
     (Self    : in out Master_Controller;
      Device  : not null I2C_Device_Driver_Access;
      Buffers : in out Buffer_Descriptor_Array;
      Stop    : Boolean;
      Success : in out Boolean);

   overriding procedure Read
     (Self    : in out Master_Controller;
      Device  : not null I2C_Device_Driver_Access;
      Buffers : in out Buffer_Descriptor_Array;
      Stop    : Boolean;
      Success : in out Boolean);

   overriding procedure Stop
     (Self    : in out Master_Controller;
      Device  : not null I2C_Device_Driver_Access;
      Success : in out Boolean);

   procedure On_Event_Interrupt (Self : in out Master_Controller'Class);

   procedure On_Error_Interrupt (Self : in out Master_Controller'Class);

end A0B.I2C.STM32F401_I2C;
