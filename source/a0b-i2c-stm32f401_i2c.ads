--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Implementation of the I2C bus master for the STM32F401 controller.
--  It use DMA for data transfer.

--  pragma Restrictions (No_Elaboration_Code);

private with Interfaces;

with A0B.ARMv7M;
with A0B.STM32F401.SVD.I2C;

with A0B.STM32F401.DMA;
private with A0B.STM32F401.SVD.DMA;

package A0B.I2C.STM32F401_I2C
  with Preelaborate
is

   type Master_Controller
     (Peripheral      : not null access A0B.STM32F401.SVD.I2C.I2C_Peripheral;
      Event_Interrupt : A0B.ARMv7M.External_Interrupt_Number;
      Error_Interrupt : A0B.ARMv7M.External_Interrupt_Number;
      Transmit_Stream : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
      Receive_Stream  : not null access A0B.STM32F401.DMA.DMA_Stream'Class) is
        limited new I2C_Bus_Master with private
          with Preelaborable_Initialization;

   procedure Configure (Self : in out Master_Controller'Class);

   --  subtype I2C1_Controller is Master_Controller
   --    (Peripheral      => A0B.STM32F401.SVD.I2C.I2C1_Periph'Access,
   --     Event_Interrupt => A0B.STM32F401.I2C1_EV,
   --     Error_Interrupt => A0B.STM32F401.I2C1_ER);

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

   type DMA_Stream
     (Peripheral : not null access A0B.STM32F401.SVD.DMA.DMA_Peripheral)
   is abstract tagged limited null record;

   not overriding function Get_Data_Length
     (Self : DMA_Stream) return Interfaces.Unsigned_16 is abstract;

   not overriding procedure Clear_Status (Self : in out DMA_Stream) is abstract;

   type Operation_Kind is (Read, Write);

   type Master_Controller
     (Peripheral      : not null access A0B.STM32F401.SVD.I2C.I2C_Peripheral;
      Event_Interrupt : A0B.ARMv7M.External_Interrupt_Number;
      Error_Interrupt : A0B.ARMv7M.External_Interrupt_Number;
      Transmit_Stream : not null access A0B.STM32F401.DMA.DMA_Stream'Class;
      Receive_Stream  : not null access A0B.STM32F401.DMA.DMA_Stream'Class) is
   limited new I2C_Bus_Master with record
      Device_Lock : Device_Locks.Lock;
      Device      : Device_Address;
      Operation   : Operation_Kind;
      Buffers     : access Buffer_Descriptor_Array;
      Active      : A0B.Types.Unsigned_32;
      C_Stream    : access DMA_Stream'Class;
      Stream      : access A0B.STM32F401.DMA.DMA_Stream'Class;
      Stop        : Boolean;
      --  Send of STOP condition is requested after completion of the current
      --  operation. This flag is used to reject erroneous Read/Write request
      --  after completion of the transfer and before release of the bus.

      C_Transmit_Stream : access DMA_Stream'Class;
      C_Receive_Stream  : access DMA_Stream'Class;

      BTF_Enabled     : Boolean;
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

   procedure On_Transmit_Stream_Interrupt
     (Self : in out Master_Controller'Class);

   procedure On_Receive_Stream_Interrupt
     (Self : in out Master_Controller'Class);

end A0B.I2C.STM32F401_I2C;
