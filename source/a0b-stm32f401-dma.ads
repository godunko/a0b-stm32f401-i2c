--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  STM32F401 DMA

pragma Restrictions (No_Elaboration_Code);

with Interfaces;
with System;

with A0B.STM32F401.SVD.DMA;

package A0B.STM32F401.DMA
  with Preelaborate
is

   type Controller_Number is range 1 .. 2;

   type Stream_Number is mod 8;

   type Channel_Number is mod 8;

   type DMA_Controller
     (Peripheral : not null access A0B.STM32F401.SVD.DMA.DMA_Peripheral;
      Controller : Controller_Number) is
     tagged limited private with Preelaborable_Initialization;

   type DMA_Stream
     (Controller : not null access DMA_Controller'Class;
      Stream     : Stream_Number;
      Interrupt  : A0B.STM32F401.Interrupt_Number) is
     tagged limited private with Preelaborable_Initialization;

   procedure Configure_Memory_To_Peripheral
     (Self       : in out DMA_Stream'Class;
      Channel    : Channel_Number;
      Peripheral : System.Address);
   --  Configure stream to do memory-to-peripheral transfers. All stream's
   --  interrupts are disabled, while NVIC interrupts are enabled.

   procedure Configure_Peripheral_To_Memory
     (Self       : in out DMA_Stream'Class;
      Channel    : Channel_Number;
      Peripheral : System.Address);

   procedure Set_Memory_Buffer
     (Self   : in out DMA_Stream'Class;
      Memory : System.Address;
      Count  : Interfaces.Unsigned_16);
   --  Sets address of the memory buffer and number of items to be transferred.

   procedure Enable (Self : in out DMA_Stream'Class);
   --  Enables stream

   procedure Disable (Self : in out DMA_Stream'Class);
   --  Disables stream

   procedure Enable_Transfer_Complete_Interrupt
     (Self : in out DMA_Stream'Class);

private

   type DMA_Controller
     (Peripheral : not null access A0B.STM32F401.SVD.DMA.DMA_Peripheral;
      Controller : Controller_Number) is
     tagged limited null record;

   procedure Enable_Clock (Self : in out DMA_Controller'Class);

   type DMA_Stream
     (Controller : not null access DMA_Controller'Class;
      Stream     : Stream_Number;
      Interrupt  : A0B.STM32F401.Interrupt_Number) is
        tagged limited null record;

end A0B.STM32F401.DMA;
