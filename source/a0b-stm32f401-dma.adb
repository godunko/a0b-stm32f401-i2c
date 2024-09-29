--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  STM32F401 DMA

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2022;

with System.Address_To_Access_Conversions;

with A0B.ARMv7M.NVIC_Utilities;
with A0B.STM32F401.SVD.RCC; --  use A0B.STM32F401.SVD.RCC;

package body A0B.STM32F401.DMA is

   type Stream_Registers is record
      CR   : A0B.STM32F401.SVD.DMA.S1CR_Register
        with Volatile, Full_Access_Only;
      NDTR : A0B.STM32F401.SVD.DMA.S1NDTR_Register
        with Volatile, Full_Access_Only;
      PAR  : System.Address
        with Volatile, Full_Access_Only;
      M0AR : System.Address
        with Volatile, Full_Access_Only;
      M1AR : System.Address
        with Volatile, Full_Access_Only;
      FCR  : A0B.STM32F401.SVD.DMA.S1FCR_Register
        with Volatile, Full_Access_Only;
   end record;
   --  Set of stream configuration registers.
   --
   --  S0CR_Register doesn't have undocumented ACK component generated, so
   --  S1CR_Register is used.

   type Stream_Registers_Access is access all Stream_Registers;

   function Registers
     (Self : DMA_Stream'Class) return not null Stream_Registers_Access;

   ----------------------------
   -- Clear_Interrupt_Status --
   ----------------------------

   procedure Clear_Interrupt_Status (Self : in out DMA_Stream'Class) is
   begin
      case Self.Stream is
         when 0 =>
            Self.Controller.Peripheral.LIFCR :=
              (CFEIF0 | CDMEIF0 | CTEIF0 | CHTIF0 | CTCIF0 => True,
               others                                      => <>);

         when 1 =>
            Self.Controller.Peripheral.LIFCR :=
              (CFEIF1 | CDMEIF1 | CTEIF1 | CHTIF1 | CTCIF1 => True,
               others                                      => <>);

         when 2 =>
            Self.Controller.Peripheral.LIFCR :=
              (CFEIF2 | CDMEIF2 | CTEIF2 | CHTIF2 | CTCIF2 => True,
               others                                      => <>);

         when 3 =>
            Self.Controller.Peripheral.LIFCR :=
              (CFEIF3 | CDMEIF3 | CTEIF3 | CHTIF3 | CTCIF3 => True,
               others                                      => <>);

         when 4 =>
            Self.Controller.Peripheral.HIFCR :=
              (CFEIF4 | CDMEIF4 | CTEIF4 | CHTIF4 | CTCIF4 => True,
               others                                      => <>);

         when 5 =>
            Self.Controller.Peripheral.HIFCR :=
              (CFEIF5 | CDMEIF5 | CTEIF5 | CHTIF5 | CTCIF5 => True,
               others                                      => <>);

         when 6 =>
            Self.Controller.Peripheral.HIFCR :=
              (CFEIF6 | CDMEIF6 | CTEIF6 | CHTIF6 | CTCIF6 => True,
               others                                      => <>);

         when 7 =>
            Self.Controller.Peripheral.HIFCR :=
              (CFEIF7 | CDMEIF7 | CTEIF7 | CHTIF7 | CTCIF7 => True,
               others                                      => <>);
      end case;
   end Clear_Interrupt_Status;

   ------------------------------------
   -- Configure_Memory_To_Peripheral --
   ------------------------------------

   procedure Configure_Memory_To_Peripheral
     (Self       : in out DMA_Stream'Class;
      Channel    : Channel_Number;
      Peripheral : System.Address)
   is
      Registers : constant not null Stream_Registers_Access := Self.Registers;

   begin
      Self.Controller.Enable_Clock;

      declare
         Aux : A0B.STM32F401.SVD.DMA.S1CR_Register := Registers.CR;

      begin
         Aux.EN     := False;  --  Stream disabled
         Aux.DMEIE  := False;  --  DME interrupt disabled
         Aux.TEIE   := False;  --  TE interrupt disabled
         Aux.HTIE   := False;  --  HT interrupt disabled
         Aux.TCIE   := False;  --  TC interrupt enabled
         Aux.PFCTRL := False;  --  The DMA is the flow controller
         Aux.CIRC   := False;  --  Circular mode disabled
         Aux.PINC   := False;  --  Peripheral address pointer is fixed
         --  Aux.PINCOS := <>;     --  No meaning when PINC = False
         Aux.MINC   := True;
         --  Memory address pointer is incremented after each data transfer
         --  (increment is done according to MSIZE)
         Aux.DBM    := False;  --  No buffer switching at the end of transfer
         Aux.CT     := False;
         --  The current target memory is Memory 0 (addressed by the
         --  DMA_SxM0AR pointer)
         --  Aux.ACK    := <>;     --  ??? Not documented
         Aux.PBURST := 2#00#;  --  single transfer
         Aux.MBURST := 2#00#;  --  single transfer

         Aux.PSIZE  := 2#00#;  --  Byte (8-bit)
         Aux.MSIZE  := 2#00#;  --  Byte (8-bit)
         Aux.PL     := 2#10#;  --  High

         Aux.DIR    := 2#01#;  --  Memory-to-peripheral
         Aux.CHSEL  := A0B.STM32F401.SVD.DMA.S1CR_CHSEL_Field (Channel);

         Registers.CR := Aux;
      end;

      Registers.PAR := Peripheral;

      A0B.ARMv7M.NVIC_Utilities.Clear_Pending (Self.Interrupt);
      A0B.ARMv7M.NVIC_Utilities.Enable_Interrupt (Self.Interrupt);
   end Configure_Memory_To_Peripheral;

   ------------------------------------
   -- Configure_Peripheral_To_Memory --
   ------------------------------------

   procedure Configure_Peripheral_To_Memory
     (Self       : in out DMA_Stream'Class;
      Channel    : Channel_Number;
      Peripheral : System.Address)
   is
      Registers : constant not null Stream_Registers_Access := Self.Registers;

   begin
      Self.Controller.Enable_Clock;

      declare
         Aux : A0B.STM32F401.SVD.DMA.S1CR_Register := Registers.CR;

      begin
         Aux.EN     := False;  --  Stream disabled
         Aux.DMEIE  := False;  --  DME interrupt disabled
         Aux.TEIE   := False;  --  TE interrupt disabled
         Aux.HTIE   := False;  --  HT interrupt disabled
         Aux.TCIE   := False;  --  TC interrupt enabled
         Aux.PFCTRL := False;  --  The DMA is the flow controller
         Aux.CIRC   := False;  --  Circular mode disabled
         Aux.PINC   := False;  --  Peripheral address pointer is fixed
         --  Aux.PINCOS := <>;     --  No meaning when PINC = False
         Aux.MINC   := True;
         --  Memory address pointer is incremented after each data transfer
         --  (increment is done according to MSIZE)
         Aux.DBM    := False;  --  No buffer switching at the end of transfer
         Aux.CT     := False;
         --  The current target memory is Memory 0 (addressed by the
         --  DMA_SxM0AR pointer)
         --  Aux.ACK    := <>;     --  ??? Not documented
         Aux.PBURST := 2#00#;  --  single transfer
         Aux.MBURST := 2#00#;  --  single transfer

         Aux.PSIZE  := 2#00#;  --  Byte (8-bit)
         Aux.MSIZE  := 2#00#;  --  Byte (8-bit)
         Aux.PL     := 2#10#;  --  High

         Aux.DIR    := 2#00#;  --  Peripheral-to-memory
         Aux.CHSEL  := A0B.STM32F401.SVD.DMA.S1CR_CHSEL_Field (Channel);

         Registers.CR := Aux;
      end;

      Registers.PAR := Peripheral;

      A0B.ARMv7M.NVIC_Utilities.Clear_Pending (Self.Interrupt);
      A0B.ARMv7M.NVIC_Utilities.Enable_Interrupt (Self.Interrupt);
   end Configure_Peripheral_To_Memory;

   -------------
   -- Disable --
   -------------

   procedure Disable (Self : in out DMA_Stream'Class) is
      Registers : constant not null Stream_Registers_Access := Self.Registers;

   begin
      Registers.CR.EN := False;
   end Disable;

   ------------
   -- Enable --
   ------------

   procedure Enable (Self : in out DMA_Stream'Class) is
      Registers : constant not null Stream_Registers_Access := Self.Registers;

   begin
      Registers.CR.EN := True;
   end Enable;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (Self : in out DMA_Controller'Class) is
   begin
      case Self.Controller is
         when 1 =>
            A0B.STM32F401.SVD.RCC.RCC_Periph.AHB1ENR.DMA1EN := True;

         when 2 =>
            A0B.STM32F401.SVD.RCC.RCC_Periph.AHB1ENR.DMA2EN := True;
      end case;
   end Enable_Clock;

   ----------------------------------------
   -- Enable_Transfer_Complete_Interrupt --
   ----------------------------------------

   procedure Enable_Transfer_Complete_Interrupt
     (Self : in out DMA_Stream'Class)
   is
      Registers : constant not null Stream_Registers_Access := Self.Registers;

   begin
      Registers.CR.TCIE := True;
   end Enable_Transfer_Complete_Interrupt;

   ---------------------------------------------
   -- Get_Masked_And_Clear_Transfer_Completed --
   ---------------------------------------------

   function Get_Masked_And_Clear_Transfer_Completed
     (Self : in out DMA_Stream'Class) return Boolean
   is
      Registers : constant not null Stream_Registers_Access := Self.Registers;

   begin
      case Self.Stream is
         when 0 =>
            return Result : constant Boolean :=
              Self.Controller.Peripheral.LISR.TCIF0 and Registers.CR.TCIE
            do
               Self.Controller.Peripheral.LIFCR.CTCIF0 := True;
            end return;

         when 1 =>
            return Result : constant Boolean :=
              Self.Controller.Peripheral.LISR.TCIF1 and Registers.CR.TCIE
            do
               Self.Controller.Peripheral.LIFCR.CTCIF1 := True;
            end return;

         when 2 =>
            return Result : constant Boolean :=
              Self.Controller.Peripheral.LISR.TCIF2 and Registers.CR.TCIE
            do
               Self.Controller.Peripheral.LIFCR.CTCIF2 := True;
            end return;

         when 3 =>
            return Result : constant Boolean :=
              Self.Controller.Peripheral.LISR.TCIF3 and Registers.CR.TCIE
            do
               Self.Controller.Peripheral.LIFCR.CTCIF3 := True;
            end return;

         when 4 =>
            return Result : constant Boolean :=
              Self.Controller.Peripheral.HISR.TCIF4 and Registers.CR.TCIE
            do
               Self.Controller.Peripheral.HIFCR.CTCIF4 := True;
            end return;

         when 5 =>
            return Result : constant Boolean :=
              Self.Controller.Peripheral.HISR.TCIF5 and Registers.CR.TCIE
            do
               Self.Controller.Peripheral.HIFCR.CTCIF5 := True;
            end return;

         when 6 =>
            return Result : constant Boolean :=
              Self.Controller.Peripheral.HISR.TCIF6 and Registers.CR.TCIE
            do
               Self.Controller.Peripheral.HIFCR.CTCIF6 := True;
            end return;

         when 7 =>
            return Result : constant Boolean :=
              Self.Controller.Peripheral.HISR.TCIF7 and Registers.CR.TCIE
            do
               Self.Controller.Peripheral.HIFCR.CTCIF7 := True;
            end return;
      end case;
   end Get_Masked_And_Clear_Transfer_Completed;

   ---------------
   -- Registers --
   ---------------

   function Registers
     (Self : DMA_Stream'Class) return not null Stream_Registers_Access
   is
      function To_Access
        (Item : System.Address) return Stream_Registers_Access;

      package Conversions is
        new System.Address_To_Access_Conversions (Stream_Registers);

      ---------------
      -- To_Access --
      ---------------

      function To_Access
        (Item : System.Address) return Stream_Registers_Access is
      begin
         return Stream_Registers_Access (Conversions.To_Pointer (Item));
      end To_Access;

   begin
      case Self.Stream is
         when 0 =>
            return To_Access (Self.Controller.Peripheral.S0CR'Address);

         when 1 =>
            return To_Access (Self.Controller.Peripheral.S1CR'Address);

         when 2 =>
            return To_Access (Self.Controller.Peripheral.S2CR'Address);

         when 3 =>
            return To_Access (Self.Controller.Peripheral.S3CR'Address);

         when 4 =>
            return To_Access (Self.Controller.Peripheral.S4CR'Address);

         when 5 =>
            return To_Access (Self.Controller.Peripheral.S5CR'Address);

         when 6 =>
            return To_Access (Self.Controller.Peripheral.S6CR'Address);

         when 7 =>
            return To_Access (Self.Controller.Peripheral.S7CR'Address);
      end case;
   end Registers;

   ---------------------
   -- Remaining_Items --
   ---------------------

   function Remaining_Items
     (Self : DMA_Stream'Class) return Interfaces.Unsigned_16
   is
      Registers : constant not null Stream_Registers_Access := Self.Registers;

   begin
      return Registers.NDTR.NDT;
   end Remaining_Items;

   -----------------------
   -- Set_Memory_Buffer --
   -----------------------

   procedure Set_Memory_Buffer
     (Self   : in out DMA_Stream'Class;
      Memory : System.Address;
      Count  : Interfaces.Unsigned_16)
   is
      Registers : constant not null Stream_Registers_Access := Self.Registers;

   begin
      Registers.M0AR     := Memory;
      Registers.NDTR.NDT := Count;
   end Set_Memory_Buffer;

end A0B.STM32F401.DMA;
