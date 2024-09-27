--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2022;

with Interfaces;
with System.Address_To_Access_Conversions;
pragma Warnings (Off, """System.Atomic_Primitives"" is an internal GNAT unit");
with System.Atomic_Primitives;
with System.Storage_Elements;

with A0B.ARMv7M.NVIC_Utilities;
with A0B.STM32F401.SVD.DMA; use A0B.STM32F401.SVD.DMA;

package body A0B.I2C.STM32F401_I2C is

   use type A0B.Types.Unsigned_32;

   procedure Setup_Data_Transmit (Self : in out Master_Controller'Class)
     with Pre => Self.Operation = Write;
   --  Setup data transmission for the active buffer.

   procedure Setup_Data_Receive (Self : in out Master_Controller'Class)
     with Pre => Self.Operation = Read;
   --  Setup data transmission for the active buffer.

   ---------------
   -- Configure --
   ---------------

   procedure Configure (Self : in out Master_Controller'Class) is
   begin
      --  Disable IC2 to be able to configure it

      Self.Peripheral.CR1.PE := False;

      --  CR1 register

      declare
         Aux : A0B.STM32F401.SVD.I2C.CR1_Register := Self.Peripheral.CR1;

      begin
         --  Aux.PE := False;
         Aux.SMBUS     := False;  --  I2C mode
         Aux.SMBTYPE   := False;  --  SMBus Device
         Aux.ENARP     := False;  --  ARP disable
 --        Aux.ENPEC := False;  --  PEC calculation disabled
         Aux.ENGC      := False;
         --  General call disabled. Address 00h is NACKed.
         Aux.NOSTRETCH := False;  --  Clock stretching enabled

         --  START          => Boolean,
         --  STOP           => Boolean,
         --  ACK            => Boolean,
         --  POS            => Boolean,
         --  PEC            => Boolean,
         --  ALERT          => Boolean,
         --  SWRST          => Boolean,

         Self.Peripheral.CR1 := Aux;
      end;

      --  CR2 register

      declare
         Aux : A0B.STM32F401.SVD.I2C.CR2_Register := Self.Peripheral.CR2;

      begin
         Aux.FREQ    := 42;     --  APB1 run @42 MHz when CPU run @84 MHz
         Aux.ITERREN := True;   --  Error interrupt enabled
         Aux.ITEVTEN := True;   --  Event interrupt enabled
         Aux.ITBUFEN := False;
         --  TxE = 1 or RxNE = 1 does not generate any interrupt.
         Aux.DMAEN   := True;   --  DMA requests enabled

         --        LAST           => Boolean,

         Self.Peripheral.CR2 := Aux;
      end;

      --  OAR1 register

      declare
         Aux : A0B.STM32F401.SVD.I2C.OAR1_Register := Self.Peripheral.OAR1;

      begin
         Aux.ADDMODE := False;        --  7-bit slave address
         Aux.ADD7    := 2#000_0000#;
         Aux.ADD10   := 2#00#;
         Aux.ADD0    := False;        --  7-bit addressing mode: don’t care

         Self.Peripheral.OAR1 := Aux;
      end;

      --  OAR2 register

      declare
         Aux : A0B.STM32F401.SVD.I2C.OAR2_Register := Self.Peripheral.OAR2;

      begin
         Aux.ENDUAL := False;
         --  Only OAR1 is recognized in 7-bit addressing mode

         Self.Peripheral.OAR2 := Aux;
      end;

      --  CCR register

      declare
         Aux : A0B.STM32F401.SVD.I2C.CCR_Register := Self.Peripheral.CCR;

      begin
         Aux.F_S  := True;   --  Fm mode I2C
         Aux.DUTY := False;  --  Fm mode tlow/thigh = 2
         Aux.CCR  := 35;
         --  communication @400 kHz when CPU run @84 MHz (APB1 @42 MHz)

         Self.Peripheral.CCR := Aux;
      end;

      --  TRISE register

      declare
         Aux : A0B.STM32F401.SVD.I2C.TRISE_Register := Self.Peripheral.TRISE;

      begin
         Aux.TRISE := 13;  --  Fm mode @400 kHz when APB1 run @42 MHz

         Self.Peripheral.TRISE := Aux;
      end;

      --  Enable I2C

      Self.Peripheral.CR1.PE := True;

      --  Clear pending and enable NVIC interrupts

      A0B.ARMv7M.NVIC_Utilities.Clear_Pending (Self.Event_Interrupt);
      A0B.ARMv7M.NVIC_Utilities.Clear_Pending (Self.Error_Interrupt);
      A0B.ARMv7M.NVIC_Utilities.Enable_Interrupt (Self.Event_Interrupt);
      A0B.ARMv7M.NVIC_Utilities.Enable_Interrupt (Self.Error_Interrupt);

      --  Configure DMA stream (DMA 1 Stream 6 Channel 1)

      DMA1_Periph.S6CR :=
        (EN     => False,   --  Stream disabled
         DMEIE  => False,   --  DME interrupt disabled
         TEIE   => False,   --  TE interrupt disabled
         HTIE   => False,   --  HT interrupt disabled
         TCIE   => True,    --  TC interrupt enabled
         PFCTRL => False,   --  The DMA is the flow controller
         DIR    => 2#01#,   --  Memory-to-peripheral
         CIRC   => False,   --  Circular mode disabled
         PINC   => False,   --  Peripheral address pointer is fixed
         MINC   => True,
         --  Memory address pointer is incremented after each data transfer
         --  (increment is done according to MSIZE)
         PSIZE  => 2#00#,   --  Byte (8-bit)
         MSIZE  => 2#00#,   --  Byte (8-bit)
         PINCOS => <>,      --  No meaning
         PL     => 2#10#,   --  High
         DBM    => False,   --  No buffer switching at the end of transfer
         CT     => False,
         --  The current target memory is Memory 0 (addressed by the
         --  DMA_SxM0AR pointer)
         ACK    => <>,      --  ??? Not documented
         PBURST => 2#00#,   --  single transfer
         MBURST => 2#00#,   --  single transfer
         CHSEL  => 2#001#,  --  channel 1 selected
         others => <>);
      DMA1_Periph.S6PAR :=
        Interfaces.Unsigned_32
          (System.Storage_Elements.To_Integer (Self.Peripheral.DR'Address));

      A0B.ARMv7M.NVIC_Utilities.Clear_Pending (A0B.STM32F401.DMA1_Stream6);
      A0B.ARMv7M.NVIC_Utilities.Enable_Interrupt (A0B.STM32F401.DMA1_Stream6);

      --  Configure DMA stream (DMA 1 Stream 0 Channel 1)

      DMA1_Periph.S0CR :=
        (EN     => False,   --  Stream disabled
         DMEIE  => False,   --  DME interrupt disabled
         TEIE   => False,   --  TE interrupt disabled
         HTIE   => False,   --  HT interrupt disabled
         TCIE   => True,    --  TC interrupt enabled
         PFCTRL => False,   --  The DMA is the flow controller
         DIR    => 2#00#,   --  Peripheral-to-memory
         CIRC   => False,   --  Circular mode disabled
         PINC   => False,   --  Peripheral address pointer is fixed
         MINC   => True,
         --  Memory address pointer is incremented after each data transfer
         --  (increment is done according to MSIZE)
         PSIZE  => 2#00#,   --  Byte (8-bit)
         MSIZE  => 2#00#,   --  Byte (8-bit)
         PINCOS => <>,      --  No meaning
         PL     => 2#10#,   --  High
         DBM    => False,   --  No buffer switching at the end of transfer
         CT     => False,
         --  The current target memory is Memory 0 (addressed by the
         --  DMA_SxM0AR pointer)
         --  ACK    => <>,      --  ??? Not documented
         PBURST => 2#00#,   --  single transfer
         MBURST => 2#00#,   --  single transfer
         CHSEL  => 2#001#,  --  channel 1 selected
         others => <>);
      DMA1_Periph.S0PAR :=
        Interfaces.Unsigned_32
          (System.Storage_Elements.To_Integer (Self.Peripheral.DR'Address));

      --  DMA1_Periph.S0FCR :=

      A0B.ARMv7M.NVIC_Utilities.Clear_Pending (A0B.STM32F401.DMA1_Stream0);
      A0B.ARMv7M.NVIC_Utilities.Enable_Interrupt (A0B.STM32F401.DMA1_Stream0);
   end Configure;

   ------------------
   -- Device_Locks --
   ------------------

   package body Device_Locks is

      function Atomic_Compare_Exchange is
        new System.Atomic_Primitives.Atomic_Compare_Exchange
              (System.Storage_Elements.Integer_Address);

      package Conversions is
        new System.Address_To_Access_Conversions
              (Abstract_I2C_Device_Driver'Class);

      -------------
      -- Acquire --
      -------------

      procedure Acquire
        (Self    : in out Lock;
         Device  : not null I2C_Device_Driver_Access;
         Success : in out Boolean)
      is
         Aux : System.Storage_Elements.Integer_Address :=
           System.Storage_Elements.To_Integer (System.Null_Address);

      begin
         if not Success
           or else (Self.Device /= null and Self.Device /= Device)
         then
            Success := False;

            return;
         end if;

         if Self.Device /= null then
            return;
         end if;

         if not Atomic_Compare_Exchange
           (Ptr      => Self.Device'Address,
            Expected => Aux'Address,
            Desired  =>
              System.Storage_Elements.To_Integer
                (Conversions.To_Address
                   (Conversions.Object_Pointer (Device))))
         then
            Success := False;

            return;
         end if;

      end Acquire;

      -------------
      -- Release --
      -------------

      procedure Release
        (Self    : in out Lock;
         Device  : not null I2C_Device_Driver_Access;
         Success : in out Boolean) is
      begin
         if not Success
           or else Self.Device /= Device
         then
            Success := False;

            return;
         end if;

         Self.Device := null;
      end Release;

   end Device_Locks;

   ------------------------
   -- On_Error_Interrupt --
   ------------------------

   procedure On_Error_Interrupt (Self : in out Master_Controller'Class) is
   begin
      raise Program_Error;
   end On_Error_Interrupt;

   ------------------------
   -- On_Event_Interrupt --
   ------------------------

   procedure On_Event_Interrupt (Self : in out Master_Controller'Class) is

      use type Interfaces.Unsigned_8;

      Status : constant A0B.STM32F401.SVD.I2C.SR1_Register :=
        Self.Peripheral.SR1;

   begin
      if Status.SB then
         --  START/ReSTART condition has been sent.

         if Self.Device <= 16#7F# then
            Self.Peripheral.DR.DR :=
              (Interfaces.Shift_Left
                 (Interfaces.Unsigned_8 (Self.Device and 16#7F#), 1)
               or (case Self.Operation is
                     when Read      => 1,
                     when Write     => 0));

         else
            raise Program_Error;
         end if;

      elsif Status.ADDR then
         --  Address transmission completed.

         declare
            --  SR2 need to be read to clear ADDR bit

            Status : constant A0B.STM32F401.SVD.I2C.SR2_Register :=
              Self.Peripheral.SR2 with Unreferenced;

         begin
            null;
         end;

      elsif Status.ADD10 then
         raise Program_Error;
      end if;

      --  Other status bits sould be handled by this handler, but they are
      --  unused:
      --   - STOPF  Slave mode only
      --   - BTF    Meaningless and handled by DMA interrupt handler
      --   - RxNE   Never set due to use of DMA
      --   - TxE    Meaningless and handled by DMA interrupt handler
   end On_Event_Interrupt;

   ---------------------------------
   -- On_Receive_Stream_Interrupt --
   ---------------------------------

   procedure On_Receive_Stream_Interrupt
     (Self : in out Master_Controller'Class)
   is
      Mask  : constant S0CR_Register := DMA1_Periph.S0CR;
      State : constant LISR_Register := DMA1_Periph.LISR;

   begin
      if State.TCIF0 and Mask.TCIE then
         --  Transmission done.

         DMA1_Periph.LIFCR.CHTIF0 := True;
         DMA1_Periph.LIFCR.CTCIF0 := True;
         --  Clear TC and HT status flags

         DMA1_Periph.S0CR.EN := False;
         --  Disable DMA stream

         --  Transfer operation has been finished.

         Self.Buffers (Self.Active).Transferred :=
           Self.Buffers (Self.Active).Size;
         Self.Buffers (Self.Active).State := Success;

         if Self.Active /= Self.Buffers'Last then
            Self.Active := @ + 1;
            Self.Setup_Data_Receive;

         else
            declare
               Stop : constant Boolean := Self.Stop;
               --  Catch value, it can be changed by callback

            begin
               if Stop then
                  Self.Peripheral.CR1.STOP := True;
                  --  Send STOP condition when requested. There is no interrupt
                  --  to handle unset of BUSY by hardware, so attempt to
                  --  overlap send of STOP condition and processing of the
                  --  recieved data by the device driver.
               end if;

               Device_Locks.Device (Self.Device_Lock).On_Transfer_Completed;
               --  Notify device driver about end of operation.

               if Stop then
                  --  Wait till end of the transaction on the bus.

                  while Self.Peripheral.SR2.BUSY loop
                     null;
                  end loop;

                  Self.Stop := False;

                  declare
                     Device  : constant I2C_Device_Driver_Access :=
                       Device_Locks.Device (Self.Device_Lock);
                     Success : Boolean := True;

                  begin
                     Device_Locks.Release (Self.Device_Lock, Device, Success);

                     Device.On_Transaction_Completed;
                  end;
               end if;
            end;
         end if;

      else
         raise Program_Error;
      end if;
   end On_Receive_Stream_Interrupt;

   ----------------------------------
   -- On_Transmit_Stream_Interrupt --
   ----------------------------------

   procedure On_Transmit_Stream_Interrupt
     (Self : in out Master_Controller'Class)
   is
      Mask  : constant S6CR_Register := DMA1_Periph.S6CR;
      State : constant HISR_Register := DMA1_Periph.HISR;

   begin
      if State.TCIF6 and Mask.TCIE then
         --  Transmission done

         DMA1_Periph.HIFCR.CHTIF6 := True;
         DMA1_Periph.HIFCR.CTCIF6 := True;
         --  Clear TC and HT status flags

         DMA1_Periph.S6CR.EN := False;
         --  Disable DMA stream

         --  Update operation status

         Self.Buffers (Self.Active).Transferred :=
           Self.Buffers (Self.Active).Size;
         Self.Buffers (Self.Active).State := Success;

         if Self.Buffers'Last /= Self.Active then
            Self.Active := @ + 1;
            Self.Setup_Data_Transmit;

         else
            declare
               Stop : constant Boolean := Self.Stop;
               --  Catch value, it can be changed by callback

            begin
               if Stop then
                  Self.Peripheral.CR1.STOP := True;
                  --  Send STOP condition when requested. There is no interrupt
                  --  to handle unset of BUSY by hardware, so attempt to
                  --  overlap send of STOP condition and processing of the
                  --  recieved data by the device driver.
               end if;

               Device_Locks.Device (Self.Device_Lock).On_Transfer_Completed;
               --  Notify device driver about end of operation.

               if Stop then
                  --  Wait till end of the transaction on the bus.

                  while Self.Peripheral.SR2.BUSY loop
                     null;
                  end loop;

                  Self.Stop := False;

                  declare
                     Device  : constant I2C_Device_Driver_Access :=
                       Device_Locks.Device (Self.Device_Lock);
                     Success : Boolean := True;

                  begin
                     Device_Locks.Release (Self.Device_Lock, Device, Success);

                     Device.On_Transaction_Completed;
                  end;
               end if;
            end;
         end if;

      else
         raise Program_Error;
      end if;
   end On_Transmit_Stream_Interrupt;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self    : in out Master_Controller;
      Device  : not null I2C_Device_Driver_Access;
      Buffers : in out Buffer_Descriptor_Array;
      Stop    : Boolean;
      Success : in out Boolean) is
   begin
      Device_Locks.Acquire (Self.Device_Lock, Device, Success);

      if not Success then
         return;
      end if;

      if Self.Stop then
         Success := False;

         return;
      end if;

      for Buffer of Buffers loop
         Buffer.Transferred := 0;
         Buffer.State       := Active;
      end loop;

      Self.Device    := Device.Target_Address;
      Self.Operation := Read;
      Self.Buffers   := Buffers'Unrestricted_Access;
      Self.Active    := 0;
      Self.Stop      := Stop;

      Self.Setup_Data_Receive;

      Self.Peripheral.CR1.START := True;
      --  Send START condition
   end Read;

   ------------------------
   -- Setup_Data_Receive --
   ------------------------

   procedure Setup_Data_Receive (Self : in out Master_Controller'Class) is
   begin
      --  Configure DMA transfer

      DMA1_Periph.S0NDTR.NDT :=
        S6NDTR_NDT_Field (Self.Buffers (Self.Active).Size);
      DMA1_Periph.S0M0AR :=
        Interfaces.Unsigned_32
          (System.Storage_Elements.To_Integer
             (Self.Buffers (Self.Active).Address));

      --  Mark last DMA transfer operation

      Self.Peripheral.CR2.LAST :=  Self.Buffers'Last = Self.Active;

      --  Reset state of the DMA stream

      declare
         Aux : LIFCR_Register := DMA1_Periph.LIFCR;

      begin
         Aux.CFEIF0  := True;
         Aux.CDMEIF0 := True;
         Aux.CTEIF0  := True;
         Aux.CHTIF0  := True;
         Aux.CTCIF0  := True;

         DMA1_Periph.LIFCR := Aux;
      end;

      --  Enable DMA stream

      DMA1_Periph.S0CR.EN := True;
   end Setup_Data_Receive;

   -------------------------
   -- Setup_Data_Transmit --
   -------------------------

   procedure Setup_Data_Transmit (Self : in out Master_Controller'Class) is
   begin
      --  Configure DMA transfer

      DMA1_Periph.S6NDTR.NDT :=
        S6NDTR_NDT_Field (Self.Buffers (Self.Active).Size);
      DMA1_Periph.S6M0AR :=
        Interfaces.Unsigned_32
          (System.Storage_Elements.To_Integer
             (Self.Buffers (Self.Active).Address));

      --  Mark last DMA transfer operation

      Self.Peripheral.CR2.LAST :=  Self.Buffers'Last = Self.Active;

      --  Reset state of the DMA stream

      declare
         Aux : HIFCR_Register := DMA1_Periph.HIFCR;

      begin
         Aux.CFEIF6  := True;
         Aux.CDMEIF6 := True;
         Aux.CTEIF6  := True;
         Aux.CHTIF6  := True;
         Aux.CTCIF6  := True;

         DMA1_Periph.HIFCR := Aux;
      end;

      --  Enable DMA stream

      DMA1_Periph.S6CR.EN := True;
   end Setup_Data_Transmit;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (Self    : in out Master_Controller;
      Device  : not null I2C_Device_Driver_Access;
      Success : in out Boolean) is
   begin
      Device_Locks.Acquire (Self.Device_Lock, Device, Success);

      if not Success then
         return;
      end if;

      Self.Device := Device.Target_Address;
   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop
     (Self    : in out Master_Controller;
      Device  : not null I2C_Device_Driver_Access;
      Success : in out Boolean) is
   begin
      raise Program_Error;
--        --
--        --  A0B.ARMv7M.NVIC_Utilities.Disable_Interrupt (Self.Event_Interrupt);
--        --  --  Disable event interrup from the peripheral controller to prevent
--        --  --  undesired TC interrupt (it will be cleared by send of the START
--        --  --  condition).
--        --
--        --  Self.Peripheral.CR1.TCIE := True;
--        --  --  Enable TC and TCE interrupts.
--        --
--        --  --  Send STOP condition.
--        --
--        --  Self.Peripheral.CR2.STOP := True;
--        --
--        --  --  declare
--        --  --     Val : A0B.SVD.STM32H723.I2C.CR2_Register := Self.Peripheral.CR2;
--        --  --
--        --  --  begin
--        --  --     Val.RD_WRN  := True;           --  Master requests a read transfer.
--        --  --     Val.NBYTES  := Buffer'Length;  --  Number of bytes to be transfered.
--        --  --
--        --  --     Val.AUTOEND := False;
--        --  --     Val.RELOAD  := False;
--        --  --     Val.START   := True;
--        --  --     --  Val.RELOAD  := True;
--        --  --     --  Val.START   := Self.State /= Read;
--        --  --     --  if Self.State /= Read then
--        --  --
--        --  --     --
--        --  --     --     --  Send (Re)START condition
--        --  --     --
--        --  --     --     Val.START := True;
--        --  --     --  end if;
--        --  --
--        --  --     Self.Peripheral.CR2 := Val;
--        --  --  end;
--        --
--        --  A0B.ARMv7M.NVIC_Utilities.Clear_Pending (Self.Event_Interrupt);
--        --  A0B.ARMv7M.NVIC_Utilities.Enable_Interrupt (Self.Event_Interrupt);
--        --  --  Clear pending interrupt status and enable interrupt.
--        --
--        --  --  null;
--        raise Program_Error;
   end Stop;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self    : in out Master_Controller;
      Device  : not null I2C_Device_Driver_Access;
      Buffers : in out Buffer_Descriptor_Array;
      Stop    : Boolean;
      Success : in out Boolean) is
   begin
      Device_Locks.Acquire (Self.Device_Lock, Device, Success);

      if not Success then
         return;
      end if;

      if Self.Stop then
         Success := False;

         return;
      end if;

      for Buffer of Buffers loop
         Buffer.Transferred := 0;
         Buffer.State       := Active;
      end loop;

      Self.Device    := Device.Target_Address;
      Self.Operation := Write;
      Self.Buffers   := Buffers'Unrestricted_Access;
      Self.Active    := 0;
      Self.Stop      := Stop;

      Self.Setup_Data_Transmit;

      Self.Peripheral.CR1.START := True;
      --  Send START condition
   end Write;

end A0B.I2C.STM32F401_I2C;
