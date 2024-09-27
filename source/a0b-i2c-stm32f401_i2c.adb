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

   --  use A0B.STM32F401.SVD.I2C;

   --  procedure Configure_Target_Address
   --    (Self : in out Master_Controller'Class);
   --  Configure target address.

--     procedure Load_Into_TX (Self : in out Master_Controller'Class);
--     --  Write next byte into the TX register, switch buffer when necessary.
--
--     procedure Store_From_RX (Self : in out Master_Controller'Class);
--     --  Read next byte from the RX register, switch buffer when necessary.

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

--        --  Configure control register 1
--
--        declare
--           Val : A0B.SVD.STM32H723.I2C.CR1_Register := Self.Peripheral.CR1;
--
--        begin
--           Val.PECEN     := False;
--           Val.ALERTEN   := False;
--           Val.SMBDEN    := False;
--           --  Device default address disabled (I2C mode)
--           Val.SMBHEN    := False;    --  Host address disabled (I2C mode)
--           Val.GCEN      := False;
--           Val.WUPEN     := False;
--           Val.NOSTRETCH := False;    --  Must be kept cleared in master mode
--           Val.SBC       := False;
--           Val.RXDMAEN   := False;    --  RX DMA disabled
--           Val.TXDMAEN   := False;    --  TX DMA disabled
--           Val.ANFOFF    := False;    --  Analog filter enabled
--           Val.DNF       := 2#0000#;  --  Digital filter disabled
--           Val.ERRIE     := True;     --  Error interrupt enabled
--           Val.TCIE      := True;     --  Transfer Complete interrupt enabled
--           Val.STOPIE    := True;
--           --  Stop detection (STOPF) interrupt enabled
--           Val.NACKIE    := True;
--           --  Not acknowledge (NACKF) received interrupts enabled
--           Val.ADDRIE    := False;
--           --  Address match (ADDR) interrupts disabled
--           Val.RXIE      := True;     --  Receive (RXNE) interrupt enabled
--           Val.TXIE      := True;     --  Transmit (TXIS) interrupt enabled
--
--           Self.Peripheral.CR1 := Val;
--        end;
--
--        --  Configure timing register (Fast Mode)
--
--        declare
--           Val : A0B.SVD.STM32H723.I2C.TIMINGR_Register :=
--             Self.Peripheral.TIMINGR;
--
--        begin
--           --  Standard Mode
--
--           Val.PRESC  := 16#2#;
--           Val.SCLDEL := 16#A#;
--           Val.SDADEL := 16#0#;
--           Val.SCLH   := 16#AC#;
--           Val.SCLL   := 16#FE#;
--
--           --  Fast Mode
--           --  Val.PRESC  := 0;
--           --  Val.SCLDEL := 16#C#;
--           --  Val.SDADEL := 0;
--           --  Val.SCLH   := 16#45#;
--           --  Val.SCLL   := 16#ED#;
--
--           Self.Peripheral.TIMINGR := Val;
--        end;

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

   ------------------------------
   -- Configure_Target_Address --
   ------------------------------
--
--     procedure Configure_Target_Address
--       (Self : in out Master_Controller'Class)
--     is
--        --  Target_Address : constant Device_Address :=
--        --    Device_Locks.Device (Self.Device_Lock).Target_Address;
--
--     begin
--        raise Program_Error;
--  --        --  if Self.State = Initial then
--  --           --  Set device address and addressing mode to do its only once.
--  --
--  --           declare
--  --              Val : A0B.SVD.STM32H723.I2C.CR2_Register := Self.Peripheral.CR2;
--  --
--  --           begin
--  --              if Target_Address <= 16#7F# then
--  --                 Val.ADD10    := False;
--  --                 Val.SADD.Val :=
--  --                   A0B.Types.Unsigned_10
--  --                     (A0B.Types.Shift_Left
--  --                        (A0B.Types.Unsigned_32 (Target_Address), 1));
--  --                 --  In 7-bit addressing mode device address should be written
--  --                 --  to SADD[7:1], so shift it left by one bit.
--  --
--  --              else
--  --                 Val.ADD10    := True;
--  --                 Val.SADD.Val := A0B.Types.Unsigned_10 (Target_Address);
--  --                 --  In 7-bit addressing mode device address should be written
--  --                 --  to SADD[7:1], so shift it left by one bit.
--  --              end if;
--  --
--  --              Self.Peripheral.CR2 := Val;
--  --           end;
--  --        --  end if;
--     end Configure_Target_Address;

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

--     ------------------
--     -- Load_Into_TX --
--     ------------------
--
--     procedure Load_Into_TX (Self : in out Master_Controller'Class) is
--        use type A0B.Types.Unsigned_32;
--        use type System.Address;
--        use type System.Storage_Elements.Storage_Offset;
--
--     begin
--        if Self.Address = System.Null_Address then
--           --  Start of the transfer
--
--           Self.Address := Self.Buffers (Self.Active).Address;
--        end if;
--
--        loop
--           exit when
--             Self.Buffers (Self.Active).Size
--               /= Self.Buffers (Self.Active).Bytes;
--
--           Self.Buffers (Self.Active).State := Success;
--
--           Self.Active  := @ + 1;
--           Self.Address := Self.Buffers (Self.Active).Address;
--        end loop;
--
--        declare
--           Data : constant A0B.Types.Unsigned_8
--             with Import, Address => Self.Address;
--
--        begin
--           Self.Peripheral.TXDR.TXDATA := Data;
--
--           Self.Address                     := @ + 1;
--           Self.Buffers (Self.Active).Bytes := @ + 1;
--        end;
--     end Load_Into_TX;

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
      use type Interfaces.Unsigned_32;

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

      elsif Status.BTF then
         --  Byte transfer finished.

         if Self.Buffers'Last /= Self.Active then
            raise Program_Error;

         else
            --  Transfer operation has been finished.

            Self.Buffers (Self.Active).Bytes :=
              Self.Buffers (Self.Active).Size;
            Self.Buffers (Self.Active).State := Success;

            --  Send STOP condition when requested.

            if Self.Stop then
               Self.Peripheral.CR1.STOP := True;
            end if;

            --  Notify device driver.

            Device_Locks.Device (Self.Device_Lock).On_Transfer_Completed;
            --  declare
            --     Device  : constant I2C_Device_Driver_Access :=
            --       Device_Locks.Device (Self.Device_Lock);
            --     --  Success : Boolean := True;
            --
            --  begin
            --     Device_Locks.Release (Self.Device_Lock, Device, Success);
            --
            --     Device.On_Transfer_Completed;
            --  end;
         end if;

      elsif Status.TxE then
         --  Generally it should never happened because CR2.ITBUFEN is set to
         --  False, however, it happned. Anyway, there is nothing to do here,
         --  DMA starts transmission automatically.

         null;

      elsif Status.RxNE then
         raise Program_Error;

      --  elsif Status.ARLO then
      --     raise Program_Error;
      --
      --  elsif
      else
         --  It looks like case when DMA receive has been completed.

         if Self.Operation /= Read then
            raise Program_Error;
         end if;

         --  Transfer operation has been finished.

         Self.Buffers (Self.Active).Bytes :=
           Self.Buffers (Self.Active).Size;
         Self.Buffers (Self.Active).State := Success;

         --  Send STOP condition when requested.

         if Self.Stop then
            Self.Peripheral.CR1.STOP := True;
         end if;

         --  Notify device driver.

         Device_Locks.Device (Self.Device_Lock).On_Transfer_Completed;
      --     raise Program_Error;
      end if;

--        Status  : constant A0B.SVD.STM32H723.I2C.ISR_Register :=
--          Self.Peripheral.ISR;
--        Mask    : constant A0B.SVD.STM32H723.I2C.CR1_Register :=
--          Self.Peripheral.CR1;
--
   --  begin
   --     raise Program_Error;
--        if Status.TXIS then
--           Self.Load_Into_TX;
--        end if;
--
--        if Status.RXNE then
--           Self.Store_From_RX;
--        end if;
--
--        if Status.TC and Mask.TCIE then
--           Self.Peripheral.CR1.TCIE := False;
--           --  Disable TC (and TCR) interrupt, it is active till master sends
--           --  START/STOP condition. Device driver need to be notified only
--           --  once, and there is nothing to do till ball is on device driver's
--           --  side.
--
--           if Self.Stop then
--              Self.Peripheral.CR2.STOP := True;
--           end if;
--
--           Device_Locks.Device (Self.Device_Lock).On_Transfer_Completed;
--        end if;
--
--        if Status.NACKF then
--           Self.Peripheral.ICR.NACKCF := True;
--
--           for J in Self.Active .. Self.Buffers'Last loop
--              Self.Buffers (Self.Active).State := Failure;
--           end loop;
--        end if;
--
--        if Status.STOPF then
--           Self.Peripheral.ICR.STOPCF := True;
--           --  Clear STOPF interrupt status
--
--           --  if Self.  ???? Set status of the buffer ???
--
--           declare
--              Device  : constant I2C_Device_Driver_Access :=
--                Device_Locks.Device (Self.Device_Lock);
--              Success : Boolean := True;
--
--           begin
--              Device_Locks.Release (Self.Device_Lock, Device, Success);
--
--              Device.On_Transaction_Completed;
--           end;
--        end if;
--
--        -----------------------------------------------------------------------
--
--        if Status.TCR and Mask.TCIE then
--           raise Program_Error;
--           --  Self.Peripheral.CR1.TCIE := False;
--           --  --  Disable TCR and TC interrupts, software should write to NBYTES
--           --  --  to clear this flag. It will be re-enabled after this write by
--           --  --  Write/Read procedure.
--           --
--           --  Self.Status.State := Success;
--           --  Device_Locks.Device (Self.Device_Lock).On_Transfer_Completed;
--        end if;
--
--        if Self.Peripheral.ISR.ADDR then
--           raise Program_Error;
--        end if;
   end On_Event_Interrupt;

   ---------------------------------
   -- On_Receive_Stream_Interrupt --
   ---------------------------------

   procedure On_Receive_Stream_Interrupt
     (Self : in out Master_Controller'Class)
   is
      pragma Unreferenced (Self);

      Mask  : constant S0CR_Register := DMA1_Periph.S0CR;
      State : constant LISR_Register := DMA1_Periph.LISR;

   begin
      if State.TCIF0 and Mask.TCIE then
         --  Transmission done.

         --  DMA2_Periph.HIFCR.CFEIF6  := True;
         --  DMA2_Periph.HIFCR.CDMEIF6 := True;
         --  DMA2_Periph.HIFCR.CTEIF6  := True;
         DMA2_Periph.LIFCR.CHTIF0 := True;
         DMA1_Periph.LIFCR.CTCIF0 := True;
         --  Clear TC and HT status flags

         DMA1_Periph.S0CR.EN := False;
         --  Disable DMA stream

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
      pragma Unreferenced (Self);

      Mask  : constant S6CR_Register := DMA1_Periph.S6CR;
      State : constant HISR_Register := DMA1_Periph.HISR;

   begin
      if State.TCIF6 and Mask.TCIE then
         --  Transmission done.

         --  DMA2_Periph.HIFCR.CFEIF6  := True;
         --  DMA2_Periph.HIFCR.CDMEIF6 := True;
         --  DMA2_Periph.HIFCR.CTEIF6  := True;
         DMA2_Periph.HIFCR.CHTIF6 := True;
         DMA1_Periph.HIFCR.CTCIF6 := True;
         --  Clear TC and HT status flags

         DMA1_Periph.S6CR.EN := False;
         --  Disable DMA stream

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
      Success : in out Boolean)
   is
      use type A0B.Types.Unsigned_32;

--        Size : A0B.Types.Unsigned_32 := 0;
--
   begin
      Device_Locks.Acquire (Self.Device_Lock, Device, Success);

      if not Success then
         return;
      end if;

      for Buffer of Buffers loop
--           Size := @ + Buffer.Size;

         Buffer.Bytes := 0;
         Buffer.State := Active;
      end loop;

      Self.Device    := Device.Target_Address;
      Self.Operation := Read;
      Self.Buffers   := Buffers'Unrestricted_Access;
      Self.Active    := 0;
--        Self.Address := System.Null_Address;
      Self.Stop      := Stop;

      DMA1_Periph.S0NDTR.NDT :=
        S6NDTR_NDT_Field (Self.Buffers (Self.Active).Size);
      DMA1_Periph.S0M0AR :=
        Interfaces.Unsigned_32
          (System.Storage_Elements.To_Integer
             (Self.Buffers (Self.Active).Address));
      DMA1_Periph.S0CR.EN := True;

      if Self.Buffers'Last /= Self.Active then
         raise Program_Error;

      else
         Self.Peripheral.CR2.LAST := True;
      end if;

      Self.Peripheral.CR1.START := True;
      --  Send START condition

--        --  A0B.ARMv7M.NVIC_Utilities.Disable_Interrupt (Self.Event_Interrupt);
--        --  --  Disable event interrup from the peripheral controller to prevent
--        --  --  undesired TC interrupt (it will be cleared by send of the START
--        --  --  condition).
--
--        --  Set transfer parameters and send (Re)START condition.
--
--        declare
--           Val : A0B.SVD.STM32H723.I2C.CR2_Register := Self.Peripheral.CR2;
--
--        begin
--           Val.RD_WRN  := True;           --  Master requests a read transfer.
--           Val.NBYTES  := A0B.Types.Unsigned_8 (Size);
--           --  Number of bytes to be transfered.
--
--           Val.AUTOEND := False;
--           Val.RELOAD  := False;
--           Val.START   := True;
--           --  Val.RELOAD  := True;
--
--           Self.Peripheral.CR2 := Val;
--        end;
--
--        Self.Peripheral.CR1.TCIE := True;
--        --  Enable TC and TCE interrupts.
--
--        --  A0B.ARMv7M.NVIC_Utilities.Clear_Pending (Self.Event_Interrupt);
--        --  A0B.ARMv7M.NVIC_Utilities.Enable_Interrupt (Self.Event_Interrupt);
--        --  --  Clear pending interrupt status and enable interrupt.
   end Read;

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

--     -------------------
--     -- Store_From_RX --
--     -------------------
--
--     procedure Store_From_RX (Self : in out Master_Controller'Class) is
--        use type A0B.Types.Unsigned_32;
--        use type System.Address;
--        use type System.Storage_Elements.Storage_Offset;
--
--     begin
--        if Self.Address = System.Null_Address then
--           --  Start of the transfer
--
--           Self.Address := Self.Buffers (Self.Active).Address;
--        end if;
--
--        loop
--           exit when
--             Self.Buffers (Self.Active).Size
--               /= Self.Buffers (Self.Active).Bytes;
--
--           Self.Active  := @ + 1;
--           Self.Address := Self.Buffers (Self.Active).Address;
--        end loop;
--
--        declare
--           Data : A0B.Types.Unsigned_8
--             with Import, Address => Self.Address;
--
--        begin
--           Data := Self.Peripheral.RXDR.RXDATA;
--
--           Self.Address                     := @ + 1;
--           Self.Buffers (Self.Active).Bytes := @ + 1;
--        end;
--     end Store_From_RX;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self    : in out Master_Controller;
      Device  : not null I2C_Device_Driver_Access;
      Buffers : in out Buffer_Descriptor_Array;
      Stop    : Boolean;
      Success : in out Boolean)
   is
      use type A0B.Types.Unsigned_32;

      --  Size : A0B.Types.Unsigned_32 := 0;

   begin
      Device_Locks.Acquire (Self.Device_Lock, Device, Success);

      if not Success then
         return;
      end if;

      for Buffer of Buffers loop
         --  Size := @ + Buffer.Size;

         Buffer.Bytes := 0;
         Buffer.State := Active;
      end loop;

      Self.Device    := Device.Target_Address;
      Self.Operation := Write;
      Self.Buffers   := Buffers'Unrestricted_Access;
      Self.Active    := 0;
--        Self.Address := System.Null_Address;
      Self.Stop      := Stop;

--        Self.Configure_Target_Address;

      DMA1_Periph.S6NDTR.NDT :=
        S6NDTR_NDT_Field (Self.Buffers (Self.Active).Size);
      DMA1_Periph.S6M0AR :=
        Interfaces.Unsigned_32
          (System.Storage_Elements.To_Integer
             (Self.Buffers (Self.Active).Address));
      DMA1_Periph.S6CR.EN := True;

      if Self.Buffers'Last /= Self.Active then
         raise Program_Error;

      else
         Self.Peripheral.CR2.LAST := True;
      end if;

      Self.Peripheral.CR1.START := True;
      --  Send START condition

      --  raise Program_Error;
--        --  A0B.ARMv7M.NVIC_Utilities.Disable_Interrupt (Self.Event_Interrupt);
--        --  --  Disable event interrup from the peripheral controller to prevent
--        --  --  undesired TC interrupt (it will be cleared by send of the START
--        --  --  condition).
--
--        --  Apply workaround.
--        --
--        --  [ES0491] 2.16.4 Transmission stalled after first byte transfer
--        --
--        --  "Write the first data in I2C_TXDR before the transmission
--        --  starts."
--
--        if Size /= 0 then
--           Self.Load_Into_TX;
--        end if;
--
--        --  Set transfer parameters and send (Re)START condition.
--
--        declare
--           Val : A0B.SVD.STM32H723.I2C.CR2_Register := Self.Peripheral.CR2;
--
--        begin
--           Val.RD_WRN  := False;  --  Master requests a write transfer.
--           Val.NBYTES  := A0B.Types.Unsigned_8 (Size);
--           --  Number of bytes to be transfered.
--
--           Val.AUTOEND := False;
--           Val.RELOAD  := False;
--           Val.START   := True;
--
--           Self.Peripheral.CR2 := Val;
--        end;
--
--        Self.Peripheral.CR1.TCIE := True;
--        --  Enable TC and TCE interrupts.
--
--        --  A0B.ARMv7M.NVIC_Utilities.Clear_Pending (Self.Event_Interrupt);
--        --  A0B.ARMv7M.NVIC_Utilities.Enable_Interrupt (Self.Event_Interrupt);
--        --  --  Clear pending interrupt status and enable interrupt.
   end Write;

end A0B.I2C.STM32F401_I2C;
