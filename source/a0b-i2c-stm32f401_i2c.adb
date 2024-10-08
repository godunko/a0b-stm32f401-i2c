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
with A0B.Callbacks.Generic_Non_Dispatching;
with A0B.STM32F401.SVD.RCC;

package body A0B.I2C.STM32F401_I2C is

   use type A0B.Types.Unsigned_32;

   procedure Setup_Data_Transfer (Self : in out Master_Controller'Class);
   --  Setup DMA data transmission for the active buffer.

   procedure Complte_Transfer
     (Self       : in out Master_Controller'Class;
      Successful : Boolean);
   --  Complete transfer of the chunk of the data.
   --
   --  When Success is False it means Acknowledge Failure, current operation
   --  is marked as incomplete, but successful, while following operations
   --  are marked as failed.

   procedure On_Transmit_Stream_Interrupt
     (Self : in out Master_Controller'Class);

   procedure On_Receive_Stream_Interrupt
     (Self : in out Master_Controller'Class);

   package On_Transmit_Stream_Interrupt_Callbacks is
     new A0B.Callbacks.Generic_Non_Dispatching
           (Master_Controller, On_Transmit_Stream_Interrupt);

   package On_Receive_Stream_Interrupt_Callbacks is
     new A0B.Callbacks.Generic_Non_Dispatching
           (Master_Controller, On_Receive_Stream_Interrupt);

   ----------------------
   -- Complte_Transfer --
   ----------------------

   procedure Complte_Transfer
     (Self       : in out Master_Controller'Class;
      Successful : Boolean) is
   begin
      --  Transfer operation has been finished.

      Self.Buffers (Self.Active).Transferred :=
        Self.Buffers (Self.Active).Size
          - Interfaces.Unsigned_32 (Self.Stream.Remaining_Items);

      if Successful then
         Self.Buffers (Self.Active).State        := Success;
         Self.Buffers (Self.Active).Acknowledged := True;

      else
         Self.Buffers (Self.Active).Acknowledged := False;

         if Self.Active = Self.Buffers'First
           and Self.Buffers (Self.Active).Transferred = 0
         then
            --  It is case when device address was not acknowledged. It means
            --  that operation has been failed.

            Self.Buffers (Self.Active).State := Failure;

         else

            Self.Buffers (Self.Active).State := Success;
         end if;

         for J in Self.Active + 1 .. Self.Buffers'Last loop
            Self.Buffers (J).State        := Failure;
            Self.Buffers (J).Acknowledged := False;
         end loop;
      end if;

      if Self.Active /= Self.Buffers'Last
        and Successful
      then
         --  Setup data transfer for the next buffer.

         Self.Active := @ + 1;
         Self.Setup_Data_Transfer;

      elsif Self.Operation = Write and Successful then
         --  Write operation has been completed successfully. BTE status bit
         --  will be set when transfer from the data register done. It is
         --  handled in the event interrupt handler.

         Self.Operation := Write_Done;

      else
         --  Operation is done.

         declare
            Stop : constant Boolean := Self.Stop;
            --  Catch value, it can be changed by callback

         begin
            if Successful and Self.Operation = Write then
               --  Wait till last byte move into shift register, overwise
               --  set of STOP drop this byte.
               --
               --  XXX Can it handled in the event handler?

               while not Self.Peripheral.SR1.TxE loop
                  null;
               end loop;
            end if;

            if Stop then
               Self.Peripheral.CR1.STOP := True;
               --  Send STOP condition when requested. There is no interrupt to
               --  handle unset of BUSY by hardware, so attempt to overlap send
               --  of STOP condition and processing of the recieved data by the
               --  device driver.
            end if;

            Device_Locks.Device (Self.Device_Lock).On_Transfer_Completed;
            --  Notify device driver about end of operation.

            if Stop then
               --  Wait till end of the transaction on the bus.

               while Self.Peripheral.SR2.BUSY loop
                  null;
               end loop;

               Self.Stop      := False;
               Self.Operation := None;

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
   end Complte_Transfer;

   ---------------
   -- Configure --
   ---------------

   procedure Configure (Self : in out Master_Controller'Class) is
   begin
      Self.SCL_Pin.Configure_Alternative_Function
        (Line  => Self.SCL_Line,
         Mode  => A0B.STM32F401.GPIO.Open_Drain,
         Speed => A0B.STM32F401.GPIO.Very_High,
         Pull  => A0B.STM32F401.GPIO.No);
      Self.SDA_Pin.Configure_Alternative_Function
        (Line  => Self.SDA_Line,
         Mode  => A0B.STM32F401.GPIO.Open_Drain,
         Speed => A0B.STM32F401.GPIO.Very_High,
         Pull  => A0B.STM32F401.GPIO.No);

      --  Enable peripheral's clock

      case Self.Controller is
         when 1 =>
            A0B.STM32F401.SVD.RCC.RCC_Periph.APB1ENR.I2C1EN := True;

         when 2 =>
            A0B.STM32F401.SVD.RCC.RCC_Periph.APB1ENR.I2C2EN := True;

         when 3 =>
            A0B.STM32F401.SVD.RCC.RCC_Periph.APB1ENR.I2C3EN := True;
      end case;

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

      --  Configure DMA stream for data transmit

      Self.Transmit_Stream.Configure_Memory_To_Peripheral
        (Channel    => Self.Transmit_Channel,
         Peripheral => Self.Peripheral.DR'Address);
      Self.Transmit_Stream.Enable_Transfer_Complete_Interrupt;
      Self.Transmit_Stream.Set_Interrupt_Callback
        (On_Transmit_Stream_Interrupt_Callbacks.Create_Callback (Self));

      --  Configure DMA stream for data receive

      Self.Receive_Stream.Configure_Peripheral_To_Memory
        (Channel    => Self.Receive_Channel,
         Peripheral => Self.Peripheral.DR'Address);
      Self.Receive_Stream.Enable_Transfer_Complete_Interrupt;
      Self.Receive_Stream.Set_Interrupt_Callback
        (On_Receive_Stream_Interrupt_Callbacks.Create_Callback (Self));
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
      Status : constant A0B.STM32F401.SVD.I2C.SR1_Register :=
        Self.Peripheral.SR1;

   begin
      if Status.OVR then
         raise Program_Error;
      end if;

      if Status.AF then
         Self.Peripheral.SR1.AF := False;

         Self.Complte_Transfer (Successful => False);
      end if;

      if Status.ARLO then
         raise Program_Error;
      end if;

      if Status.BERR then
         raise Program_Error;
      end if;

      --  Ignored
      --   - PECERR    PEC is not used
      --   - TIMEOUT   SMBus mode only
      --   - SMBALERT  SMBus mode only
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
                     when Read   => 1,
                     when Write  => 0,
                     when others => raise Program_Error));

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
            Self.BTF_Enabled := True;
         end;

      elsif Status.ADD10 then
         raise Program_Error;

      elsif Status.BTF and Self.BTF_Enabled then
         declare
            Stop : constant Boolean := Self.Stop;
            --  Catch value, it can be changed by callback

         begin
            Self.BTF_Enabled := False;

            if Stop then
               Self.Peripheral.CR1.STOP := True;
               --  Send STOP condition when requested. There is no interrupt to
               --  handle unset of BUSY by hardware, so attempt to overlap send
               --  of STOP condition and processing of the recieved data by the
               --  device driver.
            end if;

            Device_Locks.Device (Self.Device_Lock).On_Transfer_Completed;
            --  Notify device driver about end of operation.

            if Stop then
               --  Wait till end of the transaction on the bus.

               while Self.Peripheral.SR2.BUSY loop
                  null;
               end loop;

               Self.Stop      := False;
               Self.Operation := None;

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
     (Self : in out Master_Controller'Class) is
   begin
      if Self.Receive_Stream.Get_Masked_And_Clear_Transfer_Completed then
         --  Transmission done, TC flag has been cleared

         Self.Receive_Stream.Disable;
         --  Disable DMA stream

         Self.Complte_Transfer (Successful => True);

      else
         raise Program_Error;
      end if;
   end On_Receive_Stream_Interrupt;

   ----------------------------------
   -- On_Transmit_Stream_Interrupt --
   ----------------------------------

   procedure On_Transmit_Stream_Interrupt
     (Self : in out Master_Controller'Class) is
   begin
      if Self.Transmit_Stream.Get_Masked_And_Clear_Transfer_Completed then
         --  Transmission done, TC flag has been cleared

         Self.Transmit_Stream.Disable;
         --  Disable DMA stream

         Self.Complte_Transfer (Successful => True);

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
         Buffer.Transferred  := 0;
         Buffer.State        := Active;
         Buffer.Acknowledged := False;
      end loop;

      Self.Device    := Device.Target_Address;
      Self.Operation := Read;
      Self.Stream    := Self.Receive_Stream.all'Unchecked_Access;
      Self.Buffers   := Buffers'Unrestricted_Access;
      Self.Active    := 0;
      Self.Stop      := Stop;

      Self.Setup_Data_Transfer;

      Self.Peripheral.CR1.START := True;
      --  Send START condition
   end Read;

   -------------------------
   -- Setup_Data_Transfer --
   -------------------------

   procedure Setup_Data_Transfer (Self : in out Master_Controller'Class) is
   begin
      --  Configure DMA transfer

      Self.Stream.Set_Memory_Buffer
        (Self.Buffers (Self.Active).Address,
         Interfaces.Unsigned_16 (Self.Buffers (Self.Active).Size));

      --  For read operation I2C controller need to be configured to
      --  generate acknowledge:
      --   - CR1.ACK should be set when total number of received bytes
      --     greater than 1
      --   - CR2.LAST should be set when this DMA transfer is last transer
      --     in the operation
      --
      --  These flags are ignored for write operation.

      Self.Peripheral.CR1.ACK  :=
        Self.Active /= Self.Buffers'Last
          or Self.Buffers (Self.Active).Size > 1;
      Self.Peripheral.CR2.LAST := Self.Active = Self.Buffers'Last;

      Self.Stream.Clear_Interrupt_Status;
      --  Reset state of the DMA stream

      Self.Stream.Enable;
      --  Enable DMA stream
   end Setup_Data_Transfer;

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

      if Self.Peripheral.SR2.BUSY then
         --  Bus is busy, reject Start operation.

         Device_Locks.Release (Self.Device_Lock, Device, Success);
         Success := False;

      else
         Self.Device := Device.Target_Address;
      end if;
   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop
     (Self    : in out Master_Controller;
      Device  : not null I2C_Device_Driver_Access;
      Success : in out Boolean) is
   begin
      Device_Locks.Acquire (Self.Device_Lock, Device, Success);

      if not Success then
         return;
      end if;

      Self.Peripheral.CR1.STOP := True;
      --  Send STOP condition when requested. There is no interrupt to
      --  handle unset of BUSY by hardware, so attempt to overlap send
      --  of STOP condition and processing of the recieved data by the
      --  device driver.

      --  Wait till end of the transaction on the bus.

      while Self.Peripheral.SR2.BUSY loop
         null;
      end loop;

      Self.Stop := False;
      Self.Operation := None;

      declare
         Device  : constant I2C_Device_Driver_Access :=
           Device_Locks.Device (Self.Device_Lock);
         Success : Boolean := True;

      begin
         Device_Locks.Release (Self.Device_Lock, Device, Success);

         Device.On_Transaction_Completed;
      end;
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
         Buffer.Transferred  := 0;
         Buffer.State        := Active;
         Buffer.Acknowledged := False;
      end loop;

      Self.Device    := Device.Target_Address;
      Self.Operation := Write;
      Self.Stream    := Self.Transmit_Stream.all'Unchecked_Access;
      Self.Buffers   := Buffers'Unrestricted_Access;
      Self.Active    := 0;
      Self.Stop      := Stop;

      Self.Setup_Data_Transfer;

      Self.Peripheral.CR1.START := True;
      --  Send START condition
   end Write;

end A0B.I2C.STM32F401_I2C;
