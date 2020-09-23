with Ada.Real_Time;
use type Ada.Real_Time.Time;
use type Ada.Real_Time.Time_Span;
with SAM3x8e;
use type SAM3x8e.Bit;
with SAM3x8e.PMC;  --  Needed to enable I2C clocking
with SAM3x8e.PIO;  --  Needed to configure I2C pins
with SAM3x8e.TWI;  --  Needed for I2C interface
with BBS.embed;
use type BBS.embed.uint32;
with BBS.embed.due.serial.int;
with BBS.embed.log;
--
--  Package for the I2C interface
--
--  The Arduino Due has two I2C interfaces.
--  Interface  SCL  SDA   TWI
--  I2C-0      PB13 PB12  TWI0
--  I2C-1      PA18 PA17  TWI1
--
package body bbs.embed.i2c.due is
   --
   --  Function to return access to a device record.
   --
   function get_interface(d : port_id) return due_i2c_interface is
   begin
      return i2c_port(d);
   end;
   --
   --  Initialize an interface
   --
   --
   --  The I2C-0 port is connected to pins PB13 and PB12.  The UART
   --  initialization is used as a basis for this.
   --
   procedure init(chan : port_id; speed : speed_type) is
      pins    : uint32;
   begin
      --
      --  Initialize internal data structures for both devices.
      --
      i2c_port(0).dev_id  := BBS.embed.due.dev.TWI1_ID;
      i2c_port(0).port    := TWI1'Access;
      i2c_port(0).pioc    := BBS.embed.GPIO.Due.PIOB'Access;
      i2c_port(0).sda_pin := 12;
      i2c_port(0).scl_pin := 13;
      i2c_port(0).int_id  := Ada.Interrupts.Names.TWI1_Interrupt;
      i2c_port(0).handle  := buff(0);
      buff0.set_interface(i2c_0'Access);
      --
      i2c_port(1).dev_id := BBS.embed.due.dev.TWI0_ID;
      i2c_port(1).port   := TWI0'Access;
      i2c_port(1).pioc   := BBS.embed.GPIO.Due.PIOA'Access;
      i2c_port(1).sda_pin := 17;
      i2c_port(1).scl_pin := 18;
      i2c_port(1).int_id  := Ada.Interrupts.Names.TWI0_Interrupt;
      i2c_port(1).handle  := buff(1);
      buff1.set_interface(i2c_1'Access);
      --
      pins    := 2**Natural(i2c_port(chan).sda_pin) or 2**Natural(i2c_port(chan).scl_pin);
      --
      --  Initialize hardware for selected device.
      --
      --  Enable clock for I2C-x
      --
      SAM3x8e.PMC.PMC_Periph.PMC_PCER0.PID.Arr(Integer(i2c_port(chan).dev_id)) := 1;
      --
      --  Configure pins PB12 and PB13 to be I2C pins.
      --
      --  PER
      --  PDR
      i2c_port(chan).pioc.PDR.Val := SAM3x8e.UInt32(pins);
      --  OER
      --  ODR
      i2c_port(chan).pioc.OER.Val := SAM3x8e.UInt32(pins);
      --  IFER
      --  IFDR
      i2c_port(chan).pioc.IFDR.Val := SAM3x8e.UInt32(pins);
      --  SODR
      --  CODR
      --  IER
      --  IDR
      i2c_port(chan).pioc.IDR.Val := SAM3x8e.UInt32(pins);
      --  MDER
      --  MDDR
      i2c_port(chan).pioc.MDDR.Val := SAM3x8e.UInt32(pins);
      --  PUDR
      --  PUER
      i2c_port(chan).pioc.PUER.Val := SAM3x8e.UInt32(pins);
      --  ABSR -  really needed since both TWI are function A
      i2c_port(chan).pioc.ABSR.Arr(Integer(i2c_port(chan).sda_pin)) := 0;
      i2c_port(chan).pioc.ABSR.Arr(Integer(i2c_port(chan).scl_pin)) := 0;
      --  OWER
      --  OWDR
      i2c_port(chan).pioc.OWDR.Val := SAM3x8e.UInt32(pins);
      --
      --  Do whatever configuration is needed to configure the I2C controller.
      --
      --
      --  Set TWI clock for 100kHz
      --  The system clock is 84_000_000Hz
      --  The I2C clock is       100_000Hz
      --  This gives 840 system clocks per I2C clock.
      --
      --  The clock rate is set by three parameters: CLDIV, CHDIV, and CKDIV.
      --  Instead of specifying a clock rate, these specify the high time and
      --  the low time for the I2C clock.  The times are given by (note that
      --  the wave is symmetrical so Tlow = Thigh, meaning that CLDIV = CHDIV):
      --    Tlow  = ((CLDIV * 2^CKDIV) + 4) * Tmck
      --  Where:
      --    Tmch is the master clock period
      --  This should reduce to:
      --    420 = (CLDIV * 2^CKDIV) + 4
      --  or
      --    416 = CLDIV * 2^CKDIV
      --  or
      --    CLDIV = 416/(2^CKDIV)
      --
      --    CLDIV and CHDIV occupy 8 bits and this are limited to 0 - 255
      --    CKDIV is 3 bits and is limited to 0 - 7.
      --  So, we can get:
      --    CLDIV = CHDIV = 104
      --    CKDIV = 2
      --
      if speed = high400 then
         i2c_port(chan).port.CWGR.CKDIV := 0;
      else
         i2c_port(chan).port.CWGR.CKDIV := 2;
      end if;
      i2c_port(chan).port.CWGR.CLDIV := 104;
      i2c_port(chan).port.CWGR.CHDIV := 104;
      --
      --  Enable master mode
      --
      i2c_port(chan).port.CR.MSEN  := 1;  --  Enable master mode
      i2c_port(chan).port.CR.SVDIS := 1;  --  Disable slave mode
      --
      --  Set channel not busy
      --
      Ada.Synchronous_Task_Control.Set_True(i2c_port(chan).not_busy);
   end init;
   --
   --  Non-object oriented interface.
   --
   --  Routines to read and write data on the i2c bus.  These are based on the
   --  flowcharts in the datasheet.  The read routines are interrupt driven.
   --  The write routine is still partially polled.  It will eventually be
   --  converted to interrupt driven and a block write added.
   --
   procedure write(chan : port_id; addr : addr7; reg : uint8;
                   data : uint8; error : out err_code) is
      status : SAM3x8e.TWI.TWI0_SR_Register;
--      start : Ada.Real_Time.Time;
   begin
      if (addr < 16#0E#) or (addr > 16#77#) then
         error := invalid_addr;
         return;
      end if;
      Ada.Synchronous_Task_Control.Suspend_Until_True(i2c_port(chan).not_busy);
      i2c_port(chan).port.CR.MSEN    := 1;  --  Enable master mode
      i2c_port(chan).port.CR.SVDIS   := 1;  --  Disable slave mode
      i2c_port(chan).port.MMR.MREAD  := 0;  --  Master write
      i2c_port(chan).port.MMR.IADRSZ := SAM3x8e.TWI.Val_1_Byte;  --  Register addresses are 1 byte;
      i2c_port(chan).port.MMR.DADR   := SAM3x8e.UInt7(addr);
      i2c_port(chan).port.IADR.IADR  := SAM3x8e.UInt24(reg);
      i2c_port(chan).port.THR.TXDATA := SAM3x8e.Byte(data);
      i2c_port(chan).port.CR.STOP    := 1;
--      start := Ada.Real_Time.Clock;
      loop
         status := i2c_port(chan).port.SR;
         exit when status.TXRDY = 1;
         exit when status.NACK  = 1;
         exit when status.OVRE  = 1;
--         exit when (Ada.Real_Time.Clock - start) > Ada.Real_Time.To_Time_Span(0.1);
      end loop;
      if status.NACK = 1 then
         error := nack;
      elsif status.OVRE = 1 then
         error := ovre;
      else
         error := none;
      end if;
      Ada.Synchronous_Task_Control.Set_True(i2c_port(chan).not_busy);
   end;
   --
   function read(chan : port_id; addr : addr7; reg : uint8;
                 error : out err_code) return uint8 is
   begin
      read(chan, addr, reg, 1, error);
      return  uint8(i2c_port(chan).b(0));
   end;
   --
   --  When reading or writing two bytes, is the MSB first or second?  There is
   --  no standard even within a single device.
   --
   -- Read a word with MSB first
   --
   function readm1(chan : port_id; addr : addr7; reg : uint8;
                   error : out err_code) return UInt16 is
   begin
      read(chan, addr, reg, 2, error);
      return  UInt16(i2c_port(chan).b(0))*256 + UInt16(i2c_port(chan).b(1));
   end;

   --
   -- Read a word with MSB second (LSB first)
   --
   function readm2(chan : port_id; addr : addr7; reg : uint8;
                   error : out err_code) return UInt16 is
   begin
      read(chan, addr, reg, 2, error);
      return UInt16(i2c_port(chan).b(1))*256 + UInt16(i2c_port(chan).b(0));
   end;
   --
   -- Read the specified number of bytes into the device buffer
   --
   procedure read(chan : port_id; addr : addr7; reg : uint8;
                  size : buff_index; error : out err_code) is
      count  : buff_index := 0;
   begin
      if (addr < 16#0E#) or (addr > 16#77#) then
         error := invalid_addr;
         return;
      end if;
      Ada.Synchronous_Task_Control.Suspend_Until_True(i2c_port(chan).not_busy);
      buff(chan).rx_read(addr, reg, size);
      Ada.Synchronous_Task_Control.Suspend_Until_True(i2c_port(chan).not_busy);
      error := buff(chan).get_error;
      Ada.Synchronous_Task_Control.Set_True(i2c_port(chan).not_busy);
   end;
   --
   --
   --  Object oriented interface
   --
   --
   --  Write a byte to a specified register on an I2C device.
   --
   procedure write(self : in out due_i2c_interface_record; addr : addr7; reg : uint8;
                   data : uint8; error : out err_code) is
      status : SAM3x8e.TWI.TWI0_SR_Register;
   begin
      if (addr < 16#0E#) or (addr > 16#77#) then
         error := invalid_addr;
         return;
      end if;
      Ada.Synchronous_Task_Control.Suspend_Until_True(self.not_busy);
      self.port.CR.MSEN    := 1;  --  Enable master mode
      self.port.CR.SVDIS   := 1;  --  Disable slave mode
      self.port.MMR.MREAD  := 0;  --  Master write
      self.port.MMR.IADRSZ := SAM3x8e.TWI.Val_1_Byte;  --  Register addresses are 1 byte;
      self.port.MMR.DADR   := SAM3x8e.UInt7(addr);
      self.port.IADR.IADR  := SAM3x8e.UInt24(reg);
      self.port.THR.TXDATA := SAM3x8e.Byte(data);
      self.port.CR.STOP    := 1;
      loop
         status := self.port.SR;
         exit when status.TXRDY = 1;
         exit when status.NACK = 1;
         exit when status.OVRE = 1;
      end loop;
      if status.NACK = 1 then
         error := nack;
      elsif status.OVRE = 1 then
         error := ovre;
      else
         error := none;
      end if;
      Ada.Synchronous_Task_Control.Set_True(self.not_busy);
   end write;
   --
   --  Write an arbitrary number of bytes to a device on the i2c bus.
   --
   procedure write(self : in out due_i2c_interface_record; addr : addr7; reg : uint8;
                   size : buff_index; error : out err_code) is
      status : SAM3x8e.TWI.TWI0_SR_Register;
      index : buff_index := 0;
   begin
      if (addr < 16#0E#) or (addr > 16#77#) then
         error := invalid_addr;
         return;
      end if;
      Ada.Synchronous_Task_Control.Suspend_Until_True(self.not_busy);
      while index < size loop
         self.port.CR.MSEN    := 1;  --  Enable master mode
         self.port.CR.SVDIS   := 1;  --  Disable slave mode
         self.port.MMR.MREAD  := 0;  --  Master write
         self.port.MMR.IADRSZ := SAM3x8e.TWI.Val_1_Byte;  --  Register addresses are 1 byte;
         self.port.MMR.DADR   := SAM3x8e.UInt7(addr);
         self.port.IADR.IADR  := SAM3x8e.UInt24(reg);
         self.port.THR.TXDATA := SAM3x8e.Byte(self.b(index));
         index := index + 1;
         if index = size then
            self.port.CR.STOP    := 1;
         end if;
         loop
            status := self.port.SR;
            exit when status.TXRDY = 1;
            exit when status.NACK = 1;
            exit when status.OVRE = 1;
         end loop;
         if status.NACK = 1 then
            error := nack;
         elsif status.OVRE = 1 then
            error := ovre;
         else
            error := none;
         end if;
      end loop;
      Ada.Synchronous_Task_Control.Set_True(self.not_busy);
   end;
   --
   --  All the read functions use the block read procedure and return the
   --  specified data.
   --
   function read(self : in out due_i2c_interface_record; addr : addr7; reg : uint8;
                 error : out err_code) return uint8 is
   begin
      self.read(addr, reg, 1, error);
      return  self.b(0);
   end read;
   --
   -- When reading two bytes, is the MSB first or second?  There is no standard
   -- even within a single device.
   --
   -- Read a word with MSB first
   --
   function readm1(self : in out due_i2c_interface_record; addr : addr7; reg : uint8;
                   error : out err_code) return UInt16 is
   begin
      self.read(addr, reg, 2, error);
      return  UInt16(self.b(0))*256 + UInt16(self.b(1));
   end;
   --
   -- Read a word with MSB second (LSB first)
   --
   function readm2(self : in out due_i2c_interface_record; addr : addr7; reg : uint8;
                   error : out err_code) return UInt16 is
   begin
      self.read(addr, reg, 2, error);
      return  UInt16(self.b(1))*256 + UInt16(self.b(0));
   end;
   --
   --  Write a word with MSB first.
   --
   procedure writem1(self : in out i2c_interface_record; addr : addr7; reg : uint8;
                     data : uint16; error : out err_code) is
   begin
      self.b(0) := uint8((data/256) and 16#FF#);
      self.b(1) := uint8(data and 16#FF#);
      self.write(addr, reg, buff_index(2), error);
   end;
   --
   --  Write a word with MSB second (LSB first).
   --
   procedure writem2(self : in out i2c_interface_record; addr : addr7; reg : uint8;
                   data : uint16; error : out err_code) is
   begin
      self.b(0) := uint8(data and 16#FF#);
      self.b(1) := uint8((data/256) and 16#FF#);
      self.write(addr, reg, buff_index(2), error);
   end;
   --
   -- Read the specified number of bytes into a buffer
   --
   procedure read(self : in out due_i2c_interface_record; addr : addr7; reg : uint8;
                  size : buff_index; error : out err_code) is
      status : SAM3x8e.TWI.TWI0_SR_Register;
   begin
      if (addr < 16#0E#) or (addr > 16#77#) then
         error := invalid_addr;
         return;
      end if;
      Ada.Synchronous_Task_Control.Suspend_Until_True(self.not_busy);
      if size < 2 then
         self.handle.rx_read(addr, reg, 2);
      else
         self.handle.rx_read(addr, reg, size);
      end if;
      status := self.handle.get_status;
      Ada.Synchronous_Task_Control.Suspend_Until_True(self.not_busy);
      status := self.handle.get_saved_status;
      error := self.handle.get_error;
      Ada.Synchronous_Task_Control.Set_True(self.not_busy);
   end read;
   --
   --  -------------------------------------------------------------------------
   --  A protected type defining the transmit and receive buffers as well as an
   --  interface to the buffers.  This is based on the serial port handler, but
   --  is a bit simpler since (a) tx and rx is not simultaneous, so only one
   --  buffer is needed, and (b) communications are more transaction/block
   --  oriented so the user only needs to be notified when the exchange is
   --  completed.
   --
   protected body handler is
      --
      --  Set the address to the device record.  This only needs to be called
      --  once during initialization/configuration.
      --
      procedure set_interface(d : due_i2c_interface) is
      begin
         device := d;
      end;
      --
      --  Functions to return statuses
      --
      function is_busy return Boolean is
      begin
         return not not_busy;
      end;
      --
      function get_status return SAM3x8e.TWI.TWI0_SR_Register is
      begin
         return device.port.SR;
      end;
      --
      --  Entry point to transmit a character.  Per Ravenscar, there can be
      --  only one entry.
      --
      entry send(addr : addr7; reg : uint8; size : buff_index) when not_busy is
      begin
         not_busy := False;
         bytes := size;
         index := 0;
      end;
      --
      --  Procedure to read a specified number of characters into a buffer.
      --  Calls to this procedure need to be synchronized using
      --  susp_not_busy.
      --
      procedure rx_read(addr : addr7; reg : uint8; size : buff_index) is
      begin
         not_busy := False;
         err := none;
         bytes := size;
         complete := False;
         index := 0;
         --
         --  Disable interrupts
         --
         device.port.IDR.TXRDY := 1;
         device.port.IDR.SVACC := 1;
         device.port.IDR.GACC := 1;
         device.port.IDR.OVRE := 1;
         device.port.IDR.NACK := 1;
         device.port.IDR.ARBLST := 1;
         device.port.IDR.SCL_WS := 1;
         device.port.IDR.ENDRX := 1;
         device.port.IDR.ENDTX := 1;
         device.port.IDR.RXBUFF := 1;
         device.port.IDR.TXBUFE := 1;
         --
         --  Enable interrupts
         --
         device.port.IER.RXRDY  := 1;
         device.port.IER.TXCOMP := 1;
         --
         device.port.CR.MSEN    := 1;  --  Enable master mode
         device.port.CR.SVDIS   := 1;  --  Disable slave mode
         device.port.MMR.MREAD  := 1;  --  Master read
         device.port.MMR.IADRSZ := SAM3x8e.TWI.Val_1_Byte;  --  Register addresses are 1 byte;
         device.port.MMR.DADR   := SAM3x8e.UInt7(addr);
         device.port.IADR.IADR  := SAM3x8e.UInt24(reg);
         device.port.CR.START   := 1;
         if size = 1 then
            device.port.CR.STOP := 1;
         end if;
      end;
      --
      -- Return the error code, if any.
      --
      function get_error return err_code is
      begin
         return err;
      end;
      function get_complete return Boolean is
      begin
         return complete;
      end;
      function get_saved_status return SAM3x8e.TWI.TWI0_SR_Register is
      begin
         return status;
      end;
      --
      --  This is the interrupt handler.  There are three different things that
      --  may cause an interrupt:
      --  Transmitter ready:  Currently does nothing.
      --
      --  Receiver ready:  Add characters to the buffer.
      --
      --  Transmit complete:
      --
      procedure int_handler is
      begin
         status := device.port.SR;
         if status.NACK = 1 then
            err := nack;
         elsif status.OVRE = 1 then
            err := ovre;
         end if;
         --
         --  Currently transmit ready does nothing.
         --
         if status.TXRDY = 1 then
            null;
         end if;
         --
         --  Check for receiver ready and add new data to the buffer.
         --
         if status.RXRDY = 1 then
            device.b(index) := uint8(device.port.RHR.RXDATA);
            index := index + 1;
            if index = bytes then
               device.port.CR.STOP := 1;
               Ada.Synchronous_Task_Control.Set_True(device.not_busy);
            end if;
         end if;
         --
         --  Check for transmitter empty.  From the flowcharts in the datasheet,
         --  this interrupt should also fire at the end of a read operation.
         --
         if (status.TXCOMP = 1) then
            device.port.IDR.TXCOMP := 1;
            device.port.IDR.RXRDY := 1;
            device.port.IDR.OVRE := 1;
            device.port.IDR.NACK := 1;
            Ada.Synchronous_Task_Control.Set_True(device.not_busy);
            not_busy := True;
            complete := True;
         end if;
      end int_handler;
   end handler;
   --

end bbs.embed.i2c.due;
