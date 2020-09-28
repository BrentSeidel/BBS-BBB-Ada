with Ada.Interrupts;
with Ada.Interrupts.Names;
with Ada.Real_Time;
use type Ada.Real_Time.Time;
use type Ada.Real_Time.Time_Span;
with Ada.Synchronous_Task_Control;
with System;
with SAM3x8e.TWI;
with BBS.embed.GPIO.Due;
with BBS.embed.due.dev;
--
--  Concrete package for I2C interfaces on the Arduino Due.  This inherits from
--  the base class and adds the actual functionality to implement the interface.
--
--  Eventually, any dependecies on the SAM3x8e package tree should be eliminated.
--
--  The Arduino Due has two I2C interfaces.
--  Interface  SCL  SDA   TWI
--  I2C-0      PB13 PB12  TWI0
--  I2C-1      PA18 PA17  TWI1
--
package bbs.embed.i2c.due is
   --
   --  Interface speed, 100kHz and 400kHz are supported.
   --
   type speed_type is (low100, high400);
   --
   --  Port ID is 0 or 1
   --
   type port_id is  new Integer range 0 .. 1;
   --
   --  The I2C device object
   --
   type due_i2c_interface_record is new i2c_interface_record with private;
   type due_i2c_interface is access all due_i2c_interface_record'Class;
   --
   --  Function to return access to a device record.
   --
   function get_interface(d : port_id) return due_i2c_interface;
   --
   --  Initialize interface I2C-0 on the Arduino (turns out to be TWI1
   --  internally)
   --
   procedure init(chan : port_id; speed : speed_type);
   --
   --  Routines to read and write data on the i2c bus
   --
   procedure write(chan : port_id; addr : addr7; reg : uint8;
                   data : uint8; error : out err_code);
   function read(chan : port_id; addr : addr7; reg : uint8;
                 error : out err_code) return uint8;
   --
   --  Reading a single byte is straightforward.  When reading two bytes, is the
   --  MSB first or second?  There is no standard even within a single device.
   --
   --  Read a word with MSB first
   --
   function readm1(chan : port_id; addr : addr7; reg : uint8;
                   error : out err_code) return UInt16;
   --
   -- Read a word with MSB second (LSB first)
   --
   function readm2(chan : port_id; addr : addr7; reg : uint8;
                   error : out err_code) return UInt16;
   --
   -- Read the specified number of bytes into a buffer
   --
   procedure read(chan : port_id; addr : addr7; reg : uint8;
                  size : buff_index; error : out err_code);
   --
   --  Object oriented interface
   --
   overriding
   procedure write(self : in out due_i2c_interface_record; addr : addr7; reg : uint8;
                   data : uint8; error : out err_code);
   --
   overriding
   function read(self : in out due_i2c_interface_record; addr : addr7; reg : uint8;
                 error : out err_code) return uint8;
   --
   --  When reading or writing two bytes, is the MSB first or second?  There is
   --  no standard even within a single device.
   --
   --  Read a word with MSB first
   --
   overriding
   function readm1(self : in out due_i2c_interface_record; addr : addr7; reg : uint8;
                 error : out err_code) return UInt16;
   --
   --  Read a word with MSB second (LSB first)
   --
   overriding
   function readm2(self : in out due_i2c_interface_record; addr : addr7; reg : uint8;
                 error : out err_code) return UInt16;
   --
   --  Write an arbitrary number of bytes to a device on the i2c bus.
   --
   procedure write(self : in out due_i2c_interface_record; addr : addr7; reg : uint8;
                   size : buff_index; error : out err_code);
   --
   --  Read the specified number of bytes into a buffer
   --
   overriding
   procedure read(self : in out due_i2c_interface_record; addr : addr7; reg : uint8;
                  size : buff_index; error : out err_code);
private
   --
   --  Delay time for I2C bus to clear between transactions.  On occasion, without
   --  a delay the bus will lock up with back to back transaction.  This may be
   --  more a problem with the I2C devices than with the processor since resetting
   --  the processor doesn't clear the problem.  Power also needs to be cycled
   --  on the I2C devices.
   --
   --  The delay time was emperically determined.  A value of 0.000_002 causes
   --  a lockup.  A value of 0.000_003 works.
   --
   i2c_delay : constant Ada.Real_Time.Time_Span := Ada.Real_Time.To_Time_Span(0.000_003);
   --
   --  Addresses for TWI records
   --
   TWI0 : aliased SAM3x8e.TWI.TWI_Peripheral
     with Import, Address => SAM3x8e.TWI0_Base;
   --
   TWI1 : aliased SAM3x8e.TWI.TWI_Peripheral
     with Import, Address => SAM3x8e.TWI1_Base;
   --
   --  A protected type defining the transmit and receive buffers as well as an
   --  interface to the buffers.  This is based on the serial port handler, but
   --  is a bit simpler since (a) tx and rx is not simultaneous, so only one
   --  buffer is needed, and (b) communications are more transaction/block
   --  oriented so the user only needs to be notified when the exchange is
   --  completed.
   --
   protected type handler(interrupt : Ada.Interrupts.Interrupt_ID) is
      --
      --  Set the address to the device record.  This only needs to be called
      --  once during initialization/configuration.
      --
      procedure set_interface(d : due_i2c_interface);
      --
      --  Functions to return statuses
      --
      function is_busy return Boolean;
      function get_status return SAM3x8e.TWI.TWI0_SR_Register;
      --
      --  Entry point to transmit a character.  Per Ravenscar, there can be
      --  only one entry.  This is not yet implemented.
      --
      entry send(addr : addr7; reg : uint8; size : buff_index);
      --
      --  Procedure to read a specified number of characters into a buffer.
      --  Calls to this procedure need to be synchronized using
      --  susp_not_busy.
      --
      procedure rx_read(addr : addr7; reg : uint8; size : buff_index);
      --
      -- Return various information.
      --
      function get_error return err_code;
      function get_complete return Boolean;
      function get_saved_status return SAM3x8e.TWI.TWI0_SR_Register;
   private
      procedure int_handler;
      pragma Attach_Handler (int_handler, interrupt);
      pragma Interrupt_Priority(System.Interrupt_Priority'First);

      device   : due_i2c_interface;

      not_busy : Boolean := True;
      complete : Boolean := False;
      status : SAM3x8e.TWI.TWI0_SR_Register;

      bytes  : buff_index;
      index  : buff_index;

      err : err_code;
   end handler;
   --
   --  Declare a handler for each i2c port
   --
   buff0 : aliased handler(Ada.Interrupts.Names.TWI1_Interrupt);
   buff1 : aliased handler(Ada.Interrupts.Names.TWI0_Interrupt);
   --
   --  An array of the interrupt handlers so that the I/O routines can access a
   --  handler by the port ID.
   --
   type buffer_access is access all handler;
   buff : array (port_id'Range) of buffer_access :=
     (buff0'access, buff1'access);
   --
   --  Create a serial channel information record.  This should contain all
   --  the information necessary to identify a serial port and the pins used
   --  by it.
   --
   type twi_access is access all SAM3x8e.TWI.TWI_Peripheral;
   type due_i2c_interface_record is new i2c_interface_record with
      record
         dev_id   : uint8;           --  TWI device ID
         port     : twi_access;      --  Access to I2C registers
         pioc     : BBS.embed.GPIO.Due.pio_access; --  PIO controlling pins
         sda_pin  : uint8;           --  SDA pin on PIO
         scl_pin  : uint8;           --  SCL pin on PIO
         int_id   : Ada.Interrupts.Interrupt_ID; -- Interrupt for channel
         handle   : buffer_access;
         not_busy : Ada.Synchronous_Task_Control.Suspension_Object;
      end record;
   --
   --  The Arduino Due has two I2C busses available on the headers.  Note that
   --  the port numbers on the header are reversed from the internal hardware
   --  channel numbers.
   --
   i2c_0 : aliased due_i2c_interface_record;
   i2c_1 : aliased due_i2c_interface_record;
   i2c_port : array (port_id'Range) of due_i2c_interface := (i2c_0'Access, i2c_1'Access);

end bbs.embed.i2c.due;
