with Ada.Interrupts.Names;
with Ada.Synchronous_Task_Control;
with System;
with SAM3x8e;
use type SAM3x8e.Bit;
use type SAM3x8e.Byte;
use type SAM3x8e.UInt32;
with SAM3x8e.UART;
with BBS.embed.GPIO.Due;
use type BBS.embed.GPIO.Due.pio_access;
--
--  This is an interrupt driven serial package that can be used to print
--  text with reduced overhead for the user code.  Characters are written
--  to a buffer which is sent to the UART under control of interrupts.
--
--  An even more processor efficient option would be to use DMA for handling
--  the I/O.  This is left for a future project.
--
--  There are two goals for this driver.  The first is to provide console I/O
--  for a person to communicate with the device.  The second is to be able to
--  communicate with other devices.  There are many features that could be
--  added, but it should be kept fairly simple and primitive.
--
package BBS.embed.due.serial.int is
   --
   --  Serial port object
   --
   type serial_port_record is tagged limited private;
   type serial_port is access all serial_port_record;
   --
   --  Function to initialize and return a serial port object
   --
   function init(c : port_id; baud : SAM3x8e.UInt32) return serial_port;
   --
   --  Function to return an object for a specific serial port
   --
   function get_port(c : port_id) return serial_port;
   --
   --  Procedure to write a character to a serial port.
   --
   procedure put(c : Character);
   procedure put(chan : port_id; c : Character);
   procedure put(self : not null access serial_port_record'class; c : Character);
   --
   --  Procedure to put a string to the serial port
   --
   procedure put(s : string);
   procedure put(chan : port_id; s : string);
   procedure put(self : not null access serial_port_record'class; s : String);
   --
   --  Procedure to put a string to the serial port followed by a CR/LF
   --
   procedure put_line(s : string);
   procedure put_line(chan : port_id; s : string);
   procedure put_line(self : not null access serial_port_record'class; s : String);
   --
   --  Procedure to write a new line to the serial port
   --
   procedure new_line;
   procedure new_line(chan : port_id);
   procedure new_line(self : not null access serial_port_record'class);
   --
   --  Procedure to enable RS-485 mode on an I/O channel.  It requires an
   --  initialized digital I/O pin record.  If d.ctrl isn't pointing to a
   --  PIO control record, bad things can happen, so make this a precondition.
   --
   procedure enable_rs485(chan : port_id; d : BBS.embed.GPIO.Due.Due_GPIO_ptr)
     with pre => ((d.ctrl = BBS.embed.GPIO.Due.PIOA'Access) or (d.ctrl = BBS.embed.GPIO.Due.PIOB'Access) or
                      (d.ctrl = BBS.embed.GPIO.Due.PIOC'Access) or (d.ctrl = BBS.embed.GPIO.Due.PIOD'Access));
   procedure enable_rs485(self : not null access serial_port_record'class; d : BBS.embed.GPIO.Due.Due_GPIO_ptr)
     with pre => ((d.ctrl = BBS.embed.GPIO.Due.PIOA'Access) or (d.ctrl = BBS.embed.GPIO.Due.PIOB'Access) or
                      (d.ctrl = BBS.embed.GPIO.Due.PIOC'Access) or (d.ctrl = BBS.embed.GPIO.Due.PIOD'Access));
   --
   --  Wait until transmit buffer is empty.  Since the Ravenscar profile doesn't
   --  allow more than one entry in a protected object, look into using
   --  suspension_objects from Ada.Synchronous_Task_Control.
   --
   procedure flush(chan : port_id);
   procedure flush(self : not null access serial_port_record'class);
   --
   --  Enable or disable rx interrupt.
   --
   procedure rx_enable(chan : port_id; b : Boolean);
   procedure rx_enable(self : not null access serial_port_record'class; b : Boolean);
   --
   --  Check to see if characters are available in the buffer
   --
   function rx_ready return Boolean;
   function rx_ready(chan : port_id) return Boolean;
   function rx_ready(self : not null access serial_port_record'class) return Boolean;
   --
   --  Read a character from the buffer.
   --
   function get return Character;
   function get(chan : port_id) return Character;
   function get(self : not null access serial_port_record'class) return Character;
   --
   -- Return the next character in the receive buffer without removing it
   --
   function peek return Character;
   function peek(chan : port_id) return Character;
   function peek(self : not null access serial_port_record'class) return Character;
   --
   --  Return a line of text.
   --
   procedure get_line(s : in out String; l : out Integer);
   procedure get_line(chan : port_id; s : in out String; l : out Integer);
   procedure get_line(self : not null access serial_port_record'class;
                      s : in out String; l : out Integer);
   --
   -- Procedures to control configuration settings
   --
   procedure set_echo(chan : port_id; b : Boolean);
   procedure set_del(chan : port_id; b : Boolean);
   procedure set_echo(self : not null access serial_port_record'class; b : Boolean);
   procedure set_del(self : not null access serial_port_record'class; b : Boolean);


private
   --
   --  Some configuration values.
   --
--   rx_echo : array (port_id'Range) of Boolean := (True, False,
--                                                           False, False);
   tx_eol  : constant String := CR & LF;
--   rx_del_enable : array (port_id'Range) of Boolean :=
--     (True, False, False, False);
   --
   --  Declare types for the transmit buffers.  This size can be adjusted as
   --  needed.
   --
   type tx_buff_ptr is mod 2**8;
   type tx_buff_type is array (tx_buff_ptr'Range) of SAM3x8e.Byte;
   --
   --  Declare types for the receive buffers.  This size can be adjusted as
   --  needed.
   --
   type rx_buff_ptr is mod 2**8;
   type rx_buff_type is array (rx_buff_ptr'Range) of SAM3x8e.Byte;

   --
   --  A protected type defining the transmit and receive buffers as well as an
   --  interface to the buffers.  This also includes an interrupt handler to
   --  communicate with the U/SART.
   --
   protected type buffer(int_id : Ada.Interrupts.Interrupt_ID) is
      --
      --  Functions to return statuses
      --
      function tx_buffer_full return Boolean;
      function tx_complete return Boolean;
      --
      --  Entry point to transmit a character.  Per Ravenscar, there can be
      --  only one entry.
      --
      entry tx_write(c : Character);
      --
      --  Procedure to reset the receive buffer.
      --
      procedure rx_clear;
      --
      --  Procedure to read a character from the receive buffer.  Calls to this
      --  procedure need to be synchronized using susp_rx_buff_not_empty.
      --
      procedure rx_read(c : out Character);
      --
      --  Return the next character from the buffer, but don;t remove it from
      --  the buffer.  This also needs to be synchronized using
      --  susp_rx_buff_not_empty.
      --
      procedure rx_peek(c : out Character);
      --
      --  Enable or disable the RX interrupt
      --
      procedure set_rx_int(b : Boolean);
      --
      --  Procedure to enable RS-485 mode.
      --
      procedure enable_rs485(d : BBS.embed.GPIO.Due.Due_GPIO_ptr);
      --
      --  Procedure to initialize some things
      --
      procedure init(p : serial_port);
   private
      procedure int_handler;
      pragma Attach_Handler (int_handler, int_id);
      pragma Interrupt_Priority(System.Interrupt_Priority'First);

      rs485_mode : Boolean := False;
      rs485_pin  : BBS.embed.GPIO.Due.Due_GPIO_ptr;

      tx_buff_empty    : Boolean := True;
      tx_buff_not_full : Boolean := True;
      tx_fill_ptr      : tx_buff_ptr := 0;
      tx_empty_ptr     : tx_buff_ptr := 0;
      tx_buff          : tx_buff_type;

      rx_fill_ptr      : rx_buff_ptr := 0;
      rx_empty_ptr     : rx_buff_ptr := 0;
      rx_buff          : rx_buff_type;

      s : serial_port;
   end buffer;
   type buffer_access is access all buffer;

   --
   --  Declare a buffer for each serial port
   --
   buff0 : aliased buffer(channel(0).int_id);
   buff1 : aliased buffer(channel(1).int_id);
   buff2 : aliased buffer(channel(2).int_id);
   buff3 : aliased buffer(channel(3).int_id);
   --
   --  An array of the buffers so that the I/O routines can access a buffer by
   --  the port ID.
   --
   buff : array (port_id'Range) of buffer_access :=
     (buff0'access, buff1'access, buff2'access, buff3'access);
   --
   --  Serial port object
   --
   --
   --  Since the Ravenscar profile allows only one entry barrier per protected
   --  objects, use some suspension objects to accomplish the same purpose.
   --
   type serial_port_record is tagged limited record
      b             : buffer_access;
      tx_empty      : Ada.Synchronous_Task_Control.Suspension_Object;
      rx_not_empty  : Ada.Synchronous_Task_Control.Suspension_Object;
      hardware      : serial_obj;
      rx_echo       : Boolean;
      rx_del_enable : Boolean;
   end record;
   --
   --  Declare the port objects for each port
   --
   port0 : aliased serial_port_record;
   port1 : aliased serial_port_record;
   port2 : aliased serial_port_record;
   port3 : aliased serial_port_record;
   port_list : array (port_id'Range) of serial_port := (port0'Access, port1'Access,
                                                        port2'Access, port3'Access);
end BBS.embed.due.serial.int;

