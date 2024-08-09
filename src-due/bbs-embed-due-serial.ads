--
--  Author: Brent Seidel
--  Date: 9-Aug-2024
--
--  This file is part of bbs_embed.
--  Bbs_embed is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  bbs_embed is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with bbs_embed. If not, see <https://www.gnu.org/licenses/>.--
--
with SAM3x8e.UART;
with SAM3x8e;
use type SAM3x8e.Bit;
use type SAM3x8e.Byte;
use type SAM3x8e.UInt32;
with BBS.embed.due.dev;
with BBS.embed.GPIO.Due;
with Ada.Interrupts;
with Ada.Interrupts.Names;
--
--  This package is the root of the serial I/O functionality on the Arduino
--  Due.  It has four of its serial ports wired to the headers.  These ports
--  are numbered 0 through 3.  Port 0 is also wired to the programming USB
--  connector and is used as the default port, if none is specified.
--
--  Communications is available in both polled and interrupt driven varients.
--  Polled is unbuffered and has very little memory overhead, but puts more
--  load on the CPU.  It is most useful when memory is tight or serial I/O
--  needs are minimal.  It would also be useful as a method of last resort
--  for printing messages from an exception handler.
--
--  Interrupt driven copies messages into a buffer and uses an interrupt
--  handler to feed characters into the UART as needed.  The main thread of
--  software can continue processing while this is happening.  This provides
--  more processing for user software at the expense of requiring memory for
--  the buffers.
--
--  This package contains definitions and code that is common to both methods.
--  The interrupt or polled specific definitions and code are in the
--  appropriate sub-package.
--
package BBS.embed.due.serial is

   --
   --  The Arduino has four serial ports wired to the headers.
   --
   type port_id is new Integer range 0 .. 3;
   --
   --  The programming USB port is connected to pins PA08 and PA09 - URXD and
   --  UTXD.  These are controlled by the UART.  Include code here to
   --  initialize the UART and set the baud rate.
   --
   procedure init(baud_rate : SAM3x8e.UInt32);
   procedure init(chan_num : port_id; baud_rate : SAM3x8e.UInt32);
   --
   --  Check if the transmitter is ready
   --
   function tx_ready return Boolean;
   function tx_ready(chan : port_id) return Boolean;
   --
   --  Check if the receiver is ready
   --
   function rx_ready return Boolean;
   function rx_ready(chan : port_id) return Boolean;
   --
   --  Check if the transmitter is empty.
   --
   function tx_empty return Boolean;
   function tx_empty(chan : port_id) return Boolean;

private
   --
   --  Constants for some special characters.
   --
   BS : constant Character := Character'Val(8);
   LF : constant Character := Character'Val(10);
   CR : constant Character := Character'Val(13);
   DEL : constant Character := Character'Val(127);

   Serial : aliased SAM3x8e.UART.UART_Peripheral
     with Import, Address => SAM3x8e.UART_Base;
   --
   --  The USARTs seem to be a superset of the UART, so the following works,
   --  but leaves the more advanced features of the USARTs unavailable.
   --
   Serial0 : aliased SAM3x8e.UART.UART_Peripheral
     with Import, Address => SAM3x8e.USART0_Base;

   --  Universal Synchronous Asynchronous Receiver Transmitter 1
   Serial1 : aliased SAM3x8e.UART.UART_Peripheral
     with Import, Address => SAM3x8e.USART1_Base;

   --  Universal Synchronous Asynchronous Receiver Transmitter 2
   --  This does not seem to be available on the Arduino Due pin headers.
   Serial2 : aliased SAM3x8e.UART.UART_Peripheral
     with Import, Address => SAM3x8e.USART2_Base;

   --  Universal Synchronous Asynchronous Receiver Transmitter 3
   Serial3 : aliased SAM3x8e.UART.UART_Peripheral
     with Import, Address => SAM3x8e.USART3_Base;
   --
   --  Create a serial channel information record.  This should contain all
   --  the information necessary to identify a serial port and the pins used
   --  by it.
   --
   type serial_access is access all SAM3x8e.UART.UART_Peripheral;
   type channel_info_rec is tagged limited record
      dev_id   : SAM3x8e.Byte;    --  UART device ID
      port     : serial_access;   --  Access to UART registers
      pioc     : BBS.embed.GPIO.Due.pio_access;  --  PIO controlling pins
      tx_pin   : SAM3x8e.Byte;    --  Transmit pin on PIO
      rx_pin   : SAM3x8e.Byte;    --  Receive pin on PIO
      tx_absel : SAM3x8e.Bit;     --  A/B selection on PIO for transmit
      rx_absel : SAM3x8e.Bit;     --  A/B selection on PIO for receive
      int_id   : Ada.Interrupts.Interrupt_ID; -- Interrupt for channel
   end record;
   type serial_obj is access constant channel_info_rec;
   --
   -- May need a serial channel for the USB port - TODO
   --

   --
   --  The Arduino Due has four serial channels wired to the headers.
   --  0 - UART
   --  1 - USART0
   --  2 - USART1
   --  3 - USART3
   --
   chan0 : aliased constant channel_info_rec := (dev_id => dev.UART_ID, port => Serial'Access,
                                         pioc => BBS.embed.GPIO.Due.PIOA'Access, tx_pin => 9,
                                         rx_pin => 8, tx_absel => 0, rx_absel => 0,
                                         int_id => Ada.Interrupts.Names.UART_Interrupt);
   chan1 : aliased constant channel_info_rec := (dev_id => dev.USART0_ID, port => Serial0'Access,
                                         pioc => BBS.embed.GPIO.Due.PIOA'Access, tx_pin => 11,
                                         rx_pin => 10, tx_absel => 0, rx_absel => 0,
                                         int_id => Ada.Interrupts.Names.USART0_Interrupt);
   chan2 : aliased constant channel_info_rec := (dev_id => dev.USART1_ID, port => Serial1'Access,
                                         pioc => BBS.embed.GPIO.Due.PIOA'Access, tx_pin => 13,
                                         rx_pin => 12, tx_absel => 0, rx_absel => 0,
                                         int_id => Ada.Interrupts.Names.USART1_Interrupt);
   chan3 : aliased constant channel_info_rec := (dev_id => dev.USART3_ID, port => Serial3'Access,
                                         pioc => BBS.embed.GPIO.Due.PIOD'Access, tx_pin => 4,
                                         rx_pin => 5, tx_absel => 1, rx_absel => 1,
                                         int_id => Ada.Interrupts.Names.USART3_Interrupt);
   channel : constant array (port_id'Range) of serial_obj := (chan0'Access, chan1'Access,
                                                              chan2'Access, chan3'Access);
end BBS.embed.due.serial;
