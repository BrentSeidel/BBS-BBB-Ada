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
--with BBS.embed.GPIO.Due;
--
--  This is a very simple serial package that can be used to print some
--  debugging information.  It uses polling to wait for each character
--  to be passed to the UART.  This makes it not particularly efficient.
--  However, it will work even if the interrupts get all screwed up, which
--  makes it useful in a last resort exception handler to get a message
--  printed.
--
package BBS.embed.due.serial.polled is
   --
   --  Very simple procedure to write a character to the UART.  It does a
   --  busy wait on the UART_SR TXRDY (transmit ready) bit.  It does a loop
   --  until the value of the bit is 1 and then write the character.
   --
   procedure put(c : Character);
   procedure put(chan : port_id; c : Character);
   --
   --  Procedure to put a string to the serial port
   --
   procedure put(s : string);
   procedure put(chan : port_id; s : string);
   --
   --  Procedure to put a string to the serial port followed by a CR/LF
   --
   procedure put_line(s : string);
   procedure put_line(chan : port_id; s : string);
   --
   --  Read a character from serial port - wait for one to be present, if
   --  necessary.
   --
   function get return Character;
   function get(chan : port_id) return Character;

end BBS.embed.due.serial.polled;
