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
with SAM3x8e;
with SAM3x8e.PIO;
--
--  This package contains definitions and routines for the PIO controllers.  It
--  also has functions for setting and getting pin values.
--
package BBS.embed.GPIO.Due is
   --
   --  Access type to controller address
   --
   type pio_access is access all SAM3x8e.PIO.PIO_Peripheral;
   type direction is (gpio_input, gpio_output, funct_a, funct_b);
   --
   --  Definitions for object oriented pins
   --
   type Due_GPIO_record is new GPIO_record with
      record
         ctrl : pio_access;
         bit  : Integer range 0 .. 31;
      end record;
   type Due_GPIO_ptr is access all Due_GPIO_record'Class;
   --
   --  Configures a pin to be controlled by the PIO controller.  Output is
   --  enabled or disabled based on the value of dir.
   --
   procedure config(self : in out Due_GPIO_record;
                    pin : Due_GPIO_record; dir : direction);
   procedure config(self : in out Due_GPIO_record; dir : direction);
   --
   --  Set a pin to a high or low value.
   --
   overriding
   procedure set(self : Due_GPIO_record; val : Bit);
   --
   --  Read the value of a pin regardless of what is controlling it
   --
   overriding
   function get(self : Due_GPIO_record) return Bit;
   --
   --  Enable or disable pullup on a pin
   --
   procedure pullup(self : Due_GPIO_record; val : Bit);

   --  Parallel Input/Output Controller A
   PIOA : aliased SAM3x8e.PIO.PIO_Peripheral
     with Import, Address => SAM3x8e.PIOA_Base;

   --  Parallel Input/Output Controller B
   PIOB : aliased SAM3x8e.PIO.PIO_Peripheral
     with Import, Address => SAM3x8e.PIOB_Base;

   --  Parallel Input/Output Controller C
   PIOC : aliased SAM3x8e.PIO.PIO_Peripheral
     with Import, Address => SAM3x8e.PIOC_Base;

   --  Parallel Input/Output Controller D
   PIOD : aliased SAM3x8e.PIO.PIO_Peripheral
     with Import, Address => SAM3x8e.PIOD_Base;
   --
   --  LED pin is Arduino digital pin 14 (processor pin PB27)
   --
--   led_pin_obj : aliased Due_GPIO_record := (ctrl => PIOB'Access, bit => 27);
--   LED_PIN   : Due_GPIO_ptr := led_pin_obj'Access;
   --
   --  RS-485 controlled Arduino digital pin 22 (processor pin PB26)
   --
--   rs485_pin_rec : aliased Due_GPIO_record := (ctrl => PIOB'Access, bit => 26);
--   RS485_PIN : Due_GPIO_ptr := rs485_pin_rec'Access;
   --
   --  Arduino pin 23 used by toggle task.
   --
--   pin23_rec : aliased Due_GPIO_record := (ctrl => PIOA'Access, bit => 14);
--   pin23 : Due_GPIO_ptr := pin23_rec'Access;
private

end BBS.embed.GPIO.Due;
