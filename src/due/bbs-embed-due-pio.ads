with SAM3x8e;
use type SAM3x8e.Bit;
with SAM3x8e.PIO;
--
--  This package contains definitions and routines for the PIO controllers.  It
--  also has functions for setting and getting pin values.
--
package BBS.embed.due.pio is
   type direction is (gpio_input, gpio_output, funct_a, funct_b);
   --
   --  Access type to controller address
   --
   type pio_access is access all SAM3x8e.PIO.PIO_Peripheral;
   --
   --  Definitions for object oriented pins
   --
   type gpio_record is tagged record
      ctrl : pio_access;
      bit  : Integer range 0 .. 31;
   end record;
   type gpio_ptr is access all gpio_record;
   --
   --  Record containing information to translate pin number to actual I/O
   --  signals.
   --

   --
   --  Configures a pin to be controlled by the PIO controller.  Output is
   --  enabled or disabled based on the value of dir.
   --
   procedure config(self : not null access gpio_record'class;
                    pin : gpio_record; dir : direction);
   procedure config(self : not null access gpio_record'class; dir : direction);
   --
   --  Set a pin to a high or low value.
   --
   procedure set(self : not null access gpio_record'class; val : SAM3x8e.Bit);
   --
   --  Read the value of a pin regardless of what is controlling it
   --
   function get(self : not null access gpio_record'class) return SAM3x8e.Bit;
   --
   --  Enable or disable pullup on a pin
   --
   procedure pullup(self : not null access gpio_record'class; val : SAM3x8e.Bit);

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
   led_pin_obj : aliased gpio_record := (ctrl => PIOB'Access, bit => 27);
   LED_PIN   : gpio_ptr := led_pin_obj'Access;
   --
   --  RS-485 controlled Arduino digital pin 22 (processor pin PB26)
   --
   rs485_pin_rec : aliased gpio_record := (ctrl => PIOB'Access, bit => 26);
   RS485_PIN : gpio_ptr := rs485_pin_rec'Access;
   --
   --  Arduino pin 23 used by toggle task.
   --
   pin23_rec : aliased gpio_record := (ctrl => PIOA'Access, bit => 14);
   pin23 : gpio_ptr := pin23_rec'Access;

end BBS.embed.due.pio;
