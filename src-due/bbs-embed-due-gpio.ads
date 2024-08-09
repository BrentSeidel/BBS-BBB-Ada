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
with BBS.embed.GPIO.Due;
--
--  This package contains a collection of pin objects for the Arduino Pins.  If
--  memory is tight for your application, just copy out the ones that you need.
--  There does not seem to be much rhyme or reason for how the CPU pins are
--  assigned to the Arduino pins.
--
--  I will not claim that all of these have been tested.
--
package BBS.embed.Due.GPIO is
   --
   --  Arduino Due pins.  Note that many of these pins have multiple uses.
   --
   pin0_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOA'Access, bit => 8);
   pin1_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOA'Access, bit => 9);
   pin2_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOB'Access, bit => 25);
   pin3_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 28);
   --
   --  Arduino pin 4 seems to be PA29 and PC26 wired together.
   --
--   pin4_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOA'Access, bit => 29);
--   pin4 : BBS.embed.GPIO.Due.Due_GPIO_ptr := pin4_rec'Access;
--   pin4_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 26);
--   pin4 : BBS.embed.GPIO.Due.Due_GPIO_ptr := pin4_rec'Access;
   --
   pin5_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 25);
   pin6_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 24);
   pin7_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 23);
   pin8_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 22);
   pin9_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 21);
   pin10_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOA'Access, bit => 28);
   pin11_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 29);
   pin12_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOD'Access, bit => 7);
   pin13_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOD'Access, bit => 8);
   --
   --  LED pin is Arduino digital pin 14 (processor pin PB27)
   --
   pin14_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOB'Access, bit => 27);
   --
   --  More Arduino Due pins
   --
   pin15_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOD'Access, bit => 5);
   pin16_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOA'Access, bit => 13);
   pin17_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOA'Access, bit => 12);
   pin18_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOA'Access, bit => 11);
   pin19_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOA'Access, bit => 10);
   pin20_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOB'Access, bit => 12);
   pin21_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOB'Access, bit => 13);
   pin22_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOB'Access, bit => 26);
   pin23_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOA'Access, bit => 14);
   pin24_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOA'Access, bit => 15);
   pin25_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOD'Access, bit => 0);
   pin26_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOD'Access, bit => 1);
   pin27_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOD'Access, bit => 2);
   pin28_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOD'Access, bit => 3);
   pin29_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOD'Access, bit => 6);
   pin30_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOD'Access, bit => 9);
   pin31_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOA'Access, bit => 7);
   pin32_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOD'Access, bit => 10);
   pin33_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 1);
   pin34_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 2);
   pin35_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 3);
   pin36_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 4);
   pin37_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 5);
   pin38_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 6);
   pin39_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 7);
   pin40_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 8);
   pin41_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 9);
   pin42_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOA'Access, bit => 19);
   pin43_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOA'Access, bit => 20);
   pin44_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 19);
   pin45_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 18);
   pin46_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 17);
   pin47_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 16);
   pin48_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 15);
   pin49_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 14);
   pin50_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 13);
   pin51_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOC'Access, bit => 12);
   pin52_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOB'Access, bit => 21);
   pin53_rec : aliased BBS.embed.GPIO.Due.Due_GPIO_record := (ctrl => BBS.embed.GPIO.Due.PIOB'Access, bit => 14);
   --
   pin0  : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin0_rec'Access;
   pin1  : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin1_rec'Access;
   pin2  : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin2_rec'Access;
   pin3  : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin3_rec'Access;
   --
   pin4  : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := null;
   --
   pin5  : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin5_rec'Access;
   pin6  : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin6_rec'Access;
   pin7  : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin7_rec'Access;
   pin8  : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin8_rec'Access;
   pin9  : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin9_rec'Access;
   pin10 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin10_rec'Access;
   pin11 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin11_rec'Access;
   pin12 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin12_rec'Access;
   pin13 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin13_rec'Access;
   pin14 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin14_rec'Access;
   LED_PIN : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin14_rec'Access;
   pin15 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin15_rec'Access;
   pin16 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin16_rec'Access;
   pin17 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin17_rec'Access;
   pin18 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin18_rec'Access;
   pin19 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin19_rec'Access;
   pin20 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin20_rec'Access;
   pin21 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin11_rec'Access;
   pin22 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin22_rec'Access;
   pin23 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin23_rec'Access;
   pin24 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin24_rec'Access;
   pin25 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin25_rec'Access;
   pin26 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin26_rec'Access;
   pin27 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin27_rec'Access;
   pin28 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin28_rec'Access;
   pin29 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin29_rec'Access;
   pin30 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin30_rec'Access;
   pin31 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin31_rec'Access;
   pin32 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin32_rec'Access;
   pin33 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin33_rec'Access;
   pin34 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin34_rec'Access;
   pin35 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin35_rec'Access;
   pin36 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin36_rec'Access;
   pin37 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin37_rec'Access;
   pin38 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin38_rec'Access;
   pin39 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin39_rec'Access;
   pin40 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin40_rec'Access;
   pin41 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin41_rec'Access;
   pin42 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin42_rec'Access;
   pin43 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin43_rec'Access;
   pin44 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin44_rec'Access;
   pin45 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin45_rec'Access;
   pin46 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin46_rec'Access;
   pin47 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin47_rec'Access;
   pin48 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin48_rec'Access;
   pin49 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin49_rec'Access;
   pin50 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin50_rec'Access;
   pin51 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin51_rec'Access;
   pin52 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin52_rec'Access;
   pin53 : constant BBS.embed.GPIO.Due.Due_GPIO_ptr := pin53_rec'Access;
end BBS.embed.Due.GPIO;
