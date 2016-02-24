with Ada.Text_IO;
with BBS.BBB.pins;
package BBS.BBB.LED is
   --
   -- There is probably a better way to do this.  Some time it might get
   -- converted to object oriented Ada.
   --
   author : constant String := "Brent Seidel";
   version : constant String := "V00.01";
   --
   -- Some types for the LEDs
   --
   subtype led_num is Integer range 0 .. 3;
   subtype led_patterns is Integer range 0 .. 15;
   type led_state is (off, on);
   type led_states is array (led_num) of led_state;
   led_pattern : constant array (led_patterns) of led_states :=
     ((off, off, off, off), --  0
      (off, off, off,  on), --  1
      (off, off,  on, off), --  2
      (off, off,  on,  on), --  3
      (off,  on, off, off), --  4
      (off,  on, off,  on), --  5
      (off,  on,  on, off), --  6
      (off,  on,  on,  on), --  7
      ( on, off, off, off), --  8
      ( on, off, off,  on), --  9
      ( on, off,  on, off), -- 10
      ( on, off,  on,  on), -- 11
      ( on,  on, off, off), -- 12
      ( on,  on, off,  on), -- 13
      ( on,  on,  on, off), -- 14
      ( on,  on,  on,  on)); -- 15
   --
   -- Procedures
   --
   --
   -- The open procedure opens the device file for the specified LED.
   --
   procedure open(l : in led_num);
   --
   -- If no led is specified, open all of them
   --
   procedure open;
   --
   -- The set procedure sets the specified LED to the specified state.
   --
   procedure set(l : in led_num; s : in led_state);
   --
   -- Set procedures to set all of the LEDs in one call
   --
   procedure set(led0 : in led_state; led1 : in led_state;
                 led2 : in led_state; led3 : in led_state);

   procedure set(leds : in led_states);
   --
   -- The close procedure closes the device file for the specified LED.
   --
  procedure close(l : in led_num);
  --
  -- With no parameters, close all the LED device files
  --
  procedure close;
private
   --
   -- This will have to be fiddled with if the names ever change, but at
   -- least it's in one spot.
   --
--   led_names : constant array (led_num) of String(1 .. 48) :=
--     ("/sys/class/leds/beaglebone:green:usr0/brightness",
--      "/sys/class/leds/beaglebone:green:usr1/brightness",
--      "/sys/class/leds/beaglebone:green:usr2/brightness",
--      "/sys/class/leds/beaglebone:green:usr3/brightness");
   led_names : constant array (led_num) of String(1 .. 48) :=
     (BBS.BBB.pins.LED_0 & "brightness",
      BBS.BBB.pins.LED_0 & "brightness",
      BBS.BBB.pins.LED_0 & "brightness",
      BBS.BBB.pins.LED_0 & "brightness");

   led_files : array (led_num) of Ada.Text_IO.File_Type;
end;
