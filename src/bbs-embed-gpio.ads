package BBS.embed.GPIO is
--

   type GPIO_record is abstract tagged limited null record;
   type GPIO is access all GPIO_record'Class;
   type direction is (input, output);
   --
   -- Create a new GPIO object
   --
--   function gpio_new return GPIO is abstract;
   --
   -- Configure a new GPIO object.  The pin control file and GPIO directory
   -- must correspond, otherwise things will not work correctly.  Pin should
   -- be one of the pin constants and port should be one of
   -- the gpio constants from the device specific pins packages.
   --
   procedure configure(self : in out GPIO_record;
                       pin : string; port : string; dir : direction) is abstract;
   --
   -- Not all GPIOs have an associated pin control file.  Some pins are dedicated
   -- to GPIO and have no other function.
   --
   procedure configure(self : in out GPIO_record;
                       port : string; dir : direction) is abstract;
   --
   -- Set the value of an output GPIO.
   --
   procedure set(self : GPIO_record; value : bit) is abstract;
   --
   -- Read the value of an input GPIO.
   --
   function get(self : GPIO_record) return bit is abstract;
end;
