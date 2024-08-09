package BBS.embed.GPIO is
   pragma Preelaborate;
   --
   type GPIO_record is abstract tagged limited null record;
   type GPIO is access all GPIO_record'Class;
   --
   -- Set the value of an output GPIO.
   --
   procedure set(self : GPIO_record; value : bit) is abstract;
   --
   -- Read the value of an input GPIO.
   --
   function get(self : GPIO_record) return bit is abstract;
end;
