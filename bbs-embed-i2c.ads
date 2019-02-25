with BBS.embed;
package BBS.embed.i2c is

   --
   -- The I2C  object
   --
   --
   -- The I2C interface object
   --
   type i2c_interface_record is abstract tagged limited null record;
   type i2c_interface is access i2c_interface_record;
   --
   -- The root class for I2C device objects
   --
   type i2c_device_record is abstract tagged limited null record;
   type i2c_device is access i2c_device_record;
   --
   -- Reading or writing a single byte is straigtforward.
   --
   procedure write(self : not null access i2c_interface_record'class; addr : addr7; reg : uint8;
                   data : uint8; error : out integer) is abstract;
   function read(self : not null access i2c_interface_record'class; addr : addr7; reg : uint8;
                 error : out integer) return uint8 is abstract;
   --
   -- When reading two bytes, is the MSB first or second?  There is no standard
   -- even within a single device.
   --
   -- Read a word with MSB first
   --
   function readm1(self : not null access i2c_interface_record'class; addr : addr7; reg : uint8;
                 error : out integer) return uint16 is abstract;
   --
   -- Read a word with MSB second (LSB first)
   --
   function readm2(self : not null access i2c_interface_record'class; addr : addr7; reg : uint8;
                 error : out integer) return uint16 is abstract;
   --
   -- Write an arbitrary number of bytes to a device on the i2c bus.
   --
   procedure write(self : not null access i2c_interface_record'class; addr : addr7; reg : uint8;
                   buff : buff_ptr; size : uint16; error : out integer) is abstract;
   --
   -- Read the specified number of bytes into a buffer
   --
   procedure read(self : not null access i2c_interface_record'class; addr : addr7; reg : uint8;
                  buff : buff_ptr; size : uint16; error : out integer) is abstract;

end;
