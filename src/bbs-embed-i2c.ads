with BBS.embed;
--
--  This is effectively an abstract base class for I2C interfaces.  It defines
--  some common data in the interface and device clases (thus they can't be
--  labeled as abstract) and defines the required functions and procedures.  The
--  rest of the fields, the actual definitions for the functions and procedures,
--  as well as any other necessary information is defined in a hardware specific
--  subclass.
--
package BBS.embed.i2c is

   --
   --  Possible error codes
   --
   type err_code is (none, nack, ovre, invalid_addr, failed);
   --
   --  buffer to use for reading and writing from i2c bus.  In most cases, only
   --  a few bytes are needed.  This should be quite adequate.
   --
   type buff_index is new Integer range 0 .. 127;
   type buffer is array(buff_index'Range) of BBS.embed.uint8;
   type buff_ptr is access all buffer;
   --
   --   The I2C  object
   --
   --
   --  The root class for I2C interface objects.  This represents the actual I2C
   --  hardware interface.  A buffer is required for all interfaces, beyond that
   --  is hardware and operating system dependant.  There should be one of these
   --  for each interface.
   --
   type i2c_interface_record is tagged limited
      record
         b        : buffer;
      end record;
   type i2c_interface is access all i2c_interface_record'Class;
   --
   --  The I2C device object.  This represents an I2C device attached to an
   --  interface.  Each device object has a reference to its interface.  Beyond
   --  that is device dependant.
   --
   type i2c_device_record is tagged
      record
         hw      : i2c_interface;
         address : addr7;
      end record;
   type i2c_device is access i2c_device_record;
   --
   --  These functions and procedures really should be abstract, but since the
   --  objects aren't, something has to be declared for them.  Warnings are
   --  given for the functions since the error out parameter is not set.  This
   --  should not cause problems since they are never used.
   --
   --  Reading or writing a single byte is straigtforward.
   --
   procedure write(self : in out i2c_interface_record; addr : addr7; reg : uint8;
                   data : uint8; error : out err_code) is null;
   --
   pragma Warnings(off);
   function read(self : in out i2c_interface_record; addr : addr7; reg : uint8;
                 error : out err_code) return uint8 is (0);
   pragma Warnings(on);
   --
   --  When reading two bytes, is the MSB first or second?  There is no standard
   --  even within a single device.
   --
   --  Read a word with MSB first
   --
   pragma Warnings(off);
   function readm1(self : in out i2c_interface_record; addr : addr7; reg : uint8;
                 error : out err_code) return uint16 is (0);
   pragma Warnings(on);
   --
   --  Read a word with MSB second (LSB first)
   --
   pragma Warnings(off);
   function readm2(self : in out i2c_interface_record; addr : addr7; reg : uint8;
                 error : out err_code) return uint16 is (0);
   pragma Warnings(on);
   --
   --  Write an arbitrary number of bytes to a device on the i2c bus.
   --
   procedure write(self : in out i2c_interface_record; addr : addr7; reg : uint8;
                   size : buff_index; error : out err_code) is null;
   --
   --  Read the specified number of bytes into a buffer
   --
   procedure read(self : in out i2c_interface_record; addr : addr7; reg : uint8;
                  size : buff_index; error : out err_code) is null;

end;
