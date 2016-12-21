package body BBS.embed.SPI is
   --
   function SPI_new return SPI_ptr is
   begin
      return new SPI_record;
   end;
   --
   -- Configure the SPI interface on a BeagleBone Black or other systems that
   -- have multiple functions on the SPI pins.  This configureation procedure
   -- sets the pins to the SPI function.
   --
   procedure configure(self : not null access SPI_record'class; SPI_file : string;
                       SCL : string; SDA : string) is
   begin
      null;
   end;
   --
   -- Configure the SPI interface on a Raspberry PI or other systems that have
   -- dedicated pins for the SPI interface.  This would also work on a system
   -- with shared pins if the pins had already been set to the SPI function.
   --
   procedure configure(self : not null access SPI_record'class; SPI_file : string) is
   begin
      self.port := C_open(SPI_file, O_RDWR, 8#666#);
   end;
   --
   procedure set(self : not null access SPI_record'class; value : uint8) is
      temp : size_t;
      t2 : uint8 := value;
   begin
--      Ada.Text_IO.Put_Line("Writing " & integer'Image(integer(value)));
      temp := c_write(self.port, t2, 1);
   end;
   --
   function get(self : not null access SPI_record'class) return uint8 is
      temp : uint8 := 0;
      dummy1 : size_t;
      dummy2 : ssize_t;
   begin
      dummy2 := C_read(self.port, temp, dummy1);
      return temp;
   end;
   --
end;
