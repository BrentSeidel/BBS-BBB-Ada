package body BBS.embed.i2c.MCP23008 is
   --
   --
   -- Object oriented interface
   --
   function i2c_new return MCB23008_ptr is
   begin
      return new MCB23008_record;
   end;
   --
   procedure configure(self : not null access MCB23008_record'class; port : i2c_interface;
                       addr : addr7; error : out integer) is
      pragma Unreferenced(error);
   begin
      self.port := port;
      self.address := addr;
   end;
   --
   procedure set_dir(self : not null access MCB23008_record'class; dir : uint8;
                     error : out integer) is
   begin
      self.port.write(self.address, IODIR, dir, error);
   end;
   --
   procedure set_data(self : not null access MCB23008_record'class; data : uint8;
                      error : out integer) is
   begin
      self.port.write(self.address, GPIO, data, error);
   end;
   --
   function read_data(self : not null access MCB23008_record'class; error : out integer)
                      return uint8 is
   begin
      return self.port.read(self.address, GPIO, error);
   end;

end;
