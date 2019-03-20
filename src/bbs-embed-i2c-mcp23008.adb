package body BBS.embed.i2c.MCP23008 is
   --
   --
   -- Object oriented interface
   --
   procedure configure(self : in out MCB23008_record; port : i2c_interface;
                       addr : addr7; error : out err_code) is
      pragma Unreferenced(error);
   begin
      self.hw := port;
      self.address := addr;
   end;
   --
   procedure set_dir(self : MCB23008_record; dir : uint8;
                     error : out err_code) is
   begin
      self.hw.write(self.address, IODIR, dir, error);
   end;
   --
   procedure set_data(self : MCB23008_record; data : uint8;
                      error : out err_code) is
   begin
      self.hw.write(self.address, GPIO, data, error);
   end;
   --
   function read_data(self : MCB23008_record; error : out err_code)
                      return uint8 is
   begin
      return self.hw.read(self.address, GPIO, error);
   end;

end;
