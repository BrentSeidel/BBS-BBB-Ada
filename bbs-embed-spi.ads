with Ada.Streams.Stream_IO;

package BBS.embed.SPI is

   --
   -- The SPI  object
   --
   type SPI_record is tagged private;
   type SPI_ptr is access SPI_record;

private
   type SPI_record is tagged
      record
         In_File   : Ada.Streams.Stream_IO.File_Type;
         In_Stream : Ada.Streams.Stream_IO.Stream_Access;
         Out_File   : Ada.Streams.Stream_IO.File_Type;
         Out_Stream : Ada.Streams.Stream_IO.Stream_Access;
      end record;

end;
