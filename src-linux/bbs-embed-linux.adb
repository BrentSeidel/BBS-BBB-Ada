package body BBS.embed.Linux is
   --
   -- Convert an string from strerror into a printable Ada string
   --
   function cvt_cstr_adastr(str_ptr : err_msg_ptr) return string is
      null_loc : constant integer :=
        Ada.Strings.Fixed.index(string(str_ptr.all), "" & ASCII.NUL);
   begin
      return Ada.Strings.Fixed.head(string(str_ptr.all), null_loc - 1);
   end;
end BBS.embed.Linux;
