with BBS.BBB.i2c;
--
-- This package contains constants and routines to communicate with the L3GD20H
-- gyroscope on the i2c bus.
--
-- The interface is fairly basic and doesn't use the advanced features of the
-- device.  If you wish a more sophisticated interface, this could provide a
-- useful starting point.
--
package BBS.BBB.i2c.L3GD20H is
   --
   -- Addresses for L3GD20H - gyroscope
   --
   addr : constant addr7 := 16#6b#;
   who_am_i : constant uint8 := 16#0f#;
   ctrl1 : constant uint8 := 16#20#;
   ctrl2 : constant uint8 := 16#21#;
   ctrl3 : constant uint8 := 16#22#;
   ctrl4 : constant uint8 := 16#23#;
   ctrl5 : constant uint8 := 16#24#;
   ref : constant uint8 := 16#25#;
   out_temp : constant uint8 := 16#26#;
   status : constant uint8 := 16#27#;
   out_x_l : constant uint8 := 16#28#;
   out_x_h : constant uint8 := 16#29#;
   out_y_l : constant uint8 := 16#2a#;
   out_y_h : constant uint8 := 16#2b#;
   out_z_l : constant uint8 := 16#2c#;
   out_z_h : constant uint8 := 16#2d#;
   fifo_ctrl : constant uint8 := 16#2e#;
   fifo_src : constant uint8 := 16#2f#;
   ig_cfg : constant uint8 := 16#30#;
   ig_src : constant uint8 := 16#31#;
   ig_ths_xh : constant uint8 := 16#32#;
   ig_ths_xl : constant uint8 := 16#33#;
   ig_ths_yh : constant uint8 := 16#34#;
   ig_ths_yl : constant uint8 := 16#35#;
   ig_ths_zh : constant uint8 := 16#36#;
   ig_ths_zl : constant uint8 := 16#37#;
   ig_duration : constant uint8 := 16#38#;
   low_odr : constant uint8 := 16#39#;

   type rotations is
      record
         x : integer;
         y : integer;
         z : integer;
      end record;

   procedure configure(error : out integer);
   function get_temperature(error : out integer) return integer;
   function get_rotation_x(error : out integer) return integer;
   function get_rotation_y(error : out integer) return integer;
   function get_rotation_z(error : out integer) return integer;
   function get_rotations(error : out integer) return rotations;

private
   buff : aliased buffer;
end;
