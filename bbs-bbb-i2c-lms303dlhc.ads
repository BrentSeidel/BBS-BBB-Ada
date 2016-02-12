with BBS.BBB.i2c;
--
-- This package contains constants and routines to communicate with the LMS303DLHC
-- accelerometer and magnetometer on the i2c bus.
--
-- The interface is fairly basic and doesn't use the advanced features of the
-- device.  If you wish a more sophisticated interface, this could provide a
-- useful starting point.
--
package BBS.BBB.i2c.LMS303DLHC is
   --
   -- Addresses for LMS303DLHC - accelerometer and magnetometer
   -- Note that though the accelerometer and magnetometer are on the same
   -- physical chip, they have different addresses on the I2C bus.
   --
   -- Accelerometer
   addr_accel : constant addr7 := 16#19#;
   accel_ctrl1 : constant uint8 := 16#20#;
   accel_ctrl2 : constant uint8 := 16#21#;
   accel_ctrl3 : constant uint8 := 16#22#;
   accel_ctrl4 : constant uint8 := 16#23#;
   accel_ctrl5 : constant uint8 := 16#24#;
   accel_ctrl6 : constant uint8 := 16#25#;
   accel_ref : constant uint8 := 16#26#;
   accel_status : constant uint8 := 16#27#;
   --
   -- Status bits
   accel_stat_zyxor : constant uint8 := 16#80#;
   accel_stat_zor : constant uint8 := 16#40#;
   accel_stat_yor : constant uint8 := 16#20#;
   accel_stat_xor : constant uint8 := 16#10#;
   accel_stat_zyxda : constant uint8 := 16#08#;
   accel_stat_zda : constant uint8 := 16#04#;
   accel_stat_yda : constant uint8 := 16#02#;
   accel_stat_xda : constant uint8 := 16#01#;
   --
   accel_out_x_h : constant uint8 := 16#28#;
   accel_out_x_l : constant uint8 := 16#29#;
   accel_out_y_h : constant uint8 := 16#2a#;
   accel_out_y_l : constant uint8 := 16#2b#;
   accel_out_z_h : constant uint8 := 16#2c#;
   accel_out_z_l : constant uint8 := 16#2d#;
   accel_fifo_ctrl : constant uint8 := 16#2e#;
   accel_fifo_src : constant uint8 := 16#2f#;
   accel_int1_cfg : constant uint8 := 16#30#;
   accel_int1_src : constant uint8 := 16#31#;
   accel_int1_ths : constant uint8 := 16#32#;
   accel_int1_duration : constant uint8 := 16#33#;
   accel_int2_cfg : constant uint8 := 16#34#;
   accel_int2_src : constant uint8 := 16#35#;
   accel_int2_ths : constant uint8 := 16#36#;
   accel_int2_duration : constant uint8 := 16#37#;
   accel_click_cfg : constant uint8 := 16#38#;
   accel_click_src : constant uint8 := 16#39#;
   accel_click_ths : constant uint8 := 16#3a#;
   accel_time_limit : constant uint8 := 16#3b#;
   accel_time_latency : constant uint8 := 16#3c#;
   accel_time_window : constant uint8 := 16#3d#;
   --
   -- Magnetometer
   addr_mag : constant addr7 := 16#1e#;
   mag_cra : constant uint8 := 16#00#;
   mag_crb : constant uint8 := 16#01#;
   mag_mr : constant uint8 := 16#02#;
   mag_out_x_h : constant uint8 := 16#03#;
   mag_out_x_l : constant uint8 := 16#04#;
   mag_out_y_h : constant uint8 := 16#07#; -- note not ascending order
   mag_out_y_l : constant uint8 := 16#08#;
   mag_out_z_h : constant uint8 := 16#05#;
   mag_out_z_l : constant uint8 := 16#06#;
   mag_sr : constant uint8 := 16#09#;
   --
   -- Status bits
   mag_lock : constant uint8 := 16#02#;
   mag_drdy : constant uint8 := 16#01#;
   --
   mag_ira : constant uint8 := 16#0a#;
   mag_irb : constant uint8 := 16#0b#;
   mag_irc : constant uint8 := 16#0c#;
   mag_temp_h : constant uint8 := 16#31#;
   mag_temp_l : constant uint8 := 16#32#;

   type accelerations is
      record
         x : integer;
         y : integer;
         z : integer;
      end record;

   type magnetism is
      record
         x : integer;
         y : integer;
         z : integer;
      end record;

   procedure configure(error : out integer);
   function get_temperature(error : out integer) return integer;
   function get_acceleration_x(error : out integer) return integer;
   function get_acceleration_y(error : out integer) return integer;
   function get_acceleration_z(error : out integer) return integer;
   function get_accelerations(error : out integer) return accelerations;
   function get_accel_status(error : out integer) return uint8;
   function accel_data_ready(error : out integer) return boolean;
   function get_magnet_x(error : out integer) return integer;
   function get_magnet_y(error : out integer) return integer;
   function get_magnet_z(error : out integer) return integer;
   function get_magnetism(error : out integer) return magnetism;
   function get_mag_status(error : out integer) return uint8;
   function mag_data_ready(error : out integer) return boolean;

private
   buff : aliased buffer;

end;
