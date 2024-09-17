--
--  Author: Brent Seidel
--  Date: 9-Aug-2024
--
--  This file is part of bbs_embed.
--  Bbs_embed is free software: you can redistribute it and/or modify it
--  under the terms of the GNU General Public License as published by the
--  Free Software Foundation, either version 3 of the License, or (at your
--  option) any later version.
--
--  bbs_embed is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
--  Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with bbs_embed. If not, see <https://www.gnu.org/licenses/>.--
--
--
--  Driver for the ADS1015, 4 channel, 12 bit ADC/.
--
package BBS.embed.I2C.ADS1015 is
   --
   --  Mux Mode
   --  0 - AINp = AIN0 and AINn = AIN1 (default)
   --  1 - AINp = AIN0 and AINn = AIN3
   --  2 - AINp = AIN1 and AINn = AIN3
   --  3 - AINp = AIN2 and AINn = AIN3
   --  4 - AINp = AIN0 and AINn = GND (single ended)
   --  5 - AINp = AIN1 and AINn = GND (single ended)
   --  6 - AINp = AIN2 and AINn = GND (single ended)
   --  7 - AINp = AIN3 and AINn = GND (single ended)
   type mux_mode_type is (a0_a1, a0_a3, a1_a3, a2_a3, a0_gnd, a1_gnd, a2_gnd, a3_gnd);
   for mux_mode_type use (a0_a1 => 0, a0_a3 => 1, a1_a3 => 2, a2_a3 => 3, a0_gnd => 4,
                          a1_gnd => 5, a2_gnd => 6, a3_gnd => 7);
   for mux_mode_type'Size use 3;
   --
   --  PGA (Programmable Gain Amplifier)
   --  0 - FS = 6.144V
   --  1 - FS = 4.096V
   --  2 - FS = 2.048V (default)
   --  3 - FS = 1.024V
   --  4 - FS = 0.512V
   --  5 - FS = 0.256V
   --  6 - FS = 0.256V
   --  7 - FS = 0.256V
   type pga_type is (pga_6_144, pga_4_096, pga_2_048, pga_1_024, pga_0_512, pga_0_256);
   for pga_type use (pga_6_144 => 0, pga_4_096 => 1, pga_2_048 => 2,
                     pga_1_024 => 3, pga_0_512 => 4, pga_0_256 => 5);
   for pga_type'Size use 3;
   --
   --  Mode
   --  0 - Continuous conversion mode
   --  1 - Power-down single-shot mode (default)
   type mode_type is (continuous, single_shot);
   for mode_type use (continuous => 0, single_shot => 1);
   for mode_type'Size use 1;
   --
   --  DR (Data rate)
   --  0 -  128SPS (Samples per second?)
   --  1 -  250SPS
   --  2 -  490SPS
   --  3 -  920SPS
   --  4 - 1600SPS (default)
   --  5 - 2400SPS
   --  6 - 3300SPS
   --  7 - 3300SPS
   type data_rate_type is (dr_0128, dr_0250, dr_0490, dr_0920, dr_1600, dr_2400, dr_3300);
   for data_rate_type use (dr_0128 => 0, dr_0250 => 1, dr_0490 => 2,
                           dr_0920 => 3, dr_1600 => 4, dr_2400 => 5,
                           dr_3300 => 6);
   for data_rate_type'Size use 3;
   --
   --  Comp_mode (Comparator Mode)
   --  0 - Traditional, with hysteresis (default)
   --  1 - Window comparator
   type comp_mode_type is (traditional, window);
   for comp_mode_type use (traditional => 0, window => 1);
   for comp_mode_type'Size use 1;
   --
   --  Comp_pol (Comparator Polarity)
   --  0 - Active low (default)
   --  1 - Active high
   type comp_polarity is (act_low, act_high);
   for comp_polarity use (act_low => 0, act_high => 1);
   for comp_polarity'Size use 1;
   --
   --  Comp_lat (Latching Comparator)
   --  0 - Non-latching comparator (default)
   --  1 - Latching comparator
   type comp_latch is (no_latch, latch);
   for comp_latch use (no_latch => 0, latch => 1);
   for comp_latch'Size use 1;
   --
   --  Comp_que (Comparator queue and disable)
   --  0 - Assert after one conversion
   --  1 - Assert after two conversions
   --  2 - Assert after three conversions
   --  3 - Disable comparator (default)
   type comp_que_type is (comp_que_1, comp_que_2, comp_que_3, comp_que_d);
   for comp_que_type use (comp_que_1 => 0, comp_que_2 => 1, comp_que_3 => 2,
                          comp_que_d => 3);
   for comp_que_type'Size use 2;
   --
   -- Define object.
   --
   type ADS1015_record is new i2c_device_record with private;
   type ADS1015_ptr is access ADS1015_record;
   --
   --  Configuration record
   --  Config register bits
   --  15 - Operational status
   --  14 - Mux2
   --  13 - Mux1
   --  12 - Mux0
   --  11 - PGA2
   --  10 - PGA1
   --   9 - PGA0
   --   8 - Mode
   --   7 - DR2
   --   6 - DR1
   --   5 - DR0
   --   4 - Comp_mode
   --   3 - Comp_pol
   --   2 - Comp-lat
   --   1 - Comp_que1
   --   0 - Comp_que0
   --
   type ADS1015_config is record
     os : Boolean;
     mux : mux_mode_type;
     pga : pga_type;
     mode : mode_type;
     dr : data_rate_type;
     comp_mode : comp_mode_type;
     comp_pol : comp_polarity;
     comp_lat : comp_latch;
     comp_que : comp_que_type;
   end record with
     Size => 16;
   for ADS1015_config use record
     comp_que at 0 range 0 .. 1;
     comp_lat at 0 range 2 .. 2;
     comp_pol at 0 range 3 .. 3;
     comp_mode at 0 range 4 .. 4;
     dr at 0 range 5 .. 7;
     mode at 0 range 8 .. 8;
     pga at 0 range 9 .. 11;
     mux at 0 range 12 .. 14;
     os at 0 range 15 .. 15;
   end record;
   --
   --  Initial configuration to default configuration
   --
   procedure configure(self : in out ADS1015_record; port : i2c_interface;
                       addr : addr7; error : out err_code);
   --
   --  Initial configuration to specified configuration
   --
   procedure configure(self : in out ADS1015_record; port : i2c_interface;
                       addr : addr7; config : ADS1015_config; error : out err_code);
   --
   --  Check to see if configured device is present
   --
   function present(self : ADS1015_record) return Boolean;
   --
   --  Set ADC configuration to new value
   --
   procedure change_config(self : in out ADS1015_record;
                           config : ADS1015_config; error : out err_code);
   --
   procedure set_mux(self : in out ADS1015_record;
                     mux : mux_mode_type; error : out err_code);
   --
   procedure set_gain(self : in out ADS1015_record;
                     gain : pga_type; error : out err_code);
   --
   procedure set_continuous(self : in out ADS1015_record; error : out err_code);
   --
   procedure set_1shot(self : in out ADS1015_record; error : out err_code);
   --
   --  Use to start conversion when in single shot mode.  No effect in
   --  continuous mode.
   --
   procedure start_conversion(self : in out ADS1015_record; error : out err_code);
   --
   --  Checks if conversion is in progress.  Will always return false
   --  (conversion in progress) while in continuous mode.  Returns true
   --  when no conversion is in progress.
   --
   function conversion_done(self : in out ADS1015_record; error : out err_code)
      return Boolean;
   --
   function get_result(self : in out ADS1015_record; error : out err_code)
      return uint12;
   --
private
   --
   --  Device registers
   reg_conv      : constant uint8 := 0;  --  0 (RO) Conversion Register
   reg_config    : constant uint8 := 1;  --  1 (RW) Config Register
   reg_lo_thresh : constant uint8 := 2;  --  2 (RW) Low threshold
   reg_hi_thresh : constant uint8 := 3;  --  3 (RW) High threshold
   --
   default_config : constant ADS1015_config := (os => false, mux => a0_a1,
                               pga => pga_2_048, mode => single_shot, dr => dr_1600,
                               comp_mode => window, comp_pol => act_low,
                               comp_lat => no_latch, comp_que => comp_que_d);
   --
   type ADS1015_record is new i2c_device_record with record
      config : ADS1015_config;
   end record;
end;
