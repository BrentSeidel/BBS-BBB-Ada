with Ada.Unchecked_Conversion;
package body BBS.embed.I2C.ADS1015 is
  --
  --  Unchecked conversions for configuration
  --
  function config_to_uint16 is new Ada.Unchecked_Conversion
    (source => ADS1015_config, Target => uint16);
  function uint16_to_config is new Ada.Unchecked_Conversion
    (Source => uint16, Target => ADS1015_config);
  --
  --  Initial configuration of the device
  --
  procedure configure(self : in out ADS1015_record; port : i2c_interface;
                       addr : addr7; error : out err_code) is
  begin
    self.hw := port;
    self.address := addr;
    self.config := default_config;
    self.hw.writem1(self.address, reg_config, config_to_uint16(self.config), error);
  end;
  --
  procedure configure(self : in out ADS1015_record; port : i2c_interface;
                      addr : addr7; config : ADS1015_config; error : out err_code) is
  begin
    self.hw := port;
    self.address := addr;
    self.config := config;
    self.hw.writem1(self.address, reg_config, config_to_uint16(self.config), error);
  end;
  --
  procedure change_config(self : in out ADS1015_record;
                          config : ADS1015_config; error : out err_code) is
  begin
    self.config := config;
    self.hw.writem1(self.address, reg_config, config_to_uint16(self.config), error);
  end;
  --
  procedure set_mux(self : in out ADS1015_record;
                    mux : mux_mode_type; error : out err_code) is
  begin
    self.config.mux := mux;
    self.hw.writem1(self.address, reg_config, config_to_uint16(self.config), error);
  end;
  --
  procedure start_conversion(self : in out ADS1015_record; error : out err_code) is
    temp : ADS1015_config := self.config;
  begin
    temp.os := true;
    self.hw.writem1(self.address, reg_config, config_to_uint16(temp), error);
  end;
  --
  function conversion_done(self : in out ADS1015_record; error : out err_code)
      return Boolean is
    config : ADS1015_config := uint16_to_config(self.hw.readm1(self.address, reg_config, error));
  begin
    return config.os;
  end;
  --
  function get_result(self : in out ADS1015_record; error : out err_code)
      return uint12 is
    temp : uint16 := self.hw.readm1(self.address, reg_conv, error);
  begin
    return uint12(temp / 16#10#);
  end;
  --
end;
