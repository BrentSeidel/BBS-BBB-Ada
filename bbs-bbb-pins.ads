package BBS.BBB.pins is
--
-- This package contains definitions for some of the BeagleBone Black pins.  If
-- nothing else, it can serve as a reference for the pins on the board.
--
-- If the /sys directory tree changes, the constants here can be changed and
-- the rest of the software should still work.  Hopefully.  So, use the constants
-- here.
--
-- The pin state files.
-- Note that not all pins have multiple functions.  The ones with a fixed
-- function do not need a state file.  These are listed as comments with
-- the function.
--
-- Pin assignments are based on the figures in http://beagleboard.org/support/bone101
--
-- P8_01 - DGND
-- P8_02 - DGND
-- P8_03 - GPIO_38 (cape MMC1_DAT6)
-- P8_04 - GPIO_39 (cape MMC1_DAT7)
-- P8_05 - GPIO_34 (cape MMC1_DAT2)
-- P8_06 - GPIO_35 (cape MMC1_DAT3)
   P8_07 : aliased constant string := "/sys/devices/platform/ocp/ocp:P8_07_pinmux/state";
   P8_08 : aliased constant string := "/sys/devices/platform/ocp/ocp:P8_08_pinmux/state";
   P8_09 : aliased constant string := "/sys/devices/platform/ocp/ocp:P8_09_pinmux/state";
   P8_10 : aliased constant string := "/sys/devices/platform/ocp/ocp:P8_10_pinmux/state";
   P8_11 : aliased constant string := "/sys/devices/platform/ocp/ocp:P8_11_pinmux/state";
   P8_12 : aliased constant string := "/sys/devices/platform/ocp/ocp:P8_12_pinmux/state";
   P8_13 : aliased constant string := "/sys/devices/platform/ocp/ocp:P8_13_pinmux/state";
   P8_14 : aliased constant string := "/sys/devices/platform/ocp/ocp:P8_14_pinmux/state";
   P8_15 : aliased constant string := "/sys/devices/platform/ocp/ocp:P8_15_pinmux/state";
   P8_16 : aliased constant string := "/sys/devices/platform/ocp/ocp:P8_16_pinmux/state";
   P8_17 : aliased constant string := "/sys/devices/platform/ocp/ocp:P8_17_pinmux/state";
   P8_18 : aliased constant string := "/sys/devices/platform/ocp/ocp:P8_18_pinmux/state";
   P8_19 : aliased constant string := "/sys/devices/platform/ocp/ocp:P8_19_pinmux/state";
-- P8_20 - GPIO_63 (cape MMC1_CMD)
-- P8_21 - GPIO_62 (cape MMC1_CLK)
-- P8_22 - GPIO_37 (cape MMC1_DAT5)
-- P8_23 - GPIO_36 (cape MMC1_DAT4)
-- P8_24 - GPIO_33 (cape MMC1_DAT1)
-- P8_25 - GPIO_32 (cape MMC1_DAT0)
   P8_26 : aliased constant string := "/sys/devices/platform/ocp/ocp:P8_26_pinmux/state";
-- P8_27 - GPIO_86 (cape LCD VSYNC)
-- P8_28 - GPIO_88 (cape LCD_PCLK)
-- P8_29 - GPIO_87 (cape LCD_HSYNC)
-- P8_30 - GPIO_89 (cape LCD_AC_BIAS)
-- P8_31 - GPIO_10 (cape LCD_DATA14)
-- P8_32 - GPIO_11 (cape LCD_DATA15)
-- P8_33 - GPIO_9 (cape LCD_DATA13)
-- P8_34 - GPIO_81 (cape LCD_DATA11)
-- P8_35 - GPIO_8 (cape LCD_DATA12)
-- P8_36 - GPIO_80 (cape LCD_DATA10)
-- P8_37 - GPIO_78 (cape LCD_DATA8)
-- P8_38 - GPIO_79 (cape LCD_DATA9)
-- P8_39 - GPIO_76 (cape LCD_DATA6)
-- P8_40 - GPIO_77 (cape LCD_DATA7)
-- P8_41 - GPIO_74 (cape LCD_DATA4)
-- P8_42 - GPIO_75 (cape LCD_DATA5)
-- P8_43 - GPIO_72 (cape LCD_DATA2)
-- P8_44 - GPIO_73 (cape LCD_DATA3)
-- P8_45 - GPIO_70 (cape LCD_DATA0)
-- P8_46 - GPIO_71 (cape LCD_DATA1)
--
-- P9_01 - DGND
-- P9_02 - DGND
-- P9_03 - VDD_3V3
-- P9_04 - VDD_3V3
-- P9_05 - VDD_5V
-- P9_06 - VDD_5V
-- P9_07 - SYS_5V
-- P9_08 - SYS_5V
-- P9_09 - PWD_BUT
-- P9_10 - SYS_RESET
   P9_11 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_11_pinmux/state";
   P9_12 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_12_pinmux/state";
   P9_13 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_13_pinmux/state";
   P9_14 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_14_pinmux/state";
   P9_15 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_15_pinmux/state";
   P9_16 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_16_pinmux/state";
   P9_17 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_17_pinmux/state";
   P9_18 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_18_pinmux/state";
-- P9_19 - I2C2_SCL (cape use?)
-- P9_20 - I2C2_SDA (cape use?)
   P9_21 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_21_pinmux/state";
   P9_22 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_22_pinmux/state";
   P9_23 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_23_pinmux/state";
   P9_24 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_24_pinmux/state";
-- P9_25 - GPIO_117
   P9_26 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_26_pinmux/state";
   P9_27 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_27_pinmux/state";
-- P9_28 - GPIO_113
-- P9_29 - GPIO_111
   P9_30 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_30_pinmux/state";
-- P9_31 - GPIO_110
-- P9_32 - VDD_ADC
-- P9_33 - AIN4
-- P9_34 - GNDA_ADC
-- P9_35 - AIN6
-- P9_36 - AIN5
-- P9_37 - AIN2
-- P9_38 - AIN3
-- P9_39 - AIN0
-- P9_40 - AIN1
-- P9_41 - GPIO_20
   P9_42 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_42_pinmux/state";
-- P9_43 - DGND
-- P9_44 - DGND
-- P9_45 - DGND
-- P9_46 - DGND
--
-- Not sure where the following two pins are physically, but they show up in the
-- directory listing.
--
   P9_91 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_91_pinmux/state";
   P9_92 : aliased constant string := "/sys/devices/platform/ocp/ocp:P9_92_pinmux/state";
--
-- GPIO Definitions.  Note that additional GPIO may be exported.  This is the
-- set that appears on my system.  If you plan to use additional GPIO, just add
-- them to this list.
--
   GPIO_2 : aliased constant string := "/sys/class/gpio/gpio2/";
   GPIO_3 : aliased constant string := "/sys/class/gpio/gpio3/";
   GPIO_4 : aliased constant string := "/sys/class/gpio/gpio4/";
   GPIO_5 : aliased constant string := "/sys/class/gpio/gpio5/";
   GPIO_6 : aliased constant string := "/sys/class/gpio/gpio7/";
   GPIO_14 : aliased constant string := "/sys/class/gpio/gpio14/";
   GPIO_15 : aliased constant string := "/sys/class/gpio/gpio15/";
   GPIO_22 : aliased constant string := "/sys/class/gpio/gpio22/";
   GPIO_23 : aliased constant string := "/sys/class/gpio/gpio23/";
   GPIO_26 : aliased constant string := "/sys/class/gpio/gpio26/";
   GPIO_27 : aliased constant string := "/sys/class/gpio/gpio27/";
   GPIO_30 : aliased constant string := "/sys/class/gpio/gpio30/";
   GPIO_31 : aliased constant string := "/sys/class/gpio/gpio31/";
   GPIO_44 : aliased constant string := "/sys/class/gpio/gpio44/";
   GPIO_45 : aliased constant string := "/sys/class/gpio/gpio45/";
   GPIO_46 : aliased constant string := "/sys/class/gpio/gpio46/";
   GPIO_47 : aliased constant string := "/sys/class/gpio/gpio47/";
   GPIO_48 : aliased constant string := "/sys/class/gpio/gpio48/";
   GPIO_49 : aliased constant string := "/sys/class/gpio/gpio49/";
   GPIO_50 : aliased constant string := "/sys/class/gpio/gpio50/";
   GPIO_51 : aliased constant string := "/sys/class/gpio/gpio51/";
   GPIO_60 : aliased constant string := "/sys/class/gpio/gpio60/";
   GPIO_61 : aliased constant string := "/sys/class/gpio/gpio61/";
   GPIO_65 : aliased constant string := "/sys/class/gpio/gpio65/";
   GPIO_66 : aliased constant string := "/sys/class/gpio/gpio66/";
   GPIO_67 : aliased constant string := "/sys/class/gpio/gpio67/";
   GPIO_68 : aliased constant string := "/sys/class/gpio/gpio68/";
   GPIO_69 : aliased constant string := "/sys/class/gpio/gpio69/";
   GPIO_112 : aliased constant string := "/sys/class/gpio/gpio112/";
   GPIO_114 : aliased constant string := "/sys/class/gpio/gpio114/";
   GPIO_115 : aliased constant string := "/sys/class/gpio/gpio115/";
   GPIO_116 : aliased constant string := "/sys/class/gpio/gpio116/";
--
-- Built-in LED definitions
--
   LED_0 : aliased constant string := "/sys/class/leds/beaglebone:green:usr0/";
   LED_1 : aliased constant string := "/sys/class/leds/beaglebone:green:usr1/";
   LED_2 : aliased constant string := "/sys/class/leds/beaglebone:green:usr2/";
   LED_3 : aliased constant string := "/sys/class/leds/beaglebone:green:usr3/";
--
-- PWM Pins
--
--   EHRPWM2B := P8_13; -- or P8_46 (pwmchip5/pwm1)
--   EHRPWM2A := P8_19; -- or P8_45 (pwmchip5/pwm0)
--   EHRPWM1A := P9_14; -- or P8_34 (pwmchip2/pwm0)
--   EHRPWM1B := P9_16; -- or P8_36 (pwmchip2/pwm1)
--   EHRPWM0B := P9_21; -- or P9_29 (pwmchip0/pwm1)
--   EHRPWM0A := P9_22; -- or P9_31 (pwmchip0/pwm0)
--   ECAPPWM2 := P9_28; (pwmchip7) No pinmux for this pin
--   ECAPPWM0 := P9_42; (pwmchip4)
--
end;
