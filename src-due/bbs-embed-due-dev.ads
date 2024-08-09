--
--  This package contains the device IDs and any other device information that
--  should be collected into one place.  The device registers and such are in
--  the individual auto-generated device files.
--
package BBS.embed.due.dev is
   SUPC_ID   : constant :=  0;  --  Supply controller
   RSTC_ID   : constant :=  1;  --  Reset controller
   RTC_ID    : constant :=  2;  --  Real time clock
   RTT_ID    : constant :=  3;  --  Real time timer
   WDG_ID    : constant :=  4;  --  Watchdog timer
   PMC_ID    : constant :=  5;  --  Power management controller
   EEFC0_ID  : constant :=  6;  --  Enhanced embedded flash controller 0
   EEFC1_ID  : constant :=  7;  --  Enhanced embedded flash controller 7
   UART_ID   : constant :=  8;  --  Universal asynchronous receiver/transmitter
   SMC_SDRAMC_ID : constant :=  9;  -- Static memory/Synchronous dynamic RAM controller
   SDRAMC_ID : constant := 10;  --  Synchronous dynamic memory controller
   PIOA_ID   : constant := 11;  --  Parallel I/O controller A
   PIOB_ID   : constant := 12;  --  Parallel I/O controller B
   PIOC_ID   : constant := 13;  --  Parallel I/O controller C
   PIOD_ID   : constant := 14;  --  Parallel I/O controller D
   PIOE_ID   : constant := 15;  --  Parallel I/O controller E (not present)
   PIOF_ID   : constant := 16;  --  Parallel I/O controller F (not present)
   USART0_ID : constant := 17;  --  Universal synchronous/asynchronous receiver/transmitter 0
   USART1_ID : constant := 18;  --  Universal synchronous/asynchronous receiver/transmitter 1
   USART2_ID : constant := 19;  --  Universal synchronous/asynchronous receiver/transmitter 2
   USART3_ID : constant := 20;  --  Universal synchronous/asynchronous receiver/transmitter 3
   HSMCI_ID  : constant := 21;  --  High speed multimedia card interface
   TWI0_ID   : constant := 22;  --  Two-wire interface 0 (I2C)
   TWI1_ID   : constant := 23;  --  Two-wire interface 1 (I2C)
   SPI0_ID   : constant := 24;  --  Serial peripheral interface 0
   SPI1_ID   : constant := 25;  --  Serial peripheral interface 1
   SCC_ID    : constant := 26;  --  Synchronous serial controller
   TC0_ID    : constant := 27;  --  Timer counter channel 0
   TC1_ID    : constant := 28;  --  Timer counter channel 1
   TC2_ID    : constant := 29;  --  Timer counter channel 2
   TC3_ID    : constant := 30;  --  Timer counter channel 3
   TC4_ID    : constant := 31;  --  Timer counter channel 4
   TC5_ID    : constant := 32;  --  Timer counter channel 5
   TC6_ID    : constant := 33;  --  Timer counter channel 6
   TC7_ID    : constant := 34;  --  Timer counter channel 7
   TC8_ID    : constant := 35;  --  Timer counter channel 8
   PWM_ID    : constant := 36;  --  Pulse width modulator controller
   ADC_ID    : constant := 37;  --  ADC controller
   DACC_ID   : constant := 38;  --  DAC controller
   DMAC_ID   : constant := 39;  --  DMA controller
   UOTGHS_ID : constant := 40;  --  USB OTG controller
   TRNG_ID   : constant := 41;  --  True random number generator
   EMAC_ID   : constant := 42;  --  Ethernet MAC
   CAN0_ID   : constant := 43;  --  CAN controller 0
   CAN1_ID   : constant := 44;  --  CAN controller 1
end BBS.embed.due.dev;
