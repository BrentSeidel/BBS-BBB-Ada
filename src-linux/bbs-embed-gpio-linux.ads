--
--  Author: Brent Seidel
--  Date: 24-Sep-2024
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
with Ada.Direct_IO;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
--with BBS.embed.GPIO;
with BBS.embed.Linux;
use type BBS.embed.Linux.file_id;
with Interfaces.C;
use type Interfaces.C.Int;
--
package BBS.embed.GPIO.Linux is
   --
   type direction is (input, output);
   type Linux_GPIO_record is new GPIO_record with private;
   --
   --  Max number of chips supported
   --
   max_chip : constant := 4;
   --
   --  Maximum number of requested lines.
   --
   --  Must be no greater than 64, as bitmaps are restricted here to 64-bits
   --  for simplicity, and a multiple of 2 to ensure 32/64-bit alignment of
   --  structs.
   --
   GPIO_V2_LINES_MAX : constant := 64;
   --
   --  GPIOs are assigned to a gpio chip and a line on the chip.
   --
   type gpio_id is record
      chip : uint8 range 0 .. max_chip;
      line : uint8 range 0 .. GPIO_V2_LINES_MAX - 1;
   end record;
   --
   --  Configure a new GPIO object.
   --
   procedure configure(self : in out Linux_GPIO_record; pin : gpio_id; dir : direction);
   --
   --  Set the direction of a pin.  This can be used whether a GPIO pin
   --  has been configured or not.  It is also used by the configure
   --  procedure.
   --
   procedure set_dir(self : in out Linux_GPIO_record;
                     dir : direction);
   --
   --  Set the value of an output GPIO.
   --
   overriding
   procedure set(self : Linux_GPIO_record; value : bit);
   --
   --  Read the value of an input GPIO.
   --
   overriding
   function get(self : Linux_GPIO_record) return bit;
   --
   --  Close the file for the pin.  Once this is called, the GPIO object will
   --  need to be re-configured.
   --
   procedure close(self : in out Linux_GPIO_record);
   --
   --  Routines specific to Linux ioctl GPIO interface
   --
   function chip_name(self : in out Linux_GPIO_record) return String;
   function line_name(self : in out Linux_GPIO_record) return String;
   --
private
   --
   gpio_fault : Exception;
   --
   --  Much of the following is a translation of the C gpio.h header file
   --  into Ada.  Enough has been translated to support the required
   --  functionality.  Structures that related to unimplemented functionality
   --  have not yet been translated.
   --
   --  NOTE: "write" means userland is writing and kernel is
   --  reading. "read" means userland is reading and kernel is writing.
   --  C macros for creating IOCTL numbers for reference.
   --
   --  #define _IOC(dir,type,nr,size) \
   --    (((dir)  << _IOC_DIRSHIFT) | \
   --    ((type) << _IOC_TYPESHIFT) | \
   --    ((nr)   << _IOC_NRSHIFT) | \
   --    ((size) << _IOC_SIZESHIFT))
   --
   --  #define _IOC_TYPECHECK(t) (sizeof(t))
   --
   --  Used to create ioctl command numbers.
   --
   --  NOTE: _IOW means userland is writing and kernel is reading. _IOR
   --  means userland is reading and kernel is writing.
   --
   --  #define _IO(type,nr)            _IOC(_IOC_NONE,(type),(nr),0)
   --  #define _IOR(type,nr,size)      _IOC(_IOC_READ,(type),(nr),(_IOC_TYPECHECK(size)))
   --  #define _IOW(type,nr,size)      _IOC(_IOC_WRITE,(type),(nr),(_IOC_TYPECHECK(size)))
   --  #define _IOWR(type,nr,size)     _IOC(_IOC_READ|_IOC_WRITE,(type),(nr),(_IOC_TYPECHECK(size)))
   --  #define _IOR_BAD(type,nr,size)  _IOC(_IOC_READ,(type),(nr),sizeof(size))
   --  #define _IOW_BAD(type,nr,size)  _IOC(_IOC_WRITE,(type),(nr),sizeof(size))
   --  #define _IOWR_BAD(type,nr,size) _IOC(_IOC_READ|_IOC_WRITE,(type),(nr),sizeof(size))
   --
   --  These types and record replace a bunch of C macros (some of which
   --  are listed above) for breaking the 32 bit IOCTL number into fields.
   --
   type dir_type is (none, write, read, rw);
   for dir_type use (none => 0, write => 1, read => 2, rw => 3);
   for dir_type'Size use 2;
   --
   subtype nr_type   is uint8;
   subtype io_type   is uint8;
   subtype size_type is uint14;
   --
   --  Structure for decoding IOCTL numbers.
   --
   type ioctl_type is record
      nr   : nr_type;     --   8 bits
      code : io_type;     --   8 bits (Linux calls this type)
      size : size_type;   --  14 bits
      dir  : dir_type;    --   2 bits
   end record;
   for ioctl_type use record
      nr   at 0 range  0 ..  7;
      code at 0 range  8 .. 15;
      size at 0 range 16 .. 29;
      dir  at 0 range 30 .. 31;
   end record;
   for ioctl_type'Size use 32;
   --
   type ioctl_num is new uint32;
   --
   function ioctl_to_num is new Ada.Unchecked_Conversion(source => ioctl_type,
         target => ioctl_num);
   --
   GPIO_MAX_NAME_SIZE : constant Integer := 32;  --  The maximum size of name and label arrays.
   --
   --  struct gpiochip_info - Information about a certain GPIO chip
   --  @name: the Linux kernel name of this GPIO chip
   --  @label: a functional name for this GPIO chip, such as a product
   --          number, may be empty (i.e. label[0] == '\0')
   --  @lines: number of GPIO lines on this chip
   --
   type gpiochip_info is record
      name  : String(1 .. GPIO_MAX_NAME_SIZE);
      label : String(1 .. GPIO_MAX_NAME_SIZE);
      lines : uint32;
   end record with Convention => C;
   --
   --  The maximum number of configuration attributes associated with a line
   --  request.
   --
   GPIO_V2_LINE_NUM_ATTRS_MAX : constant Integer := 10;
   --
   --  enum gpio_v2_line_flag - &struct gpio_v2_line_attribute.flags values
   --  @GPIO_V2_LINE_FLAG_USED: line is not available for request
   --  @GPIO_V2_LINE_FLAG_ACTIVE_LOW: line active state is physical low
   --  @GPIO_V2_LINE_FLAG_INPUT: line is an input
   --  @GPIO_V2_LINE_FLAG_OUTPUT: line is an output
   --  @GPIO_V2_LINE_FLAG_EDGE_RISING: line detects rising (inactive to active)
   --                                  edges
   --  @GPIO_V2_LINE_FLAG_EDGE_FALLING: line detects falling (active to
   --                                   inactive) edges
   --  @GPIO_V2_LINE_FLAG_OPEN_DRAIN: line is an open drain output
   --  @GPIO_V2_LINE_FLAG_OPEN_SOURCE: line is an open source output
   --  @GPIO_V2_LINE_FLAG_BIAS_PULL_UP: line has pull-up bias enabled
   --  @GPIO_V2_LINE_FLAG_BIAS_PULL_DOWN: line has pull-down bias enabled
   --  @GPIO_V2_LINE_FLAG_BIAS_DISABLED: line has bias disabled
   --  @GPIO_V2_LINE_FLAG_EVENT_CLOCK_REALTIME: line events contain REALTIME timestamps
   --  @GPIO_V2_LINE_FLAG_EVENT_CLOCK_HTE: line events contain timestamps from
   --                                      hardware timestamp engine
   --
   type gpio_v2_line_flag is record
      GPIO_V2_LINE_FLAG_USED                 : Boolean;
      GPIO_V2_LINE_FLAG_ACTIVE_LOW           : Boolean;
      GPIO_V2_LINE_FLAG_INPUT                : Boolean;
      GPIO_V2_LINE_FLAG_OUTPUT               : Boolean;
      GPIO_V2_LINE_FLAG_EDGE_RISING          : Boolean;
      GPIO_V2_LINE_FLAG_EDGE_FALLING         : Boolean;
      GPIO_V2_LINE_FLAG_OPEN_DRAIN           : Boolean;
      GPIO_V2_LINE_FLAG_OPEN_SOURCE          : Boolean;
      GPIO_V2_LINE_FLAG_BIAS_PULL_UP         : Boolean;
      GPIO_V2_LINE_FLAG_BIAS_PULL_DOWN       : Boolean;
      GPIO_V2_LINE_FLAG_BIAS_DISABLED        : Boolean;
      GPIO_V2_LINE_FLAG_EVENT_CLOCK_REALTIME : Boolean;
      GPIO_V2_LINE_FLAG_EVENT_CLOCK_HTE      : Boolean;
   end record with Alignment => 8;
   for gpio_v2_line_flag use record
      GPIO_V2_LINE_FLAG_USED                 at 0 range  0 ..  0;
      GPIO_V2_LINE_FLAG_ACTIVE_LOW           at 0 range  1 ..  1;
      GPIO_V2_LINE_FLAG_INPUT                at 0 range  2 ..  2;
      GPIO_V2_LINE_FLAG_OUTPUT               at 0 range  3 ..  3;
      GPIO_V2_LINE_FLAG_EDGE_RISING          at 0 range  4 ..  4;
      GPIO_V2_LINE_FLAG_EDGE_FALLING         at 0 range  5 ..  5;
      GPIO_V2_LINE_FLAG_OPEN_DRAIN           at 0 range  6 ..  6;
      GPIO_V2_LINE_FLAG_OPEN_SOURCE          at 0 range  7 ..  7;
      GPIO_V2_LINE_FLAG_BIAS_PULL_UP         at 0 range  8 ..  8;
      GPIO_V2_LINE_FLAG_BIAS_PULL_DOWN       at 0 range  9 ..  9;
      GPIO_V2_LINE_FLAG_BIAS_DISABLED        at 0 range 10 .. 10;
      GPIO_V2_LINE_FLAG_EVENT_CLOCK_REALTIME at 0 range 11 .. 11;
      GPIO_V2_LINE_FLAG_EVENT_CLOCK_HTE      at 0 range 12 .. 12;
   end record;
   for gpio_v2_line_flag'Size use 64;

   type bits64 is array (0 .. 63) of bit
      with pack;
   for bits64'Size use 64;
   --
   --  struct gpio_v2_line_values - Values of GPIO lines
   --  @bits: a bitmap containing the value of the lines, set to 1 for active
   --         and 0 for inactive.
   --  @mask: a bitmap identifying the lines to get or set, with each bit
   --         number corresponding to the index into &struct
   --         gpio_v2_line_request.offsets.
   --
   type gpio_v2_line_values is record
      bits : bits64;
      mask : bits64;
   end record with Alignment => 8, Convention => C;
   for gpio_v2_line_values'Size use 128;

   --
   --  enum gpio_v2_line_attr_id - &struct gpio_v2_line_attribute.id values
   --  identifying which field of the attribute union is in use.
   --  @GPIO_V2_LINE_ATTR_ID_FLAGS: flags field is in use
   --  @GPIO_V2_LINE_ATTR_ID_OUTPUT_VALUES: values field is in use
   --  @GPIO_V2_LINE_ATTR_ID_DEBOUNCE: debounce_period_us field is in use
   --
   type gpio_v2_line_attr_id is (GPIO_V2_LINE_ATTR_ID_FLAGS,
                                 GPIO_V2_LINE_ATTR_ID_OUTPUT_VALUES,
                                 GPIO_V2_LINE_ATTR_ID_DEBOUNCE);
   for gpio_v2_line_attr_id use (GPIO_V2_LINE_ATTR_ID_FLAGS => 1,
                                 GPIO_V2_LINE_ATTR_ID_OUTPUT_VALUES => 2,
                                 GPIO_V2_LINE_ATTR_ID_DEBOUNCE => 3);
   for gpio_v2_line_attr_id'Size use 32;

   --
   --  struct gpio_v2_line_attribute - a configurable attribute of a line
   --  @id: attribute identifier with value from &enum gpio_v2_line_attr_id
   --  @padding: reserved for future use and must be zero filled
   --  @flags: if id is %GPIO_V2_LINE_ATTR_ID_FLAGS, the flags for the GPIO
   --          line, with values from &enum gpio_v2_line_flag, such as
   --          %GPIO_V2_LINE_FLAG_ACTIVE_LOW, %GPIO_V2_LINE_FLAG_OUTPUT etc, added
   --          together.  This overrides the default flags contained in the &struct
   --          gpio_v2_line_config for the associated line.
   --  @values: if id is %GPIO_V2_LINE_ATTR_ID_OUTPUT_VALUES, a bitmap
   --           containing the values to which the lines will be set, with each bit
   --           number corresponding to the index into &struct
   --           gpio_v2_line_request.offsets.
   --  @debounce_period_us: if id is %GPIO_V2_LINE_ATTR_ID_DEBOUNCE, the
   --                       desired debounce period, in microseconds
   --
   type line_attr_union (discr : gpio_v2_line_attr_id := GPIO_V2_LINE_ATTR_ID_FLAGS) is record
      case discr is
         when GPIO_V2_LINE_ATTR_ID_FLAGS =>
            flags : gpio_v2_line_flag;
         when GPIO_V2_LINE_ATTR_ID_OUTPUT_VALUES =>
            values : uint64;
         when others =>
            debounce_period_us : uint32;
      end case;
   end record
   with Convention => C_Pass_By_Copy,
        Unchecked_Union => True;
   type gpio_v2_line_attribute is record
      id     : gpio_v2_line_attr_id;
      unused : uint32;
      union  : line_attr_union;
   end record with Convention => C_Pass_By_Copy;
   type line_attr_array is array (0 .. GPIO_V2_LINE_NUM_ATTRS_MAX - 1) of gpio_v2_line_attribute;

   --
   --  struct gpio_v2_line_config_attribute - a configuration attribute
   --  associated with one or more of the requested lines.
   --  @attr: the configurable attribute
   --  @mask: a bitmap identifying the lines to which the attribute applies,
   --         with each bit number corresponding to the index into &struct
   --         gpio_v2_line_request.offsets.
   --
   type gpio_v2_line_config_attribute is record
      attr : gpio_v2_line_attribute;
      mask : uint64;
   end record with  Alignment => 8;
   type config_attr_array is array (0 .. GPIO_V2_LINE_NUM_ATTRS_MAX - 1) of gpio_v2_line_config_attribute;
   --
   --  struct gpio_v2_line_config - Configuration for GPIO lines
   --  @flags: flags for the GPIO lines, with values from &enum
   --          gpio_v2_line_flag, such as %GPIO_V2_LINE_FLAG_ACTIVE_LOW,
   --          %GPIO_V2_LINE_FLAG_OUTPUT etc, added together.  This is the default for
   --          all requested lines but may be overridden for particular lines using
   --          @attrs.
   --  @num_attrs: the number of attributes in @attrs
   --  @padding: reserved for future use and must be zero filled
   --  @attrs: the configuration attributes associated with the requested
   --          lines.  Any attribute should only be associated with a particular line
   --          once.  If an attribute is associated with a line multiple times then the
   --          first occurrence (i.e. lowest index) has precedence.
   --
   type gpio_v2_line_config is record
      flags     : gpio_v2_line_flag;
      num_attrs : uint32;
      unused1   : uint32 := 0;
      unused2   : uint32 := 0;
      unused3   : uint32 := 0;
      unused4   : uint32 := 0;
      unused5   : uint32 := 0;
      attrs     : config_attr_array;
   end record;
   --
   --  struct gpio_v2_line_request - Information about a request for GPIO lines
   --  @offsets: an array of desired lines, specified by offset index for the
   --            associated GPIO chip
   --  @consumer: a desired consumer label for the selected GPIO lines such as
   --             "my-bitbanged-relay"
   --  @config: requested configuration for the lines.
   --  @num_lines: number of lines requested in this request, i.e. the number
   --              of valid fields in the %GPIO_V2_LINES_MAX sized arrays, set to 1 to
   --              request a single line
   --  @event_buffer_size: a suggested minimum number of line events that the
   --                      kernel should buffer.  This is only relevant if edge
   --                      detection is enabled in the configuration. Note that
   --                      this is only a suggested value and the kernel may
   --                      allocate a larger buffer or cap the size of the
   --                      buffer. If this field is zero then the buffer size
   --                      defaults to a minimum of @num_lines * 16.
   --  @padding: reserved for future use and must be zero filled
   --  @fd: if successful this field will contain a valid anonymous file handle
   --       after a %GPIO_GET_LINE_IOCTL operation, zero or negative value means
   --       error
   --
   type off_num is new uint32;
   type offset_arr is array (0 .. GPIO_V2_LINES_MAX - 1) of off_num;
   type gpio_v2_line_request is record
      offsets   : offset_arr;
      consumer  : String(1 .. GPIO_MAX_NAME_SIZE);
      config    : gpio_v2_line_config;
      num_lines : uint32;
      event_buffer_size : uint32;
      unused1 : uint32 := 0;
      unused2 : uint32 := 0;
      unused3 : uint32 := 0;
      unused4 : uint32 := 0;
      unused5 : uint32 := 0;
      fd      : BBS.embed.Linux.file_id;
   end record;
   --
   --  struct gpio_v2_line_info - Information about a certain GPIO line
   -- * @name: the name of this GPIO line, such as the output pin of the line on
   --          the chip, a rail or a pin header name on a board, as specified by the
   --          GPIO chip, may be empty (i.e. name[0] == '\0')
   --  @consumer: a functional name for the consumer of this GPIO line as set
   --             by whatever is using it, will be empty if there is no current user but
   --             may also be empty if the consumer doesn't set this up
   --  @offset: the local offset on this GPIO chip, fill this in when
   --           requesting the line information from the kernel
   --  @num_attrs: the number of attributes in @attrs
   --  @flags: flags for this GPIO line, with values from &enum
   --          gpio_v2_line_flag, such as %GPIO_V2_LINE_FLAG_ACTIVE_LOW,
   --          %GPIO_V2_LINE_FLAG_OUTPUT etc, added together.
   --  @attrs: the configuration attributes associated with the line
   --  @padding: reserved for future use
   --
   type gpio_v2_line_info is record
      name      : String(1 .. GPIO_MAX_NAME_SIZE);
      consumer  : String(1 .. GPIO_MAX_NAME_SIZE);
      offset    : off_num;
      num_attrs : uint32;
      flags     : gpio_v2_line_flag;
      attrs     : line_attr_array;
      unused1   : uint32 := 0;
      unused2   : uint32 := 0;
      unused3   : uint32 := 0;
      unused4   : uint32 := 0;
   end record;

--/**
-- * enum gpio_v2_line_changed_type - &struct gpio_v2_line_changed.event_type
-- * values
-- * @GPIO_V2_LINE_CHANGED_REQUESTED: line has been requested
-- * @GPIO_V2_LINE_CHANGED_RELEASED: line has been released
-- * @GPIO_V2_LINE_CHANGED_CONFIG: line has been reconfigured
-- */
--enum gpio_v2_line_changed_type {
--	GPIO_V2_LINE_CHANGED_REQUESTED	= 1,
--	GPIO_V2_LINE_CHANGED_RELEASED	= 2,
--	GPIO_V2_LINE_CHANGED_CONFIG	= 3,
--};

--/**
-- * struct gpio_v2_line_info_changed - Information about a change in status
-- * of a GPIO line
-- * @info: updated line information
-- * @timestamp_ns: estimate of time of status change occurrence, in nanoseconds
-- * @event_type: the type of change with a value from &enum
-- * gpio_v2_line_changed_type
-- * @padding: reserved for future use
-- */
--struct gpio_v2_line_info_changed {
--	struct gpio_v2_line_info info;
--	__aligned_u64 timestamp_ns;
--	__u32 event_type;
--	/* Pad struct to 64-bit boundary and reserve space for future use. */
--	__u32 padding[5];
--};

--/**
-- * enum gpio_v2_line_event_id - &struct gpio_v2_line_event.id values
-- * @GPIO_V2_LINE_EVENT_RISING_EDGE: event triggered by a rising edge
-- * @GPIO_V2_LINE_EVENT_FALLING_EDGE: event triggered by a falling edge
-- */
--enum gpio_v2_line_event_id {
--	GPIO_V2_LINE_EVENT_RISING_EDGE	= 1,
--	GPIO_V2_LINE_EVENT_FALLING_EDGE	= 2,
--};

--/**
-- * struct gpio_v2_line_event - The actual event being pushed to userspace
-- * @timestamp_ns: best estimate of time of event occurrence, in nanoseconds.
-- * @id: event identifier with value from &enum gpio_v2_line_event_id
-- * @offset: the offset of the line that triggered the event
-- * @seqno: the sequence number for this event in the sequence of events for
-- * all the lines in this line request
-- * @line_seqno: the sequence number for this event in the sequence of
-- * events on this particular line
-- * @padding: reserved for future use
-- *
-- * By default the @timestamp_ns is read from %CLOCK_MONOTONIC and is
-- * intended to allow the accurate measurement of the time between events.
-- * It does not provide the wall-clock time.
-- *
-- * If the %GPIO_V2_LINE_FLAG_EVENT_CLOCK_REALTIME flag is set then the
-- * @timestamp_ns is read from %CLOCK_REALTIME.
-- */
--struct gpio_v2_line_event {
--	__aligned_u64 timestamp_ns;
--	__u32 id;
--	__u32 offset;
--	__u32 seqno;
--	__u32 line_seqno;
--	/* Space reserved for future use. */
--	__u32 padding[6];
--};
   --
   --  v1 and v2 ioctl()s
   --
   --  Note that ioctl() expects the size to be in bytes while 'Size returns
   --  the size in bits, hence the divide by 8.
   --
   --  Passing the ioctl_type to the ioctl() does not seem to work, even
   --  though doing an unchecked conversion of it to an ioctl_num (uint32)
   --  and passing it to the ioctl() does work.  The structure must put
   --  something else on the stack that confuses the C code.
   --
   GPIO_GET_CHIPINFO_IOCTL          : constant ioctl_num
      := ioctl_to_num((dir => read, code => 16#b4#, nr => 1, size => gpiochip_info'Size/8));
   GPIO_GET_LINEINFO_UNWATCH_IOCTL  : constant ioctl_num
      := ioctl_to_num((dir => rw, code => 16#b4#, nr => 16#0c#, size => uint32'Size/8));
   --
   --  v2 ioctl()s
   --
   GPIO_V2_GET_LINEINFO_IOCTL       : constant ioctl_num := ioctl_to_num((
      dir => rw, code => 16#b4#, nr => 5, size => gpio_v2_line_info'Size/8));
   GPIO_V2_GET_LINEINFO_WATCH_IOCTL : constant ioctl_num := ioctl_to_num((
      dir => rw, code => 16#b4#, nr => 6, size => gpio_v2_line_info'Size/8));
   GPIO_V2_GET_LINE_IOCTL           : constant ioctl_num := ioctl_to_num((
      dir => rw, code => 16#b4#, nr => 7, size => gpio_v2_line_request'Size/8));
   GPIO_V2_LINE_SET_CONFIG_IOCTL    : constant ioctl_num := ioctl_to_num((
      dir => rw, code => 16#b4#, nr => 16#0d#, size => gpio_v2_line_config'Size/8));
   GPIO_V2_LINE_GET_VALUES_IOCTL    : constant ioctl_num := ioctl_to_num((
      dir => rw, code => 16#b4#, nr => 16#0e#, size => gpio_v2_line_values'Size/8));
   GPIO_V2_LINE_SET_VALUES_IOCTL    : constant ioctl_num := ioctl_to_num((
      dir => rw, code => 16#b4#, nr => 16#0f#, size => gpio_v2_line_values'Size/8));
   --
   function cinfo_ioctl(f_id : BBS.embed.Linux.file_id; cmd : ioctl_num; -- BBS.embed.gpio.Linux.ioctl_type;
      info : out gpiochip_info) return Interfaces.C.Int
      with pre => (cmd = GPIO_GET_CHIPINFO_IOCTL);
   pragma Import(C, cinfo_ioctl, "ioctl");
   --
   function linfo_ioctl(f_id : BBS.embed.Linux.file_id; cmd : ioctl_num; -- BBS.embed.gpio.Linux.ioctl_type;
      info : in out gpio_v2_line_info) return Interfaces.C.Int
      with pre => (cmd = GPIO_V2_GET_LINEINFO_IOCTL);
   pragma Import(C, linfo_ioctl, "ioctl");
   --
   function values_ioctl(f_id : BBS.embed.Linux.file_id; cmd : ioctl_num; -- BBS.embed.gpio.Linux.ioctl_type;
      info : in out gpio_v2_line_values) return Interfaces.C.Int
      with pre => ((cmd = GPIO_V2_LINE_GET_VALUES_IOCTL) or (cmd = GPIO_V2_LINE_SET_VALUES_IOCTL));
   pragma Import(C, values_ioctl, "ioctl");
   --
   function req_ioctl(f_id : BBS.embed.Linux.file_id; cmd : ioctl_num; -- BBS.embed.gpio.Linux.ioctl_type;
      info : in out gpio_v2_line_request) return Interfaces.C.Int
      with pre => (cmd = GPIO_V2_GET_LINE_IOCTL);
   pragma Import(C, req_ioctl, "ioctl");

   --
   --  File names
   --
   names : constant array (uint8 range 0 .. max_chip) of String(1 .. 14) :=
         ("/dev/gpiochip0", "/dev/gpiochip1", "/dev/gpiochip2", "/dev/gpiochip3", "/dev/gpiochip4");
   --
   --  GPIO Object.
   --
   type Linux_GPIO_record is new GPIO_record with record
      chip   : uint8;
      line   : uint8;
      offset : off_num;  --  This comes from the line request ioctl() call.
                         --  It is used instead of line number when setting
                         --  or getting the GPIO state.
      req    : BBS.embed.Linux.file_id;  --  This file descriptor is the one used to actually
                         --  set and get the GPIO value.  It is returned
                         --  by a line request ioctl() call.
      dir    : direction;
      valid  : Boolean := False;
   end record;
   --
   --  Chip data
   --
   type gpiochip_data is record
      chip : BBS.embed.Linux.file_id;
      open : Boolean := False;
   end record;
   gpiochips : array (uint8 range 0 .. max_chip) of gpiochip_data;

end BBS.embed.GPIO.Linux;
