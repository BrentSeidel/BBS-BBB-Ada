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
with Ada.Text_IO;
with Ada.Direct_IO;
with BBS.embed.GPIO;
--
package BBS.embed.GPIO.Linux is
   --
   type direction is (input, output);
   type Linux_GPIO_record is new GPIO_record with private;
   --
   --  *****************************************************************
   --  ***  The following routines will probably need to be updated
   --  ***  to work with the new GPIO system.  There may still be files
   --  ***  involved, but it looks like there will also be addresses to
   --  ***  structures.  These should be able to be treated as opaque
   --  ***  objects by the Ada software.
   --  *****************************************************************
   --
   --  Configure a new GPIO object.  The pin control file and GPIO directory
   --  must correspond, otherwise things will not work correctly.  Pin should
   --  be one of the pin constants and port should be one of
   --  the gpio constants from the device specific pins packages.
   --
   procedure configure(self : in out Linux_GPIO_record;
                       pin : string; port : string; dir : direction);
   --
   --  Not all GPIOs have an associated pin control file.  Some pins are dedicated
   --  to GPIO and have no other function.
   --
   procedure configure(self : in out Linux_GPIO_record;
                       port : string; dir : direction);
   --
   --  Set the direction of a pin.  This can be used whether a GPIO pin
   --  has been configured or not.  It is also used by the configure
   --  procedure.
   --
   procedure set_dir(self : in out Linux_GPIO_record;
                     port : String; dir : direction);
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
--private
   --
   --  Stuff for IOCTL.  This might be collected and moved elsewhere.
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
   --  Used to create numbers.
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
      dir  : dir_type;    --   2 bits
      code : io_type;     --   8 bits (Linux calls this type)
      size : size_type;   --  14 bits
   end record;
   for ioctl_type use record
      nr   at 0 range  0 ..  7;
      code at 0 range  8 .. 15;
      size at 0 range 16 .. 29;
      dir  at 0 range 30 .. 31;
   end record;
   for ioctl_type'Size use 32;
   --
   --  Structures and constants for GPIO IOCTL calls.  Based on the contents
   --  of gpio.h.
   --
   GPIO_MAX_NAME_SIZE : constant Integer := 32;  --  The maximum size of name and label arrays.
   --
   --  struct gpiochip_info - Information about a certain GPIO chip
   --  @name: the Linux kernel name of this GPIO chip
   --  @label: a functional name for this GPIO chip, such as a product
   --  number, may be empty (i.e. label[0] == '\0')
   --  @lines: number of GPIO lines on this chip
   --
   type gpiochip_info is record
      name  : String(1 .. GPIO_MAX_NAME_SIZE);
      label : String(1 .. GPIO_MAX_NAME_SIZE);
      lines : uint32;
   end record;
   --
   --  Maximum number of requested lines.
   --
   --  Must be no greater than 64, as bitmaps are restricted here to 64-bits
   --  for simplicity, and a multiple of 2 to ensure 32/64-bit alignment of
   --  structs.
   --
   GPIO_V2_LINES_MAX : constant Integer := 64;
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

   --
   --  struct gpio_v2_line_values - Values of GPIO lines
   --  @bits: a bitmap containing the value of the lines, set to 1 for active
   --         and 0 for inactive.
   --  @mask: a bitmap identifying the lines to get or set, with each bit
   --         number corresponding to the index into &struct
   --  gpio_v2_line_request.offsets.
   --
   type gpio_v2_line_values is record
      bits : uint64;
      mask : uint64;
   end record with Alignment => 8;

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


--/**
-- * struct gpio_v2_line_attribute - a configurable attribute of a line
-- * @id: attribute identifier with value from &enum gpio_v2_line_attr_id
-- * @padding: reserved for future use and must be zero filled
-- * @flags: if id is %GPIO_V2_LINE_ATTR_ID_FLAGS, the flags for the GPIO
-- * line, with values from &enum gpio_v2_line_flag, such as
-- * %GPIO_V2_LINE_FLAG_ACTIVE_LOW, %GPIO_V2_LINE_FLAG_OUTPUT etc, added
-- * together.  This overrides the default flags contained in the &struct
-- * gpio_v2_line_config for the associated line.
-- * @values: if id is %GPIO_V2_LINE_ATTR_ID_OUTPUT_VALUES, a bitmap
-- * containing the values to which the lines will be set, with each bit
-- * number corresponding to the index into &struct
-- * gpio_v2_line_request.offsets.
-- * @debounce_period_us: if id is %GPIO_V2_LINE_ATTR_ID_DEBOUNCE, the
-- * desired debounce period, in microseconds
-- */
--struct gpio_v2_line_attribute {
--	__u32 id;
--	__u32 padding;
--	union {
--		__aligned_u64 flags;
--		__aligned_u64 values;
--		__u32 debounce_period_us;
--	};
--};

--/**
-- * struct gpio_v2_line_config_attribute - a configuration attribute
-- * associated with one or more of the requested lines.
-- * @attr: the configurable attribute
-- * @mask: a bitmap identifying the lines to which the attribute applies,
-- * with each bit number corresponding to the index into &struct
-- * gpio_v2_line_request.offsets.
-- */
--struct gpio_v2_line_config_attribute {
--	struct gpio_v2_line_attribute attr;
--	__aligned_u64 mask;
--};

--/**
-- * struct gpio_v2_line_config - Configuration for GPIO lines
-- * @flags: flags for the GPIO lines, with values from &enum
-- * gpio_v2_line_flag, such as %GPIO_V2_LINE_FLAG_ACTIVE_LOW,
-- * %GPIO_V2_LINE_FLAG_OUTPUT etc, added together.  This is the default for
-- * all requested lines but may be overridden for particular lines using
-- * @attrs.
-- * @num_attrs: the number of attributes in @attrs
-- * @padding: reserved for future use and must be zero filled
-- * @attrs: the configuration attributes associated with the requested
-- * lines.  Any attribute should only be associated with a particular line
-- * once.  If an attribute is associated with a line multiple times then the
-- * first occurrence (i.e. lowest index) has precedence.
-- */
--struct gpio_v2_line_config {
--	__aligned_u64 flags;
--	__u32 num_attrs;
--	/* Pad to fill implicit padding and reserve space for future use. */
--	__u32 padding[5];
--	struct gpio_v2_line_config_attribute attrs[GPIO_V2_LINE_NUM_ATTRS_MAX];
--};

--/**
-- * struct gpio_v2_line_request - Information about a request for GPIO lines
-- * @offsets: an array of desired lines, specified by offset index for the
-- * associated GPIO chip
-- * @consumer: a desired consumer label for the selected GPIO lines such as
-- * "my-bitbanged-relay"
-- * @config: requested configuration for the lines.
-- * @num_lines: number of lines requested in this request, i.e. the number
-- * of valid fields in the %GPIO_V2_LINES_MAX sized arrays, set to 1 to
-- * request a single line
-- * @event_buffer_size: a suggested minimum number of line events that the
-- * kernel should buffer.  This is only relevant if edge detection is
-- * enabled in the configuration. Note that this is only a suggested value
-- * and the kernel may allocate a larger buffer or cap the size of the
-- * buffer. If this field is zero then the buffer size defaults to a minimum
-- * of @num_lines * 16.
-- * @padding: reserved for future use and must be zero filled
-- * @fd: if successful this field will contain a valid anonymous file handle
-- * after a %GPIO_GET_LINE_IOCTL operation, zero or negative value means
-- * error
-- */
--struct gpio_v2_line_request {
--	__u32 offsets[GPIO_V2_LINES_MAX];
--	char consumer[GPIO_MAX_NAME_SIZE];
--	struct gpio_v2_line_config config;
--	__u32 num_lines;
--	__u32 event_buffer_size;
--	/* Pad to fill implicit padding and reserve space for future use. */
--	__u32 padding[5];
--	__s32 fd;
--};

--/**
-- * struct gpio_v2_line_info - Information about a certain GPIO line
-- * @name: the name of this GPIO line, such as the output pin of the line on
-- * the chip, a rail or a pin header name on a board, as specified by the
-- * GPIO chip, may be empty (i.e. name[0] == '\0')
-- * @consumer: a functional name for the consumer of this GPIO line as set
-- * by whatever is using it, will be empty if there is no current user but
-- * may also be empty if the consumer doesn't set this up
-- * @offset: the local offset on this GPIO chip, fill this in when
-- * requesting the line information from the kernel
-- * @num_attrs: the number of attributes in @attrs
-- * @flags: flags for this GPIO line, with values from &enum
-- * gpio_v2_line_flag, such as %GPIO_V2_LINE_FLAG_ACTIVE_LOW,
-- * %GPIO_V2_LINE_FLAG_OUTPUT etc, added together.
-- * @attrs: the configuration attributes associated with the line
-- * @padding: reserved for future use
-- */
--struct gpio_v2_line_info {
--	char name[GPIO_MAX_NAME_SIZE];
--	char consumer[GPIO_MAX_NAME_SIZE];
--	__u32 offset;
--	__u32 num_attrs;
--	__aligned_u64 flags;
--	struct gpio_v2_line_attribute attrs[GPIO_V2_LINE_NUM_ATTRS_MAX];
--	/* Space reserved for future use. */
--	__u32 padding[4];
--};

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
   GPIO_GET_CHIPINFO_IOCTL : constant ioctl_type := (dir => read, code => 16#b4#,
                                          nr => 1, size => gpiochip_info'Size);
   GPIO_GET_LINEINFO_UNWATCH_IOCTL : constant ioctl_type := (dir => rw, code => 16#b4#,
                                          nr => 16#0c#, size => uint32'Size);
   --
   --  v2 ioctl()s
   --
--#define GPIO_V2_GET_LINEINFO_IOCTL _IOWR(0xB4, 0x05, struct gpio_v2_line_info)
--#define GPIO_V2_GET_LINEINFO_WATCH_IOCTL _IOWR(0xB4, 0x06, struct gpio_v2_line_info)
--#define GPIO_V2_GET_LINE_IOCTL _IOWR(0xB4, 0x07, struct gpio_v2_line_request)
--#define GPIO_V2_LINE_SET_CONFIG_IOCTL _IOWR(0xB4, 0x0D, struct gpio_v2_line_config)
--#define GPIO_V2_LINE_GET_VALUES_IOCTL _IOWR(0xB4, 0x0E, struct gpio_v2_line_values)
--#define GPIO_V2_LINE_SET_VALUES_IOCTL _IOWR(0xB4, 0x0F, struct gpio_v2_line_values)
private
   --
   --  GPIO Object.
   --
   type Linux_GPIO_record is new GPIO_record with record
      dir : direction;
   end record;
end BBS.embed.GPIO.Linux;
