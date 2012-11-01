--
--
--  .Name of module: GWindows-HID.ads
--  .Project:        Human Interface Devices
--  .Notes:          Only supported on XP or higher
--  .Abstract:       To get raw input data from input devices
--  .Author(s):      Andre van Splunter
--
--

--  Include Ada specification file(s)
with Ada.Strings.Unbounded;

--  Include GNAVI specification file(s)
with GWindows;
with GWindows.Base;
with GWindows.Types;

package GWindows.HID is

   RIM_TYPEMOUSE    : constant := 0;
   RIM_TYPEKEYBOARD : constant := 1;
   RIM_TYPEHID      : constant := 2;

   --  .Definition: Structure to collect a list of raw input devices
   --  * DType, RIM_TYPEHID, RIM_TYPEKEYBOARD, RIM_TYPEMOUSE
   --  * Device, a Windows handle pointing to the raw input device
   --  * Name, the name of the devoce
   --  * Class, type of device from registry
   --  * DeviceDesc, descriptive text from registry
   --  * Manufactor, manufactor strin from registry
   --  Mouse:
   --  * Id, the identifier for the mouse device
   --  * NrButtons, the number of buttons for the mouse
   --  * SampleRate, Number of data points per second.
   --    This information may not be applicable for every mouse device.
   --  * HorWheel, TRUE if the mouse has a wheel for horizontal scrolling
   --    This member is only supported under Windows Vista and later versions.
   --  Keyboard:
   --  * KType, type of the keyboard
   --  * KSubType, subtype of the keyboard
   --  * KMode, scan code mode
   --  * NrFunctionKeys, number of function keys on the keyboards
   --  * NrIndicators, number of LED indicators on the keyboards
   --  * TotalNrKeys, total number of keys on the keyboards
   --  Notes: For the keyboard, the Usage Page is 1 and the Usage is 6.
   --  Other HID devices:
   --  * VendorId, the vendor identifier for the HID
   --  * ProductId, the product identifier for the HID
   --  * VersionNr, the version number of the HID
   --  * UsagePage, top-level collection Usage Page for the device
   --  * Usage, top-level collection Usage for the device
   type Hid_Device_Info (DType : Natural) is
   record
      --  RID_DEVICE_INFO structure and registry info
      Device     : GWindows.Types.Handle;
      Name       : GWindows.GString_Unbounded;
      Class      : GWindows.GString_Unbounded;
      DeviceDesc : GWindows.GString_Unbounded;
      Manufactor : GWindows.GString_Unbounded;
      case DType is
         when RIM_TYPEMOUSE =>  --  RID_DEVICE_INFO_MOUSE structure
            Id             : Natural;
            NrButtons      : Natural;
            SampleRate     : Natural;
            HorWheel       : Boolean;
         when RIM_TYPEKEYBOARD =>  --  RID_DEVICE_INFO_KEYBOARD structure
            KType          : Natural;
            KSubType       : Natural;
            KMode          : Natural;
            NrFunctionKeys : Natural;
            NrIndicators   : Natural;
            TotalNrKeys    : Natural;
         when RIM_TYPEHID =>  --  RID_DEVICE_INFO_HID structure
            VendorId       : Natural;
            ProductId      : Natural;
            VersionNr      : Natural;
            UsagePage      : Natural;
            Usage          : Natural;
         when others => null;
      end case;
   end record;
   --  .Definition: Pointer to an instance of Hid_Device_Info.
   type Hid_Device_Info_Ptr is access all Hid_Device_Info;

   --  .Definition: Array of instances of Hid_Device_Info.
   type Hid_Array is
      array (Integer range <>) of Hid_Device_Info_Ptr;
   --  .Definition: Pointer to an instance of Hid_Array.
   type Hid_Array_Ptr is access all Hid_Array;

   --  .Definition: Structure to collect the raw input
   --  * DType, RIM_TYPEHID, RIM_TYPEKEYBOARD, RIM_TYPEMOUSE
   --  * Device, a Windows handle pointing to the raw input device
   type Hid_Raw_Input (DType : Natural) is
   record
      --  What was needed from RAWINPUTHEADER structure
      Device     : GWindows.Types.Handle;
      RType      : Natural;
      case DType is
         when RIM_TYPEMOUSE =>  --  RAWMOUSE structure
            MFlags       : Natural;
            MButtonFlags : Natural;
            MButtonData  : Natural;
            MRawButtons  : Natural;
            MLastX       : Integer;
            MLastY       : Integer;
            MExtraInfo   : Natural;
         when RIM_TYPEKEYBOARD =>
            KMakeCode    : Natural;
            KFlags       : Natural;
            KReserved    : Natural;
            KVKey        : Natural;
            KMessage     : Natural;
            KExtraInfo   : Natural;
         when RIM_TYPEHID =>
            HSizeHid     : Natural;   --  Size of each HID input block
            HCount       : Natural;   --  Nr of HID input blocks
            HRawData     : Ada.Strings.Unbounded.Unbounded_String;
         when others => null;
      end case;
   end record;

   procedure Delete
      (HIDs : in out Hid_Array_Ptr);

   procedure Get_Raw_Input_Device_List
      (HIDs : in out Hid_Array_Ptr);

   --   Device Type          Usage Page     Usage ID
   --   --------------------------------------------
   --   Pointer                 0x01           0x01
   --   Mouse                   0x01           0x02
   --   Joystick                0x01           0x04
   --   Game pad                0x01           0x05
   --   Keyboard                0x01           0x06
   --   Keypad                  0x01           0x07
   --   System Control          0x01           0x80
   --   Consumer Audio Control  0x0C           0x01
   procedure Register_Raw_Input_Device
      (Window : in GWindows.Base.Base_Window_Type;
       UsagePage : in Integer;
       Usage     : in Integer);

   procedure Unregister_Raw_Input_Device
      (Window    : in GWindows.Base.Base_Window_Type;
       UsagePage : in Integer;
       Usage     : in Integer);

   procedure Get_Raw_Input_Data
      (HID     : in     GWindows.Types.Handle;
       RawData : in     GWindows.Types.Handle;
       RawInfo : in out Hid_Raw_Input);

   procedure Def_Raw_Input_Proc
      (RawData : in GWindows.Types.Handle);

end GWindows.HID;
