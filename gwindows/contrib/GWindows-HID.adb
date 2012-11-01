--  Include Ada specification file(s)
--  with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces;                        use Interfaces;

with System;                            use System;

with Win32;                             use Win32;
with Win32.Winbase;                     use Win32.Winbase;
with Win32.Winnt;
with Win32.Windef;

--  Include GNAVI specification file(s)
with GWindows.GStrings;                 use GWindows.GStrings;
with GWindows.Registry;                 use GWindows.Registry;
with GWindows.Types;                    use GWindows.Types;

package body GWindows.HID is

   subtype String2 is String (1 .. 2);
   function S2N is new Ada.Unchecked_Conversion (String2, Unsigned_16);
   subtype String4 is String (1 .. 4);
   function S4N is new Ada.Unchecked_Conversion (String4, Unsigned_32);
   function S4I is new Ada.Unchecked_Conversion (String4, Integer);

   --  .Definition: Structure to collect a list of raw input devices
   --  * Device, a Windows Handle pointing to the raw input device
   --  * DType, RIM_TYPEHID, RIM_TYPEKEYBOARD, RIM_TYPEMOUSE
   type Raw_Input_Device_List_Type is
   record
      Device : GWindows.Types.Handle;
      DType  : Natural;
   end record;

   --  .Definition: Array of instances of Raw_Input_Device.
   type Raw_Input_Device_Arr is
      array (Integer range <>) of Raw_Input_Device_List_Type;

   --  .Definition: Pointer to an instance of Raw_Input_Device_Arr.
   type Raw_Input_Device_Arrs is access all Raw_Input_Device_Arr;

   --  .Definition: Structure to simulate a mouse event using SendInmput
--    type Send_Mouse_Input_Type is
--    record
--       InputType : Natural;
--       DeltaX    : Integer;
--       DeltaY    : Integer;
--       MouseData : Natural;
--       Flags     : Natural;
--       SendTime  : Natural;
--       ExtraInfo : Natural;
--    end record;

--    function SendInput
--       (NrInputs : in     Natural;
--        Inputs   : access Send_Mouse_Input_Type;
--        Size     : in     Integer)
--       return Natural;
--    pragma Import (StdCall, SendInput, "SendInput");

--    function GetRawInputDeviceList
--       (RIDList    : in     Raw_Input_Device_Arrs;
--        NumDevices : access Integer;
--        Size       : in     Integer)
--       return Integer;
--    pragma Import (StdCall, GetRawInputDeviceList, "GetRawInputDeviceList");

   type GetRawInputDeviceListPtr is access function
         (RIDList    : in     Raw_Input_Device_Arrs;
          NumDevices : access Integer;
          Size       : in     Integer)
         return Integer;
   pragma Convention (StdCall, GetRawInputDeviceListPtr);

--    function GetRawInputDeviceInfoW
--       (Device     : in     GWindows.Types.Handle;
--        Command    : in     Natural;
--        Data       : access GString;
--        Size       : access Integer)
--       return Integer;
--    pragma Import (StdCall, GetRawInputDeviceInfoW,
--       "GetRawInputDeviceInfoW");

   type GetRawInputDeviceInfoWPtr is access function
         (Device     : in     GWindows.Types.Handle;
          Command    : in     Natural;
          Data       : access GString;
          Size       : access Integer)
         return Integer;
   pragma Convention (StdCall, GetRawInputDeviceInfoWPtr);

--    function GetRawInputDeviceInfoA
--       (Device     : in     GWindows.Types.Handle;
--        Command    : in     Natural;
--        Data       : access String;
--        Size       : access Integer)
--       return Integer;
--    pragma Import (StdCall, GetRawInputDeviceInfoA,
--       "GetRawInputDeviceInfoA");

   type GetRawInputDeviceInfoAPtr is access function
         (Device     : in     GWindows.Types.Handle;
          Command    : in     Natural;
          Data       : access String;
          Size       : access Integer)
         return Integer;
   pragma Convention (StdCall, GetRawInputDeviceInfoAPtr);

   type Raw_Input_Device_Select_Type is
   record
      UsagePage : Unsigned_16;
      Usage     : Unsigned_16;
      Flags     : Natural;
      Target    : GWindows.Types.Handle;
   end record;
--   type Raw_Input_Device_Select_Arr is
--      array (Integer range <>) of Raw_Input_Device_Select_Type;

   type RegisterRawInputDevicesPtr is access function
         (RID       : access Raw_Input_Device_Select_Type;
          NumDevs   : in     Integer;
          Size      : in     Integer)
          return Boolean;
   pragma Convention (StdCall, RegisterRawInputDevicesPtr);

--    function RegisterRawInputDevices
--      (RID       : access Raw_Input_Device_Select_Arr;
--       (RID       : access Raw_Input_Device_Select_Type;
--        NumDevs   : in     Integer;
--        Size      : in     Integer)
--        return Boolean;
--    pragma Import (StdCall, RegisterRawInputDevices,
--      "RegisterRawInputDevices");

   type GetRawInputDataPtr is access function
      (RawInput  : in     GWindows.Types.Handle;
       Command   : in     Integer;
       Data      : access String;
       Size      : access Integer;
       SizeHdr   : in     Integer)
      return Integer;
   pragma Convention (StdCall, GetRawInputDataPtr);

--    function GetRawInputData
--       (RawInput  : in     GWindows.Types.Handle;
--        Command   : in     Integer;
--        Data      : access String;
--        Size      : access Integer;
--        SizeHdr   : in     Integer)
--       return Integer;
--    pragma Import (StdCall, GetRawInputData,
--      "GetRawInputData");

   type DefRawInputProcPtr is access procedure
      (RawInput : access String;
       NrInput  : in     Integer;
       SizeHdr  : in     Integer);
   pragma Convention (StdCall, DefRawInputProcPtr);

--    procedure DefRawInputProc
--       (RawInput : access String;
--        NrInput  : in     Integer;
--        SizeHdr  : in     Integer);
--    pragma Import (StdCall, DefRawInputProc, "DefRawInputProc");

   --  All pointers to dynamicly load the functions
   Dll                     : Win32.Winnt.HANDLE;
   GetRawInputDeviceList   : GetRawInputDeviceListPtr   := null;
   GetRawInputDeviceInfoW  : GetRawInputDeviceInfoWPtr  := null;
   GetRawInputDeviceInfoA  : GetRawInputDeviceInfoAPtr  := null;
   RegisterRawInputDevices : RegisterRawInputDevicesPtr := null;
   GetRawInputData         : GetRawInputDataPtr         := null;
   DefRawInputProc         : DefRawInputProcPtr         := null;

--   RIM_INPUT          : constant := 16#00000000#;
--   RIM_INPUTSINK      : constant := 16#00000001#;

   RID_INPUT          : constant := 16#10000003#;
--   RID_HEADER         : constant := 16#10000005#;

--   RIDI_PREPARESEDATA : constant := 16#20000005#;
   RIDI_DEVICENAME    : constant := 16#20000007#;
   RIDI_DEVICEINFO    : constant := 16#2000000B#;

   RIDEV_REMOVE       : constant := 16#00000001#;
--   RIDEV_EXCLUDE      : constant := 16#00000010#;
--   RIDEV_PAGEONLY     : constant := 16#00000020#;
--   RIDEV_NOLEGACY     : constant := 16#00000030#;
--   RIDEV_INPUTSINK    : constant := 16#00000100#;
--   RIDEV_CAPTUREMOUSE : constant := 16#00000200#;
--   RIDEV_NOHOTKEYS    : constant := 16#00000200#;
--   RIDEV_APPKEYS      : constant := 16#00000400#;

   --  SendInput: dwType of INPUT
--    INPUT_MOUSE : constant := 0;
   --  INPUT_KEYBOARD : constant := 1;
   --  INPUT_HARDWARE : constant := 2;
   --  SendInput: Flags with INPUT_MOUSE
--    MOUSEEVENTF_ABSOLUTE    : constant := 16#8000#;
--    MOUSEEVENTF_MOVE        : constant := 16#0001#;
--    MOUSEEVENTF_LEFTDOWN    : constant := 16#0002#;
--    MOUSEEVENTF_LEFTUP      : constant := 16#0004#;
--    MOUSEEVENTF_RIGHTDOWN   : constant := 16#0008#;
--    MOUSEEVENTF_RIGHTUP     : constant := 16#0010#;
--    MOUSEEVENTF_MIDDLEDOWN  : constant := 16#0020#;
--    MOUSEEVENTF_MIDDLEUP    : constant := 16#0040#;
--    MOUSEEVENTF_VIRTUALDESK : constant := 16#4000#;
--    MOUSEEVENTF_WHEEL       : constant := 16#1000#;
--    MOUSEEVENTF_HWHEEL      : constant := 16#2000#;
--    MOUSEEVENTF_XDOWN       : constant := 16#0080#;
--    MOUSEEVENTF_XUP         : constant := 16#0100#;
   --  MOUSEEVENTF_MOVE_NOCOALESCE (vista or up)
--    XBUTTON1                : constant := 1;
--    XBUTTON2                : constant := 2;

   --  RAW_INPUT, Flags with RAWMOUSE
   MOUSE_MOVE_RELATIVE      : constant := 0;
   MOUSE_MOVE_ABSOLUTE      : constant := 1;
   MOUSE_VIRTUAL_DESKTOP    : constant := 2;
   MOUSE_ATTRIBUTES_CHANGED : constant := 4;
   --  RAW_INPUT, Button Flags with RAWMOUSE
--    RI_MOUSE_LEFT_BUTTON_DOWN   : constant := 1;
--    RI_MOUSE_LEFT_BUTTON_UP     : constant := 2;
--    RI_MOUSE_RIGHT_BUTTON_DOWN  : constant := 4;
--    RI_MOUSE_RIGHT_BUTTON_UP    : constant := 8;
--    RI_MOUSE_MIDDLE_BUTTON_DOWN : constant := 16;
--    RI_MOUSE_MIDDLE_BUTTON_UP   : constant := 32;
--    RI_MOUSE_BUTTON_4_DOWN      : constant := 64;  --  (XBUTTON1)
--    RI_MOUSE_BUTTON_4_UP        : constant := 128;
--    RI_MOUSE_BUTTON_5_DOWN      : constant := 256;  --  (XBUTTON2)
--    RI_MOUSE_BUTTON_5_UP        : constant := 512;
--    RI_MOUSE_WHEEL              : constant := 1024;

   procedure Get_HID_Name
      (HID : in out Hid_Device_Info)
   is
      Sze : aliased Integer;
      Err : Integer;
   begin
      Err := GetRawInputDeviceInfoA.all (HID.Device, RIDI_DEVICENAME, null,
                                         Sze'Access);
      pragma Warnings (Off);
      if Character_Mode_Identifier = "W" then
         declare
            Name : GString (1 .. Sze);
         begin
            Err := GetRawInputDeviceInfoW.all (HID.Device, RIDI_DEVICENAME,
               Name'Unrestricted_Access, Sze'Access);
            HID.Name :=  To_GString_Unbounded (Name (1 .. Err));
         end;
      else
         declare
            Name : String (1 .. Sze);
         begin
            Err := GetRawInputDeviceInfoA.all (HID.Device, RIDI_DEVICENAME,
               Name'Unrestricted_Access, Sze'Access);
            HID.Name :=
               To_GString_Unbounded (To_GString_From_String (Name (1 .. Err)));
         end;
      end if;
      pragma Warnings (On);
   end Get_HID_Name;

   procedure Get_HID_Info
      (HID : in out Hid_Device_Info)
   is
      Sze : aliased Integer;
      Err : Integer;
   begin
      Err := GetRawInputDeviceInfoA.all (HID.Device, RIDI_DEVICEINFO, null,
         Sze'Access);
      declare
         Info : String (1 .. Sze) := (others => Character'Val (0));
      begin
         Info (1) := Character'Val (Sze mod 256);
         Info (5) := Character'Val (HID.DType mod 256);
         Err := GetRawInputDeviceInfoA.all (HID.Device, RIDI_DEVICEINFO,
            Info'Unrestricted_Access, Sze'Access);
         case HID.DType is
            when RIM_TYPEMOUSE    =>
               HID.Id         := Natural (S4N (Info (9 .. 12)));
               HID.NrButtons  := Natural (S4N (Info (13 .. 16)));
               HID.SampleRate := Natural (S4N (Info (17 .. 20)));
               HID.HorWheel   := Info (21) /= Character'Val (0);
            when RIM_TYPEKEYBOARD =>
               HID.KType          := Natural (S4N (Info (9 .. 12)));
               HID.KSubType       := Natural (S4N (Info (13 .. 16)));
               HID.KMode          := Natural (S4N (Info (17 .. 20)));
               HID.NrFunctionKeys := Natural (S4N (Info (21 .. 24)));
               HID.NrIndicators   := Natural (S4N (Info (25 .. 28)));
               HID.TotalNrKeys    := Natural (S4N (Info (29 .. 32)));
            when RIM_TYPEHID      =>
               HID.VendorId       := Natural (S4N (Info (9 .. 12)));
               HID.ProductId      := Natural (S4N (Info (13 .. 16)));
               HID.VersionNr      := Natural (S4N (Info (17 .. 20)));
               HID.UsagePage      := Natural (S2N (Info (21 .. 22)));
               HID.Usage          := Natural (S2N (Info (23 .. 24)));
            when others => null;
         end case;
      end;
   end Get_HID_Info;

   procedure Get_HID_Registry_Info
      (HID : in out Hid_Device_Info)
   is
      DeviceName   : GString := To_GString_From_Unbounded (HID.Name);
      Reg_Key_Name : GString := "SYSTEM\CurrentControlSet\Enum\";
      Reg_Root_Key : Integer := GWindows.Registry.HKEY_LOCAL_MACHINE;

      function From_Registry
         (Name    : in GString;
          SubKey  : in GString := "";
          Default : in GString := "")
         return GString
      is
      begin
         return Get_Value
            (Key_Name => Reg_Key_Name & SubKey,
             Name     => Name,
             Root_Key => Reg_Root_Key);
      exception
         when others =>
            return Default;
      end From_Registry;

      Idx : array (1 .. 5) of Integer := (others => -1);
      Tmp : Integer;
   begin
      Idx (1) := DeviceName'First + 4;
      Idx (5) := DeviceName'Last;
      Tmp := 2;
      for I in Idx (1) .. Idx (5) loop
         if DeviceName (I) = '#' then
            Idx (Tmp) := I + 1;
            Tmp := Tmp + 1;
         end if;
      end loop;
      if Tmp >= 5 then
         HID.Class := To_GString_Unbounded (From_Registry ("Class",
            DeviceName (Idx (1) .. Idx (2) - 2) & "\" &
            DeviceName (Idx (2) .. Idx (3) - 2) & "\" &
            DeviceName (Idx (3) .. Idx (4) - 2)));
         HID.DeviceDesc := To_GString_Unbounded (From_Registry ("DeviceDesc",
            DeviceName (Idx (1) .. Idx (2) - 2) & "\" &
            DeviceName (Idx (2) .. Idx (3) - 2) & "\" &
            DeviceName (Idx (3) .. Idx (4) - 2)));
         HID.Manufactor := To_GString_Unbounded (From_Registry ("Mfg",
            DeviceName (Idx (1) .. Idx (2) - 2) & "\" &
            DeviceName (Idx (2) .. Idx (3) - 2) & "\" &
            DeviceName (Idx (3) .. Idx (4) - 2)));
      end if;
   end Get_HID_Registry_Info;

   procedure Delete
      (HIDs : in out Hid_Array_Ptr)
   is
      procedure Free is new Ada.Unchecked_Deallocation
         (Hid_Array, Hid_Array_Ptr);
   begin
      Free (HIDs);
   end Delete;

   procedure Get_Raw_Input_Device_List
      (HIDs : in out Hid_Array_Ptr)
   is
      procedure Free is new Ada.Unchecked_Deallocation
         (Raw_Input_Device_Arr, Raw_Input_Device_Arrs);

      Err : Integer;
      Num : aliased Integer;
      Lst : Raw_Input_Device_Arrs := null;
   begin
      Delete (HIDs);
      if GetRawInputDeviceList /= null then
         Err := GetRawInputDeviceList.all (null, Num'Access, 8);
         Lst  := new Raw_Input_Device_Arr'
           (1 .. Num => (GWindows.Types.Null_Handle, 0));
         HIDs := new Hid_Array'(1 .. Num => null);
         Err := GetRawInputDeviceList.all (Lst, Num'Access, 8);
         for I in 1 .. Num loop
            HIDs (I) := new Hid_Device_Info (Lst (I).DType);
            HIDs (I).Device := Lst (I).Device;
            Get_HID_Name (HIDs (I).all);
            Get_HID_Info (HIDs (I).all);
            Get_HID_Registry_Info (HIDs (I).all);
         end loop;
         Free (Lst);
      end if;
   end Get_Raw_Input_Device_List;

   procedure Register_Raw_Input_Device
      (Window : in GWindows.Base.Base_Window_Type;
       UsagePage : in Integer;
       Usage     : in Integer)
   is
      Dummy     : Boolean;
      Devs      : aliased Raw_Input_Device_Select_Type;
--      Devs      : aliased Raw_Input_Device_Select_Arr (1 .. 1);
   begin
      Devs := (Unsigned_16 (UsagePage), Unsigned_16 (Usage),
      --  RIDEV_INPUTSINK,  --   + RIDEV_NOLEGACY,
         0, GWindows.Base.Handle (Window));
      Dummy := RegisterRawInputDevices.all (Devs'Access, 1, 12);
   end Register_Raw_Input_Device;

   procedure Unregister_Raw_Input_Device
      (Window : in GWindows.Base.Base_Window_Type;
       UsagePage : in Integer;
       Usage     : in Integer)
   is
      Dummy     : Boolean;
      Devs      : aliased Raw_Input_Device_Select_Type;
--      Devs      : aliased Raw_Input_Device_Select_Arr (1 .. 1);
      pragma Unreferenced (Window);
   begin
      Devs := (Unsigned_16 (UsagePage), Unsigned_16 (Usage),
               RIDEV_REMOVE, GWindows.Types.Null_Handle);
              --  No window Handle! else it won't work
      Dummy := RegisterRawInputDevices.all (Devs'Access, 1, 12);
   end Unregister_Raw_Input_Device;

   procedure Get_Raw_Input_Data
      (HID     : in     GWindows.Types.Handle;
       RawData : in     GWindows.Types.Handle;
       RawInfo : in out Hid_Raw_Input)
   is
      Ret : Integer;
      Sze : aliased Integer;
   begin
      Ret := GetRawInputData.all (RawData, RID_INPUT, null, Sze'Access, 16);
      declare
         Info : String (1 .. Sze) := (others => Character'Val (0));
      begin
         Ret := GetRawInputData.all (RawData, RID_INPUT,
            Info'Unrestricted_Access, Sze'Access, 16);
         RawInfo.RType := Natural (S4N (Info (1 .. 4)));
         RawInfo.Device := GWindows.Types.To_Handle
           (GWindows.Types.Wparam (S4N (Info (9 .. 12))));
         if RawInfo.RType /= RawInfo.DType or HID /= RawInfo.Device then
--            Put_Line ("Wrong device " & RawInfo.Device'Img);
            return;
         end if;
         case RawInfo.RType is
            when RIM_TYPEMOUSE    =>
               RawInfo.MFlags       := Natural (S2N (Info (17 .. 18)));
               RawInfo.MButtonFlags := Natural (S2N (Info (21 .. 22)));
               RawInfo.MButtonData  := Natural (S2N (Info (23 .. 24)));
               RawInfo.MRawButtons  := Natural (S4N (Info (25 .. 28)));
               RawInfo.MLastX       := S4I (Info (29 .. 32));
               RawInfo.MLastY       := S4I (Info (33 .. 36));
               RawInfo.MExtraInfo   := Natural (S4N (Info (37 .. 40)));
--               Put (RawInfo.Device'Img);
--               Put (RawInfo.MRawButtons'Img);
--               Put (RawInfo.MButtonFlags'Img);
               case RawInfo.MFlags is
                  when MOUSE_MOVE_RELATIVE      =>
                     null;
--                     Put_Line (" MOUSE_MOVE_RELATIVE" &
--                     RawInfo.MLastX'Img & RawInfo.MLastY'Img);
                  when MOUSE_MOVE_ABSOLUTE      =>
                     null;
--                     Put_Line (" MOUSE_MOVE_ANSOLUTE");
                  when MOUSE_VIRTUAL_DESKTOP    =>
                     null;
--                     Put_Line (" MOUSE_VIRTUAL_DESKTOP");
                  when MOUSE_ATTRIBUTES_CHANGED =>
                     null;
--                     Put_Line (" MOUSE_ATTRIBUTES_CHANGE");
                  when others =>
--                     Put_Line ("MFlags=" & RawInfo.MFlags'Img);
                     null;
               end case;
            when RIM_TYPEKEYBOARD =>
               RawInfo.KMakeCode    := Natural (S2N (Info (17 .. 18)));
               RawInfo.KFlags       := Natural (S2N (Info (19 .. 20)));
               RawInfo.KReserved    := Natural (S2N (Info (21 .. 22)));
               RawInfo.KVKey        := Natural (S2N (Info (23 .. 24)));
               RawInfo.KMessage     := Natural (S4N (Info (25 .. 28)));
               RawInfo.KExtraInfo   := Natural (S4N (Info (29 .. 32)));
            when RIM_TYPEHID =>
               RawInfo.HSizeHid     := Natural (S4N (Info (17 .. 20)));
               RawInfo.HCount       := Natural (S4N (Info (21 .. 24)));
               RawInfo.HRawData     :=
                  Ada.Strings.Unbounded.To_Unbounded_String (Info (25 .. Sze));
               --  Or create a byte pointer for the rest?
            when others => null;
         end case;
      end;
   end Get_Raw_Input_Data;

   procedure Def_Raw_Input_Proc
      (RawData : in GWindows.Types.Handle)
   is
      Ret : Integer;
      Sze : aliased Integer;
   begin
      Ret := GetRawInputData.all (RawData, RID_INPUT, null, Sze'Access, 16);
      declare
         Info : String (1 .. Sze);
      begin
         Ret := GetRawInputData.all (RawData, RID_INPUT,
            Info'Unrestricted_Access, Sze'Access, 16);
         DefRawInputProc.all (Info'Unrestricted_Access, 1, 16);
      end;
   end Def_Raw_Input_Proc;

   function GetProcAddress
      (HModule    : Win32.Windef.HINSTANCE;
       LpProcName : System.Address)
      return GetRawInputDeviceListPtr;
   function GetProcAddress
      (HModule    : Win32.Windef.HINSTANCE;
       LpProcName : System.Address)
      return GetRawInputDeviceInfoWPtr;
   function GetProcAddress
      (HModule    : Win32.Windef.HINSTANCE;
       LpProcName : System.Address)
      return GetRawInputDeviceInfoAPtr;
   function GetProcAddress
      (HModule    : Win32.Windef.HINSTANCE;
       LpProcName : System.Address)
      return RegisterRawInputDevicesPtr;
   function GetProcAddress
      (HModule    : Win32.Windef.HINSTANCE;
       LpProcName : System.Address)
      return GetRawInputDataPtr;
   function GetProcAddress
      (HModule    : Win32.Windef.HINSTANCE;
       LpProcName : System.Address)
      return DefRawInputProcPtr;

   pragma Import (StdCall, GetProcAddress, "GetProcAddress");

   Name_GetRawInputDeviceList   : constant String :=
      "GetRawInputDeviceList" & ASCII.NUL;
   Name_GetRawInputDeviceInfoW  : constant String :=
      "GetRawInputDeviceInfoW" & ASCII.NUL;
   Name_GetRawInputDeviceInfoA  : constant String :=
      "GetRawInputDeviceInfoA" & ASCII.NUL;
   Name_RegisterRawInputDevices : constant String :=
      "RegisterRawInputDevices" & ASCII.NUL;
   Name_GetRawInputData         : constant String :=
      "GetRawInputData" & ASCII.NUL;
   Name_DefRawInputProc         : constant String :=
      "DefRawInputProc" & ASCII.NUL;

begin
   Dll := LoadLibrary (Addr ("user32.dll" & ASCII.NUL));
   GetRawInputDeviceList   := GetProcAddress (Dll,
      Name_GetRawInputDeviceList'Address);
   GetRawInputDeviceInfoW  := GetProcAddress (Dll,
      Name_GetRawInputDeviceInfoW'Address);
   GetRawInputDeviceInfoA  := GetProcAddress (Dll,
      Name_GetRawInputDeviceInfoA'Address);
   RegisterRawInputDevices := GetProcAddress (Dll,
      Name_RegisterRawInputDevices'Address);
   GetRawInputData         := GetProcAddress (Dll,
      Name_GetRawInputData'Address);
   DefRawInputProc         := GetProcAddress (Dll,
      Name_DefRawInputProc'Address);
end GWindows.HID;
