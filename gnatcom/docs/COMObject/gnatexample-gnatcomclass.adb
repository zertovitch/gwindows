with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;

with Interfaces.C.Pointers;

with Win32.Rpcdce;
with Win32.Winbase;
with Win32.Winerror;
with Win32.WinUser;

package body GNATExample.GNATCOMClass is

   package C renames Interfaces.C;

   procedure Free (Pointer : in System.Address);
   --  Deallocates interfaces and the COM object when needed from memory

   function To_Interface_Pointer is
      new Ada.Unchecked_Conversion (System.Address, Interface_Pointer);
   --  Convert address to pointer

   function To_Object_Pointer is
      new Ada.Unchecked_Conversion (System.Address, GNATCOMClass_Pointer);
   --  Convert address to Object

   procedure Message_Box (Title, Message : String);
   --  Display message box

   type PWChar_t is access all Interfaces.C.WChar_t;
   --  Pointer to a WChar_t


   function To_PWChar_t is
      new Ada.Unchecked_Conversion (System.Address, PWChar_t);
   package WChar_Array_Pointer is new Interfaces.C.Pointers
         (index              => Interfaces.C.Size_t,
          element            => Interfaces.C.WChar_t,
          element_array      => Interfaces.C.WChar_array,
          default_terminator => Interfaces.C.Wide_Nul);
   --  Used in conversion of WChat_t* to Ada Strings

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Controlled_Information) is
      use type Win32.OleAuto.LPTYPEINFO;

      Result : Win32.ULONG;
      HR     : Win32.Objbase.HRESULT;
      Lib    : aliased Win32.OleAuto.LPTYPELIB;
   begin
      if This.Type_Information = null then
         HR := Win32.OleAuto.LoadRegTypeLib
           (LIBID_GNATCOMLibrary'Access,
            1, 0, 0,
            Lib'Unchecked_Access);

         HR := Lib.Lpvtbl.GetTypeInfoOfGuid
           (Lib,
            IID_IGNATMessage'Access,
            This.Type_Information'Unchecked_Access);

         Result := Lib.Lpvtbl.Release (Lib);
      end if;

   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Controlled_Information) is
      use type Win32.OleAuto.LPTYPEINFO;

      Result : Win32.ULONG;
   begin
      if This.Type_Information /= null then
         Result :=
           This.Type_Information.Lpvtbl.Release (This.Type_Information);
         This.Type_Information := null;
      end if;
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out Controlled_Information) is
      use type Win32.OleAuto.LPTYPEINFO;

      Result : Win32.ULONG;
   begin
      if This.Type_Information /= null then
         Result := This.Type_Information.Lpvtbl.AddRef (This.Type_Information);
      end if;
   end Adjust;

   -----------------------------
   -- IUnknown_QueryInterface --
   -----------------------------

   function IUnknown_QueryInterface
     (This      : access Interface;
      riid      : in     Win32.Objbase.REFIID;
      ppvObject : access Win32.PVOID)
     return HRESULT
   is
      use type Win32.Rpcdce.Guid;

      New_Interface : aliased Interface_Pointer;
      lResult       : Win32.LONG;
      Result        : Win32.ULONG;
      Object        : GNATCOMClass_Pointer := To_Object_Pointer (This.CoClass);
   begin
      if riid.all = IID_IUnknown then
         --  Since IUnknown is the parent of every interface, just
         --  return back a pointer to this interface with an additional
         --  reference count and the client will use it as if it was
         --  IUnknown
         ppvObject.all := This.all'Address;
         Result := IUnknown_AddRef (This);
      elsif Riid.all = IID_IDispatch then
         New_Interface := new Interface;
         New_Interface.CoClass := This.CoClass;
         lResult :=
           Win32.Winbase.InterlockedIncrement (Object.Ref_Count'Access);
         New_Interface.Vtbl := IGNATMessage_Vtbl'Address;
         ppvObject.all := New_Interface.all'Address;
      elsif Riid.all = IID_IGNATMessage then
         New_Interface := new Interface;
         New_Interface.CoClass := This.CoClass;
         lResult :=
           Win32.Winbase.InterlockedIncrement (Object.Ref_Count'Access);
         New_Interface.Vtbl := IGNATMessage_Vtbl'Address;
         ppvObject.all := New_Interface.all'Address;
      elsif Riid.all = IID_IGNATStat then
         New_Interface := new Interface;
         New_Interface.CoClass := This.CoClass;
         lResult :=
           Win32.Winbase.InterlockedIncrement (Object.Ref_Count'Access);
         New_Interface.Vtbl := IGNATStat_Vtbl'Address;
         ppvObject.all := New_Interface.all'Address;
      else
         ppvObject.all := System.Null_Address;
         return Win32.Winerror.E_NOINTERFACE;
      end if;

      return Win32.Winerror.S_OK;
   end IUnknown_QueryInterface;

   ---------------------
   -- IUnknown_AddRef --
   ---------------------

   function IUnknown_AddRef (This : access Interface)
     return Win32.ULONG
   is
      lResult : Win32.Long;
      Object  : GNATCOMClass_Pointer := To_Object_Pointer (This.CoClass);
   begin
      --  InterlockedIncrement is a thread protected Win32 API function
      --  to increment a long

      --  Interface reference increment
      lResult := Win32.Winbase.InterlockedIncrement (This.Ref_Count'Access);

      --  Object reference increment
      lResult :=
        Win32.Winbase.InterlockedIncrement (Object.Ref_Count'Access);

      return Win32.ULONG (This.Ref_Count);

   end IUnknown_AddRef;

   ----------------------
   -- IUnknown_Release --
   ----------------------

   function IUnknown_Release (This : access Interface)
     return Win32.ULONG
   is
      use type Interfaces.C.Long;

      lResult : Win32.Long;
      Object  : GNATCOMClass_Pointer := To_Object_Pointer (This.CoClass);
   begin
      --  InterlockedDecrement is a thread protected Win32 API function
      --  to decrement a long
      lResult :=
        Win32.Winbase.InterlockedDecrement (Object.Ref_Count'Access);

      if Win32.Winbase.InterlockedDecrement (This.Ref_Count'Access) /= 0 then
         --  This.Ref_Count should never be less then 0, so if it is
         --  we get a CONSTRAINT_ERROR exception from Ada
         return Win32.ULONG (This.Ref_Count);
      else
         --  Last reference to Interface so free it
         Free (This.all'Address);
         return 0;
      end if;

   end IUnknown_Release;

   ----------
   -- Free --
   ----------

   procedure Free (Pointer : in System.Address) is
      use type Interfaces.C.Long;

      procedure Free is
         new Ada.Unchecked_Deallocation (Interface, Interface_Pointer);

      procedure Free is
         new Ada.Unchecked_Deallocation (GNATCOMClass_Object,
                                         GNATCOMClass_Pointer);

      X       : Interface_Pointer;
      lResult : Win32.Long;
   begin
      X := To_Interface_Pointer (Pointer);

      declare
         Object  : GNATCOMClass_Pointer := To_Object_Pointer (X.CoClass);
      begin
         if Object.Ref_Count < 1 then
            --  All interfaces to this COM object have been released
            --  so free the COM objects data completing the release
            --  of the COM object from memory
            Free (Object);
            lResult :=
              Win32.Winbase.InterlockedDecrement (Component_Count'Access);
         end if;
      end;

      --  Free Interface
      Free (X);

      --  Check to see if container should close down
      Can_Close;

   end Free;

   --------------------------------
   -- IDispatch_GetTypeInfoCount --
   --------------------------------

   function IDispatch_GetTypeInfoCount
     (This    : access Interface;
      pctinfo : access Win32.UINT)
     return HRESULT
   is
   begin
      Pctinfo.all := 1;
      return Win32.Winerror.S_OK;
   end IDispatch_GetTypeInfoCount;

   ---------------------------
   -- IDispatch_GetTypeInfo --
   ---------------------------

   function IDispatch_GetTypeInfo
     (This    : access Interface;
      itinfo  : in     Win32.UINT;
      lcid    : in     Win32.Winnt.LCID;
      pptinfo : access Win32.OleAuto.LPTYPEINFO)
     return HRESULT
   is
      use type Win32.UINT;
      use type Win32.OleAuto.LPTYPEINFO;

      Object  : GNATCOMClass_Pointer := To_Object_Pointer (This.CoClass);
      Result  : Win32.ULONG;
   begin
      if itinfo /= 0 then
         return Win32.Winerror.DISP_E_BADINDEX;
      end if;

      if Object.Data.Type_Information /= null then
         Result := Object.Data.Type_Information.Lpvtbl.AddRef
           (Object.Data.Type_Information);
      else
         Pptinfo.all := null;
         return Win32.Winerror.E_FAIL;
      end if;

      Pptinfo.all := Object.Data.Type_Information;

      return Win32.Winerror.S_OK;

   end IDispatch_GetTypeInfo;

   -----------------------------
   -- IDispatch_GetIDsOfNames --
   -----------------------------

   function IDispatch_GetIDsOfNames
     (This      : access Interface;
      riid      : access Win32.Objbase.IID;
      rgszNames : access Win32.Objbase.LPOLESTR;
      cNames    : in     Win32.UINT;
      lcid      : in     Win32.Winnt.LCID;
      rgdispid  : access Win32.OleAuto.DISPID)
     return HRESULT
   is
      Object  : GNATCOMClass_Pointer := To_Object_Pointer (This.CoClass);
   begin

      --  Use Win32 API GetDispIDsOfNames to implement IDispatch_Invoke
      --  through the type library

      return
        HRESULT (Win32.OleAuto.DispGetIDsOfNames
                 (Object.Data.Type_Information,
                  RgszNames,
                  cNames,
                  RgDispID));
   end IDispatch_GetIDsOfNames;

   ----------------------
   -- IDispatch_Invoke --
   ----------------------

   function IDispatch_Invoke
     (This         : access Interface;
      dispidMember : in     Win32.OleAuto.DISPID;
      riid         : access Win32.Objbase.IID;
      lcid         : in     Win32.Winnt.LCID;
      wFlags       : in     Win32.WORD;
      pdispparams  : access Win32.OleAuto.DISPPARAMS;
      pvarResult   : access Win32.OleAuto.VARIANT;
      pexcepinfo   : access Win32.OleAuto.EXCEPINFO;
      puArgErr     : access Win32.UINT)
     return HRESULT
   is
      Object  : GNATCOMClass_Pointer := To_Object_Pointer (This.CoClass);
   begin

      --  Use Win32 API DispInvoke to implement IDispatch_Invoke
      --  through the type library

      return
        HRESULT (Win32.OleAuto.DispInvoke
                 (This.all'Address,
                  Object.Data.Type_Information,
                  dispIdMember,
                  wFlags,
                  PDispParams,
                  PVarResult,
                  PExcepInfo,
                  PuArgErr));
   end IDispatch_Invoke;

   -----------------------
   -- IGNATMessage_Beep --
   -----------------------

   function IGNATMessage_Beep (This : access Interface)
     return HRESULT
   is
      RetValue : Win32.BOOL;
      Object   : GNATCOMClass_Pointer := To_Object_Pointer (This.CoClass);
   begin
      RetValue := Win32.WinUser.MessageBeep (Win32.WinUser.MB_ICONEXCLAMATION);
      Object.Count := Object.Count + 1;
      return Win32.Winerror.S_OK;
   end IGNATMessage_Beep;

   -----------------------------
   -- IGNATMessage_MessageBox --
   -----------------------------

   function IGNATMessage_MessageBox
     (This    : access Interface;
      Message : in     Win32.OleAuto.BSTR)
     return HRESULT
   is
      use type Win32.OleAuto.BSTR;

      Object   : GNATCOMClass_Pointer := To_Object_Pointer (This.CoClass);
   begin
      if Message /= null then
         declare
            C_Message   : C.WChar_Array :=
              WChar_Array_Pointer.Value (WChar_Array_Pointer.pointer
                  (To_PWChar_T (Message.all'Address)));

            Ada_Message : String :=
              Ada.Characters.Handling.To_String (C.To_Ada (C_Message));
         begin
            Message_Box ("GNATExample COM Object",
                         Ada_Message);
         end;
      end if;

      Object.Count := Object.Count + 1;
      return Win32.Winerror.S_OK;
   end IGNATMessage_MessageBox;


   ---------------------
   -- IGNATStat_Calls --
   ---------------------

   function IGNATStat_Calls
     (This          : access Interface;
      NumberOfTimes : access Interfaces.C.Int)
     return HRESULT
   is
      Object   : GNATCOMClass_Pointer := To_Object_Pointer (This.CoClass);
   begin
      Message_Box ("Calls", Integer'Image (Object.Count));
      NumberOfTimes.all := C.Int (Object.Count);

      return Win32.Winerror.S_OK;
   end IGNATStat_Calls;

   ----------------
   -- New_Object --
   ----------------

   function New_Object return Interface_Pointer
   is
      New_Interface : aliased Interface_Pointer;
      New_Object    : aliased GNATCOMClass_Pointer;
   begin
      New_Interface := new Interface;
      New_Object    := new GNATCOMClass_Object;
      New_Interface.CoClass := New_Object.all'Address;
      New_Interface.Vtbl := IGNATMessage_Vtbl'Address;
      return New_Interface;
   end New_Object;

   -----------------
   -- Message_Box --
   -----------------

   procedure Message_Box (Title, Message : String) is
      function To_LPCSTR is
         new Ada.Unchecked_Conversion (System.Address, Win32.LPCSTR);

      BoxTitle    : C.Char_Array := C.To_C (Title);
      BoxMessage  : C.Char_Array := C.To_C (Message);
      RetValue : Win32.Int;
   begin
      RetValue := Win32.WinUser.MessageBox (System.Null_Address,
                                            To_LPCSTR (BoxMessage'Address),
                                            To_LPCSTR (BoxTitle'Address),
                                            Win32.WinUser.MB_YESNO);
   end Message_Box;


end GNATExample.GNATCOMClass;








