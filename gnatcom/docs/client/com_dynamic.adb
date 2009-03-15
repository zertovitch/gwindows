with win32.objbase; use win32.objbase;
with win32.winerror; use win32.winerror;
with Win32.OleAuto; use Win32.OleAuto;
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with System;

procedure COM_Dynamic is
   pragma Linker_Options ("-lole32");
   pragma Linker_Options ("-loleaut32");

   --  Type conversion functions

   function To_Pointer_To_IDispatch is
      new Ada.Unchecked_Conversion (Win32.PVOID, Win32.OleAuto.LPDISPATCH);

   function To_LPOLESTR is
      new Ada.Unchecked_Conversion (system.address, Win32.Objbase.LPOLESTR);

   function To_LPCOLESTR is
      new Ada.Unchecked_Conversion (system.address, Win32.Objbase.LPCOLESTR);

   function To_LPIID is
      new Ada.Unchecked_Conversion (system.address, Win32.Objbase.LPIID);

   --  IID for IUnkown
   IID_IDispatch  : aliased Win32.Objbase.IID :=
     (16#00020400#, 16#0000#, 16#0000#,
      (char'Val (16#C0#), char'Val (16#00#),
       char'Val (16#00#), char'Val (16#00#),
       char'Val (16#00#), char'Val (16#00#),
       char'Val (16#00#), char'Val (16#46#)));

   IID_NULL  : aliased Win32.Objbase.IID :=
     (16#00000000#, 16#0000#, 16#0000#,
      (char'Val (16#00#), char'Val (16#00#),
       char'Val (16#00#), char'Val (16#00#),
       char'Val (16#00#), char'Val (16#00#),
       char'Val (16#00#), char'Val (16#00#)));

   RetPointer        : aliased Win32.PVOID;
   hr                : Win32.Winerror.HRESULT;
   refcount          : Win32.ULONG;
   com_error         : exception;
   Class_ID          : aliased CLSID;
   Dispatch_Ref      : Win32.OleAuto.LPDISPATCH;
begin
   Put_Line ("Initialize Com Libraries");
   hr := CoInitialize (System.Null_Address);

   if hr /= S_OK then
      raise com_error;
   end if;

   Put_Line ("Get CLSID using PROGID");

   Hr := CLSIDFromProgID
     (To_LPCOLESTR
      (To_C (Wide_String'("GNATCOMLibrary.GNATCOMClass"))'Address),
      Class_ID'Unchecked_Access);

   if hr /= S_OK then
      raise com_error;
   end if;

   Put_Line ("CoCreateInstance of object and request IDispatch");
   hr := CoCreateInstance (Class_ID'Unchecked_Access,
                           null,
                           Win32.DWORD (CLSCTX_ALL),
                           IID_IDispatch'Unchecked_Access,
                           RetPointer'Unchecked_Access);
   if hr /= S_OK then
      raise com_error;
   end if;

   Put_Line ("Convert return pointer to pointer to IDispatch");
   Dispatch_Ref := To_Pointer_To_IDispatch (RetPointer);

   declare
      Method_Name  : aliased Win32.ObjBase.LPOLESTR :=
        To_LPOLESTR (To_C (Wide_String'("Beep"))'Address);
      ID           : aliased Win32.OleAuto.DISPID;

      No_Arguments : aliased Win32.OleAuto.DISPPARAMS :=
        (null, null, 0, 0);
      Result       : aliased Win32.OleAuto.Variant;
      Excep_Info   : aliased Win32.OleAuto.EXCEPINFO;
      Arg_Err      : aliased Win32.UINT;
   begin
      Put_Line ("Look up ID of method to call");

      Hr := Dispatch_Ref.lpvtbl.GetIDsOfNames (Dispatch_Ref,
                                               IID_NULL'Unchecked_Access,
                                               Method_Name'Unchecked_Access,
                                               1,
                                               0,
                                               ID'Unchecked_Access);

      Hr := Dispatch_Ref.lpvtbl.Invoke (Dispatch_Ref,
                                        ID,
                                        IID_NULL'Unchecked_Access,
                                        0,
                                        DISPATCH_METHOD,
                                        No_Arguments'Unchecked_Access,
                                        Result'Unchecked_Access,
                                        Excep_Info'Unchecked_Access,
                                        Arg_Err'Unchecked_Access);
   end;

   Put_Line ("Release IDispatch Interface");
   refcount := Dispatch_Ref.lpvtbl.Release (Dispatch_Ref);

   Put_Line ("Uninit COM Libs");
   CoUninitialize;

end COM_Dynamic;
