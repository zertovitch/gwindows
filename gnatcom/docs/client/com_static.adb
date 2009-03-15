with win32.objbase; use win32.objbase;
with win32.winerror; use win32.winerror;
with Win32.OleAuto; use Win32.OleAuto;
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with System;

procedure COM_Static is
   pragma Linker_Options ("-lole32");
   pragma Linker_Options ("-loleaut32");

   --  CLSID of COM object to create
   --  {45F9F481-787C-11D3-821C-52544C1913DE}

   CLSID_GNATCOMClass : aliased CLSID :=
     (16#45F9F481#, 16#787C#, 16#11D3#,
       (char'Val (16#82#), char'Val (16#1C#),
        char'Val (16#52#), char'Val (16#54#),
        char'Val (16#4C#), char'Val (16#19#),
        char'Val (16#13#), char'Val (16#DE#)));


   --  Element Name             : IGNATMessage
   --  Element Documentation    : Message dual interface
   --  Element Kind             : Dual Interface
   --  {45F9F482-787C-11D3-821C-52544C1913DE}
   IID_IGNATMessage : aliased IID :=
     (16#45F9F482#, 16#787C#, 16#11D3#,
       (char'Val (16#82#), char'Val (16#1C#),
        char'Val (16#52#), char'Val (16#54#),
        char'Val (16#4C#), char'Val (16#19#),
        char'Val (16#13#), char'Val (16#DE#)));

   --------------------------------------
   -- Define Interface of IGNATMessage --
   --------------------------------------

   --  Forward refrences for IGNATMessage
   type IGNATMessage;
   type IGNATMessageVtbl;
   type Pointer_To_IGNATMessage is access all IGNATMessage;
   type Pointer_To_IGNATMessageVtbl is access all IGNATMessageVtbl;

   --  Inherited methods from IUnknown
   type af_IGNATMessage_QueryInterface is access
     function (This   : access IGNATMessage;
               riid   : in     REFIID;
               ppvObj : access Win32.PVOID)
     return Win32.Winerror.HRESULT;
   pragma Convention (StdCall, af_IGNATMessage_QueryInterface);

   type af_IGNATMessage_AddRef is access
     function (This : access IGNATMessage)
     return Win32.ULONG;
   pragma Convention (StdCall, af_IGNATMessage_AddRef);

   type af_IGNATMessage_Release is access
     function (This : access IGNATMessage)
     return Win32.ULONG;
   pragma Convention (StdCall, af_IGNATMessage_Release);

   --  Methods of IGNATMessage

   type af_IGNATMessage_Beep is access
     function (This : access IGNATMessage)
     return Win32.Winerror.HRESULT;
   pragma Convention (StdCall, af_IGNATMessage_Beep);
   --  Sounds an audio alert

   type af_IGNATMessage_MessageBox is access
     function (This    : access IGNATMessage;
               Message : in     Win32.OleAuto.BSTR)
     return Win32.Winerror.HRESULT;
   pragma Convention (StdCall, af_IGNATMessage_MessageBox);
   --  Display Message Box

   --  Create interface to table of functions
   type IGNATMessage is
      record
         lpVtbl : Pointer_To_IGNATMessageVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IGNATMessage);

   --  Create table of functions - VTBL
   --  IGNATMessage inherits from both IDispatch wich inherits from
   --  IUnknown. Since the IDispatch methods are not used when binding
   --  to the interface directly as they exist only for dynamic binding,
   --  we do not create access functions definitions for them, but we
   --  still need to create space in the table of pointers to functions
   --  for the method's pointers as they will be filled in when creating
   --  the object.

   type IGNATMessageVtbl is
      record
         --  IUnknown methods
         QueryInterface   : af_IGNATMessage_QueryInterface;
         AddRef           : af_IGNATMessage_AddRef;
         Release          : af_IGNATMessage_Release;

         --  IDispatch methods
         GetTypeInfoCount : Win32.PVOID;
         GetTypeInfo      : Win32.PVOID;
         GetIDsOfNames    : Win32.PVOID;
         Invoke           : Win32.PVOID;

         --  IGNATMessage methods
         Beep             : af_IGNATMessage_Beep;
         MessageBox       : af_IGNATMessage_MessageBox;
      end record;
   pragma Convention (C_Pass_By_Copy, IGNATMessageVtbl);

   --  Element Name             : IGNATStat
   --  Element Documentation    : Statistics GNATCOMclass object interface
   --  Element Kind             : Interface
   --  {45F9F483-787C-11D3-821C-52544C1913DE}
   IID_IGNATStat : aliased IID :=
     (16#45F9F483#, 16#787C#, 16#11D3#,
       (char'Val (16#82#), char'Val (16#1C#),
        char'Val (16#52#), char'Val (16#54#),
        char'Val (16#4C#), char'Val (16#19#),
        char'Val (16#13#), char'Val (16#DE#)));

   -----------------------------------
   -- Define Interface of IGNATStat --
   -----------------------------------

   --  Forward refrences for IGNATMessage

   type IGNATStat;
   type IGNATStatVtbl;
   type Pointer_To_IGNATStat is access all IGNATStat;
   type Pointer_To_IGNATStatVtbl is access all IGNATStatVtbl;

   --  Inherited methods from IUnknown
   type af_IGNATStat_QueryInterface is access
     function (This   : access IGNATStat;
               riid   : in     REFIID;
               ppvObj : access Win32.PVOID)
     return Win32.Winerror.HRESULT;
   pragma Convention (StdCall, af_IGNATStat_QueryInterface);

   type af_IGNATStat_AddRef is access
     function (This : access IGNATStat)
     return Win32.ULONG;
   pragma Convention (StdCall, af_IGNATStat_AddRef);

   type af_IGNATStat_Release is access
     function (This : access IGNATStat)
     return Win32.ULONG;
   pragma Convention (StdCall, af_IGNATStat_Release);

   --  Methods of IGNATStat

   type af_IGNATStat_Calls is access
     function (This          : access IGNATStat;
               NumberOfTimes : access Interfaces.C.Int)
     return Win32.Winerror.HRESULT;
   pragma Convention (StdCall, af_IGNATStat_Calls);
   --  Return number of times methods of the IGNATMessage interface
   --  were called

   type IGNATStat is
      record
         lpVtbl : Pointer_To_IGNATStatVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IGNATStat);

   type IGNATStatVtbl is
      record
         QueryInterface : af_IGNATStat_QueryInterface;
         AddRef         : af_IGNATStat_AddRef;
         Release        : af_IGNATStat_Release;
         Calls          : af_IGNATStat_Calls;
      end record;
   pragma Convention (C_Pass_By_Copy, IGNATStatVtbl);

   --  Type conversion functions

   function To_Pointer_To_IGNATMessage is
      new Ada.Unchecked_Conversion (Win32.PVOID, Pointer_To_IGNATMessage);

   function To_Pointer_To_IGNATStat is
      new Ada.Unchecked_Conversion (Win32.PVOID, Pointer_To_IGNATStat);

   function To_PCWSTR is
      new Ada.Unchecked_Conversion (system.address, Win32.PCWSTR);

   RetPointer        : aliased Win32.PVOID;
   hr                : Win32.Winerror.HRESULT;
   refcount          : Win32.ULONG;
   com_error         : exception;

   GnatMessage_Ref   : Pointer_To_IGNATMessage;
   Message           : Win32.OleAuto.BSTR;

   GnatStat_Ref      : Pointer_To_IGNATStat;
   Number_Of_Calls   : aliased Interfaces.C.int;

begin
   Put_Line ("Initialize Com Libraries");
   hr := CoInitialize (System.Null_Address);
   if hr /= S_OK then
      raise com_error;
   end if;

   Put_Line ("CoCreateInstance");
   hr := CoCreateInstance (CLSID_GNATCOMClass'Unchecked_Access,
                           null,
                           Win32.DWORD (CLSCTX_ALL),
                           IID_IGNATMessage'Unchecked_Access,
                           RetPointer'Unchecked_Access);
   if hr /= S_OK then
      raise com_error;
   end if;

   Put_Line ("Convert return pointer to pointer to IGNATMessage");
   GnatMessage_Ref := To_Pointer_To_IGNATMessage (RetPointer);

   Put_Line ("IGNATMessage->Beep");
   hr := GnatMessage_Ref.lpvtbl.Beep (GnatMessage_Ref);

   if hr /= S_OK then
      raise com_error;
   end if;

   Put_Line ("IGNATMessage->MessageBox");
   Message :=
     SysAllocString (To_PCWSTR (To_C (Wide_String'("Hello World!"))'Address));

   hr := GnatMessage_Ref.lpvtbl.MessageBox (GnatMessage_Ref, Message);

   SysFreeString (Message);

   if hr /= S_OK then
      raise com_error;
   end if;

   Put_Line ("IGNATMessage->QueryInterface to get IGNATStat");
   hr := GnatMessage_Ref.lpvtbl.QueryInterface (GnatMessage_Ref,
                                                IID_IGNATStat'Unchecked_Access,
                                                RetPointer'Unchecked_Access);
   if hr /= S_OK then
      raise com_error;
   end if;

   Put_Line ("Convert return pointer to pointer to IGNATStat");
   GnatStat_Ref := To_Pointer_To_IGNATStat (RetPointer);

   Put_Line ("IGNATStat->Calls");
   hr := GnatStat_Ref.lpvtbl.Calls (GnatStat_Ref,
                                    Number_Of_Calls'Unchecked_Access);
   if hr /= S_OK then
      raise com_error;
   end if;

   Put_Line ("The IGNATMessage Interface was called" &
             Interfaces.C.int'Image (Number_Of_Calls) &
             " times.");

   Put_Line ("Release IGNATMessage");
   refcount := GnatMessage_Ref.lpvtbl.Release (GnatMessage_Ref);

   Put_Line ("Release IGNATStat");
   refcount := GnatStat_Ref.lpvtbl.Release (GnatStat_Ref);

   Put_Line ("Uninit COM Libs");
   CoUninitialize;

end COM_Static;
