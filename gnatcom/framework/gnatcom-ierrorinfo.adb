------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                   G N A T C O M . I E R R O R I N F O                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2006 David Botton                   --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. It is distributed in the hope that it will be useful,  but WITHOUT --
-- ANY WARRANTY;  without  even the  implied warranty of MERCHANTABILITY or --
-- FITNESS FOR A PARTICULAR PURPOSE.    See the GNU General  Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- More information about GNATCOM and the most current version can          --
-- be located on the web at http://www.gnavi.org/gnatcom                    --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Interfaces.C;

with GNATCOM.Errors;
with GNATCOM.GUID;
with GNATCOM.BSTR;

package body GNATCOM.IErrorInfo is

   type IErrorInfo;
   type ICreateErrorInfo;
   type ISupportErrorInfo;

   type Pointer_To_IErrorInfo is access all IErrorInfo;
   type Pointer_To_ICreateErrorInfo is access all ICreateErrorInfo;
   type Pointer_To_ISupportErrorInfo is access all ISupportErrorInfo;

   --  Element Name          : IErrorInfo
   --  Element Type          : Interface

   IID_IErrorInfo : aliased constant GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{1CF2B120-547D-101B-8E65-08002B2BD119}");

   type af_IErrorInfo_QueryInterface is access
     function (This   : access IErrorInfo;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IErrorInfo_QueryInterface);

   type af_IErrorInfo_AddRef is access
     function (This : access IErrorInfo)
              return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IErrorInfo_AddRef);

   type af_IErrorInfo_Release is access
     function (This : access IErrorInfo)
              return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IErrorInfo_Release);

   type af_IErrorInfo_GetGUID is access
     function (This  : access IErrorInfo;
               pGUID : GNATCOM.Types.Pointer_To_GUID)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IErrorInfo_GetGUID);

   type af_IErrorInfo_GetSource is access
     function (This        : access IErrorInfo;
               pBstrSource : GNATCOM.Types.Pointer_To_BSTR)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IErrorInfo_GetSource);

   type af_IErrorInfo_GetDescription is access
     function (This             : access IErrorInfo;
               pBstrDescription : GNATCOM.Types.Pointer_To_BSTR)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IErrorInfo_GetDescription);

   type af_IErrorInfo_GetHelpFile is access
     function (This          : access IErrorInfo;
               pBstrHelpFile : GNATCOM.Types.Pointer_To_BSTR)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IErrorInfo_GetHelpFile);

   type af_IErrorInfo_GetHelpContext is access
     function (This           : access IErrorInfo;
               pdwHelpContext : GNATCOM.Types.Pointer_To_unsigned_long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IErrorInfo_GetHelpContext);

   type IErrorInfoVtbl;
   type Pointer_To_IErrorInfoVtbl is access all IErrorInfoVtbl;

   type IErrorInfo is
      record
         Vtbl : Pointer_To_IErrorInfoVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IErrorInfo);

   type IErrorInfoVtbl is
      record
         QueryInterface : af_IErrorInfo_QueryInterface;
         AddRef         : af_IErrorInfo_AddRef;
         Release        : af_IErrorInfo_Release;
         GetGUID        : af_IErrorInfo_GetGUID;
         GetSource      : af_IErrorInfo_GetSource;
         GetDescription : af_IErrorInfo_GetDescription;
         GetHelpFile    : af_IErrorInfo_GetHelpFile;
         GetHelpContext : af_IErrorInfo_GetHelpContext;
      end record;
   pragma Convention (C_Pass_By_Copy, IErrorInfoVtbl);

--     function To_Pointer_To_IErrorInfo is
--        new Ada.Unchecked_Conversion
--       (GNATCOM.Types.Pointer_To_Void, Pointer_To_IErrorInfo);

   --  Element Name          : ICreateErrorInfo
   --  Element Type          : Interface

--     IID_ICreateErrorInfo : aliased GNATCOM.Types.GUID :=
--       GNATCOM.GUID.To_GUID ("{22F03340-547D-101B-8E65-08002B2BD119}");

   type af_ICreateErrorInfo_QueryInterface is access
     function (This   : access ICreateErrorInfo;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ICreateErrorInfo_QueryInterface);

   type af_ICreateErrorInfo_AddRef is access
     function (This : access ICreateErrorInfo)
              return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ICreateErrorInfo_AddRef);

   type af_ICreateErrorInfo_Release is access
     function (This : access ICreateErrorInfo)
              return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ICreateErrorInfo_Release);

   type af_ICreateErrorInfo_SetGUID is access
     function (This  : access ICreateErrorInfo;
               rguid : GNATCOM.Types.Pointer_To_GUID)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ICreateErrorInfo_SetGUID);

   type af_ICreateErrorInfo_SetSource is access
     function (This     : access ICreateErrorInfo;
               szSource : GNATCOM.Types.LPWSTR)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ICreateErrorInfo_SetSource);

   type af_ICreateErrorInfo_SetDescription is access
     function (This          : access ICreateErrorInfo;
               szDescription : GNATCOM.Types.LPWSTR)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ICreateErrorInfo_SetDescription);

   type af_ICreateErrorInfo_SetHelpFile is access
     function (This       : access ICreateErrorInfo;
               szHelpFile : GNATCOM.Types.LPWSTR)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ICreateErrorInfo_SetHelpFile);

   type af_ICreateErrorInfo_SetHelpContext is access
     function (This          : access ICreateErrorInfo;
               dwHelpContext : Interfaces.C.unsigned_long)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ICreateErrorInfo_SetHelpContext);

   type ICreateErrorInfoVtbl;
   type Pointer_To_ICreateErrorInfoVtbl is access all ICreateErrorInfoVtbl;

   type ICreateErrorInfo is
      record
         Vtbl : Pointer_To_ICreateErrorInfoVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, ICreateErrorInfo);

   type ICreateErrorInfoVtbl is
      record
         QueryInterface : af_ICreateErrorInfo_QueryInterface;
         AddRef         : af_ICreateErrorInfo_AddRef;
         Release        : af_ICreateErrorInfo_Release;
         SetGUID        : af_ICreateErrorInfo_SetGUID;
         SetSource      : af_ICreateErrorInfo_SetSource;
         SetDescription : af_ICreateErrorInfo_SetDescription;
         SetHelpFile    : af_ICreateErrorInfo_SetHelpFile;
         SetHelpContext : af_ICreateErrorInfo_SetHelpContext;
      end record;
   pragma Convention (C_Pass_By_Copy, ICreateErrorInfoVtbl);

--     function To_Pointer_To_ICreateErrorInfo is
--        new Ada.Unchecked_Conversion
--       (GNATCOM.Types.Pointer_To_Void, Pointer_To_ICreateErrorInfo);

   --  Element Name          : ISupportErrorInfo
   --  Element Type          : Interface

   IID_ISupportErrorInfo : aliased constant GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{DF0B3D60-548F-101B-8E65-08002B2BD119}");

   type af_ISupportErrorInfo_QueryInterface is access
     function (This   : access ISupportErrorInfo;
               riid   : GNATCOM.Types.Pointer_To_GUID;
               ppvObj : GNATCOM.Types.Pointer_To_Pointer_To_Void)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ISupportErrorInfo_QueryInterface);

   type af_ISupportErrorInfo_AddRef is access
     function (This : access ISupportErrorInfo)
              return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ISupportErrorInfo_AddRef);

   type af_ISupportErrorInfo_Release is access
     function (This : access ISupportErrorInfo)
              return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ISupportErrorInfo_Release);

   type af_ISupportErrorInfo_InterfaceSupportsErrorInfo is access
     function (This : access ISupportErrorInfo;
               riid : GNATCOM.Types.Pointer_To_GUID)
              return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall,
                        af_ISupportErrorInfo_InterfaceSupportsErrorInfo);

   type ISupportErrorInfoVtbl;
   type Pointer_To_ISupportErrorInfoVtbl is access all ISupportErrorInfoVtbl;

   type ISupportErrorInfo is
      record
         Vtbl : Pointer_To_ISupportErrorInfoVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, ISupportErrorInfo);

   type ISupportErrorInfoVtbl is
      record
         QueryInterface             : af_ISupportErrorInfo_QueryInterface;
         AddRef                     : af_ISupportErrorInfo_AddRef;
         Release                    : af_ISupportErrorInfo_Release;
         InterfaceSupportsErrorInfo :
           af_ISupportErrorInfo_InterfaceSupportsErrorInfo;
      end record;
   pragma Convention (C_Pass_By_Copy, ISupportErrorInfoVtbl);

   function To_Pointer_To_ISupportErrorInfo is
      new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_ISupportErrorInfo);

   -----------------------
   -- Create_IErrorInfo --
   -----------------------

   procedure Create_IErrorInfo
     (Description     : in String;
      Source_PROGID   : in String              := "";
      Associated_GUID : in GNATCOM.Types.GUID  := GNATCOM.Types.GUID_NULL;
      Help_Context    : in GNATCOM.Types.DWORD := 0;
      Help_File_Path  : in String              := "")
   is
      use GNATCOM.Iinterface;
      use GNATCOM.BSTR;

      pcerrinfo        : aliased Pointer_To_ICreateErrorInfo;
      CError_Interface : GNATCOM.Iinterface.Interface_Type;
      Error_Interface  : GNATCOM.Iinterface.Interface_Type;

      function CreateErrorInfo
        (pperrinfo : access Pointer_To_ICreateErrorInfo)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, CreateErrorInfo, "CreateErrorInfo");

      procedure SetErrorInfo
        (Reserverd : Integer := 0;
         perrinfo  : GNATCOM.Types.Pointer_To_IUnknown);
      pragma Import (StdCall, SetErrorInfo, "SetErrorInfo");

      Local_GUID       : aliased GNATCOM.Types.GUID := Associated_GUID;
      BSTR_Source      : constant GNATCOM.Types.BSTR :=
        To_BSTR (Source_PROGID);
      BSTR_Description : constant GNATCOM.Types.BSTR :=
        To_BSTR (Description);
      BSTR_Path        : constant GNATCOM.Types.BSTR :=
        To_BSTR (Help_File_Path);
   begin
      GNATCOM.Errors.Error_Check (CreateErrorInfo (pcerrinfo'Access));

      Attach (CError_Interface, pcerrinfo.all'Address);

      GNATCOM.Errors.Error_Check
        (pcerrinfo.Vtbl.SetGUID (pcerrinfo,
                                 Local_GUID'Unchecked_Access));

      GNATCOM.Errors.Error_Check
        (pcerrinfo.Vtbl.SetSource (pcerrinfo, BSTR_Source));
      GNATCOM.BSTR.Free (BSTR_Source);

      GNATCOM.Errors.Error_Check
        (pcerrinfo.Vtbl.SetDescription (pcerrinfo, BSTR_Description));
      GNATCOM.BSTR.Free (BSTR_Description);

      GNATCOM.Errors.Error_Check
        (pcerrinfo.Vtbl.SetHelpFile (pcerrinfo,
                                     BSTR_Path));
      GNATCOM.BSTR.Free (BSTR_Path);

      GNATCOM.Errors.Error_Check
        (pcerrinfo.Vtbl.SetHelpContext
         (pcerrinfo,
          Interfaces.C.unsigned_long (Help_Context)));

      Set_IID (Error_Interface, IID_IErrorInfo);
      Query (Error_Interface, CError_Interface);
      SetErrorInfo (perrinfo => Pointer (Error_Interface));
   end Create_IErrorInfo;

   --------------------
   -- Get_IErrorInfo --
   --------------------

   function Get_IErrorInfo return String is
      use Ada.Strings.Unbounded;
      Desc    : Ada.Strings.Unbounded.Unbounded_String;
      ProgID  : Ada.Strings.Unbounded.Unbounded_String;
      AGUID   : GNATCOM.Types.GUID;
      Context : GNATCOM.Types.DWORD;
      Path    : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Get_IErrorInfo (Desc, ProgID, AGUID, Context, Path);
      return To_String (Desc);
   end Get_IErrorInfo;

   --------------------
   -- Get_IErrorInfo --
   --------------------

   procedure Get_IErrorInfo
     (Description     : out Ada.Strings.Unbounded.Unbounded_String;
      Source_PROGID   : out Ada.Strings.Unbounded.Unbounded_String;
      Associated_GUID : out GNATCOM.Types.GUID;
      Help_Context    : out GNATCOM.Types.DWORD;
      Help_File_Path  : out Ada.Strings.Unbounded.Unbounded_String)
   is
      use Ada.Strings.Unbounded;
      use GNATCOM.Iinterface;
      use GNATCOM.BSTR;

      function GetErrorInfo
        (Reserverd : in     Integer := 0;
         pperrinfo : access Pointer_To_IErrorInfo)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, GetErrorInfo, "GetErrorInfo");

      pErrorInfo       : aliased Pointer_To_IErrorInfo;
      Error_Interface  : GNATCOM.Iinterface.Interface_Type;

      Assoc_GUID       : aliased GNATCOM.Types.GUID;
      BSTR_Source      : aliased GNATCOM.Types.BSTR;
      BSTR_Description : aliased GNATCOM.Types.BSTR;
      BSTR_Path        : aliased GNATCOM.Types.BSTR;
      Context          : aliased Interfaces.C.unsigned_long;
   begin
      if
        GNATCOM.Errors.Logical_Check
        (GetErrorInfo (pperrinfo => pErrorInfo'Access))
      then
         Attach (Error_Interface, pErrorInfo.all'Address);

         GNATCOM.Errors.Error_Check
           (pErrorInfo.Vtbl.GetGUID (pErrorInfo, Assoc_GUID'Unchecked_Access));
         Associated_GUID := Assoc_GUID;

         GNATCOM.Errors.Error_Check
           (pErrorInfo.Vtbl.GetSource
            (pErrorInfo, BSTR_Source'Unchecked_Access));
         Source_PROGID := To_Unbounded_String (To_Ada (BSTR_Source));

         GNATCOM.Errors.Error_Check
           (pErrorInfo.Vtbl.GetDescription
            (pErrorInfo,
             BSTR_Description'Unchecked_Access));
         Description := To_Unbounded_String (To_Ada (BSTR_Description));

         GNATCOM.Errors.Error_Check
           (pErrorInfo.Vtbl.GetHelpFile (pErrorInfo,
                                         BSTR_Path'Unchecked_Access));
         Help_File_Path := To_Unbounded_String (To_Ada (BSTR_Path));

         GNATCOM.Errors.Error_Check
           (pErrorInfo.Vtbl.GetHelpContext (pErrorInfo,
                                            Context'Unchecked_Access));
         Help_Context := GNATCOM.Types.DWORD (Context);
      else
         Source_PROGID := To_Unbounded_String ("");
         Associated_GUID := GNATCOM.Types.GUID_NULL;
         Description := To_Unbounded_String ("");
         Help_File_Path := To_Unbounded_String ("");
         Help_Context := 0;
      end if;
   end Get_IErrorInfo;

   -------------------------
   -- Supports_IErrorInfo --
   -------------------------

   function Supports_IErrorInfo
     (Object : in GNATCOM.Iinterface.Interface_Type'Class)
      return Boolean
   is
      use GNATCOM.Iinterface;

      Support    : GNATCOM.Iinterface.Interface_Type;
      pSupport   : Pointer_To_ISupportErrorInfo;
      Local_GUID : aliased GNATCOM.Types.GUID := IID (Object);
   begin
      Set_IID (Support, IID_ISupportErrorInfo);
      Query (Support, Object);
      pSupport := To_Pointer_To_ISupportErrorInfo (Address (Support));

      return GNATCOM.Errors.Logical_Check
        (pSupport.Vtbl.InterfaceSupportsErrorInfo
         (pSupport,
          Local_GUID'Unchecked_Access));

   exception
      when GNATCOM.Errors.NO_INTERFACE_ERROR =>
         return False;
   end Supports_IErrorInfo;

end GNATCOM.IErrorInfo;
