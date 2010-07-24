------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--         G N A T C O M . I T Y P E I N F O _ I N T E R F A C E            --
--                                                                          --
--                                B o d y                                   --
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

with GNATCOM.Errors; use GNATCOM.Errors;
with GNATCOM.ITypeLib_Interface;
with GNATCOM.BSTR;

package body GNATCOM.ITypeInfo_Interface is

   -- Attach --

   procedure Attach
     (This    : in out ITypeInfo_Type;
      Pointer :        GNATCOM.Types.Pointer_To_ITypeInfo)
   is
   begin
      Attach (This,
              GNATCOM.Iinterface.To_Pointer_To_IUnknown (Pointer.all'Address));
   end Attach;

   -- GetContainingTypeLib --

   function GetContainingTypeLib
     (This   : ITypeInfo_Type;
      pIndex : GNATCOM.Types.Pointer_To_int)
     return GNATCOM.Types.Pointer_To_ITypeLib
   is
      pTLib  : aliased GNATCOM.Types.Pointer_To_ITypeLib;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetContainingTypeLib (Pointer (This),
                                                   pTLib'Unchecked_Access,
                                                   pIndex));
      return pTLib;
   end GetContainingTypeLib;

   -- GetDllEntry --

   procedure GetDllEntry
     (This         : ITypeInfo_Type;
      memid        : Interfaces.C.long;
      invkind      : GNATCOM.Types.INVOKEKIND;
      pBstrDllName : GNATCOM.Types.Pointer_To_BSTR;
      pBstrName    : GNATCOM.Types.Pointer_To_BSTR;
      pwOrdinal    : GNATCOM.Types.Pointer_To_short)
   is
   begin
      Error_Check
         (Pointer (This).Vtbl.GetDllEntry (Pointer (This),
                                           memid,
                                           invkind,
                                           pBstrDllName,
                                           pBstrName,
                                           pwOrdinal));
   end GetDllEntry;

   -- GetDocumentation --

   procedure GetDocumentation
     (This           : ITypeInfo_Type;
      memid          : Interfaces.C.long;
      pBstrName      : GNATCOM.Types.Pointer_To_BSTR;
      pBstrDocString : GNATCOM.Types.Pointer_To_BSTR;
      pdwHelpContext : GNATCOM.Types.Pointer_To_unsigned_long;
      pBstrHelpFile  : GNATCOM.Types.Pointer_To_BSTR)
   is
   begin
      Error_Check
         (Pointer (This).Vtbl.GetDocumentation (Pointer (This),
                                                memid,
                                                pBstrName,
                                                pBstrDocString,
                                                pdwHelpContext,
                                                pBstrHelpFile));
   end GetDocumentation;

   -- GetFuncDesc --

   function GetFuncDesc
     (This  : ITypeInfo_Type;
      index : Interfaces.C.int)
      return GNATCOM.Types.Pointer_To_FUNCDESC
   is
      pFuncDesc : aliased GNATCOM.Types.Pointer_To_FUNCDESC;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetFuncDesc (Pointer (This),
                                          index,
                                          pFuncDesc'Unchecked_Access));
      return pFuncDesc;
   end GetFuncDesc;

   -- GetImplTypeFlags --

   function GetImplTypeFlags
     (This  : ITypeInfo_Type;
      index : Interfaces.C.int)
      return Interfaces.C.unsigned
   is
      pImplTypeFlags : aliased Interfaces.C.int;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetImplTypeFlags
         (Pointer (This),
          index,
          pImplTypeFlags'Unchecked_Access));

      return Interfaces.C.unsigned (pImplTypeFlags);
   end GetImplTypeFlags;

   -- GetMops --

   function GetMops
     (This  : ITypeInfo_Type;
      memid : Interfaces.C.long)
      return GNATCOM.Types.BSTR
   is
      BstrMops : aliased GNATCOM.Types.BSTR;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetMops (Pointer (This),
                                      memid,
                                      BstrMops'Unchecked_Access));

         return BstrMops;
   end GetMops;

   -- GetNames --

   procedure GetNames
     (This        : ITypeInfo_Type;
      memid       : Interfaces.C.long;
      rgBstrNames : GNATCOM.Types.Pointer_To_BSTR_PARAM_ARRAY;
      cMaxNames   : Interfaces.C.int;
      pcNames     : GNATCOM.Types.Pointer_To_int)
   is
   begin
      Error_Check
        (Pointer (This).Vtbl.GetNames (Pointer (This),
                                       memid,
                                       rgBstrNames,
                                       cMaxNames,
                                       pcNames));
   end GetNames;

   -- GetRefTypeInfo --

   function GetRefTypeInfo
     (This     : ITypeInfo_Type;
      hreftype : Interfaces.C.unsigned_long)
      return GNATCOM.Types.Pointer_To_ITypeInfo
   is
      pTInfo : aliased GNATCOM.Types.Pointer_To_ITypeInfo;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetRefTypeInfo (Pointer (This),
                                             hreftype,
                                             pTInfo'Unchecked_Access));

         return pTInfo;
   end GetRefTypeInfo;

   -- GetRefTypeOfImplType --

   function GetRefTypeOfImplType
     (This  : ITypeInfo_Type;
      index : Interfaces.C.int)
      return Interfaces.C.unsigned_long
   is
      RefType : aliased Interfaces.C.unsigned_long;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetRefTypeOfImplType (Pointer (This),
                                                   index,
                                                   RefType'Unchecked_Access));

      return RefType;
   end GetRefTypeOfImplType;

   -- GetTypeAttr --

   function GetTypeAttr
     (This : ITypeInfo_Type)
      return GNATCOM.Types.Pointer_To_TYPEATTR
   is
      pTypeAttr : aliased GNATCOM.Types.Pointer_To_TYPEATTR;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetTypeAttr (Pointer (This),
                                         pTypeAttr'Unchecked_Access));

      return pTypeAttr;
   end GetTypeAttr;

   -- GetTypeComp --

   function GetTypeComp
     (This : ITypeInfo_Type)
      return GNATCOM.Types.Pointer_To_ITypeComp
   is
      pTComp : aliased GNATCOM.Types.Pointer_To_ITypeComp;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetTypeComp (Pointer (This),
                                          pTComp'Unchecked_Access));

      return pTComp;
   end GetTypeComp;

   -- GetVarDesc --

   function GetVarDesc
     (This  : ITypeInfo_Type;
      index : Interfaces.C.int)
      return GNATCOM.Types.Pointer_To_VARDESC
   is
      pVarDesc : aliased GNATCOM.Types.Pointer_To_VARDESC;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetVarDesc (Pointer (This),
                                         index,
                                         pVarDesc'Unchecked_Access));

      return pVarDesc;
   end GetVarDesc;

   -- Initialize --

   procedure Initialize (This : in out ITypeInfo_Type) is
   begin
      Set_IID (This, GNATCOM.Types.IID_ITypeInfo);
   end Initialize;

   -- Pointer --

   function Pointer
     (This : ITypeInfo_Type)
      return GNATCOM.Types.Pointer_To_ITypeInfo
   is
   begin
      return To_Pointer_To_ITypeInfo (Address (This));
   end Pointer;

   -- ReleaseFuncDesc --

   procedure ReleaseFuncDesc
     (This      : ITypeInfo_Type;
      pFuncDesc : GNATCOM.Types.Pointer_To_FUNCDESC)
   is
   begin
      Error_Check
        (Pointer (This).Vtbl.ReleaseFuncDesc (Pointer (This),
                                              pFuncDesc));

   end ReleaseFuncDesc;

   -- ReleaseTypeAttr --

   procedure ReleaseTypeAttr
     (This      : ITypeInfo_Type;
      pTypeAttr : GNATCOM.Types.Pointer_To_TYPEATTR)
   is
   begin
      Error_Check
        (Pointer (This).Vtbl.ReleaseTypeAttr (Pointer (This),
                                              pTypeAttr));
   end ReleaseTypeAttr;

   -- ReleaseVarDesc --

   procedure ReleaseVarDesc
     (This     : ITypeInfo_Type;
      pVarDesc : GNATCOM.Types.Pointer_To_VARDESC)
   is
   begin
      Error_Check
        (Pointer (This).Vtbl.ReleaseVarDesc (Pointer (This),
                                             pVarDesc));
   end ReleaseVarDesc;

   -- GetName --

   function GetName
     (This : ITypeInfo_Type)
     return String
   is
      use GNATCOM.ITypeLib_Interface;

      Ref_Lib    : ITypeLib_Type;
      Ref_Index  : aliased Interfaces.C.int;
      Name       : aliased GNATCOM.Types.BSTR;
   begin
      Attach (Ref_Lib, GetContainingTypeLib
              (This,
               Ref_Index'Unchecked_Access));

      GetDocumentation (Ref_Lib,
                        Ref_Index,
                        Name'Unchecked_Access,
                        null,
                        null,
                        null);

      return GNATCOM.BSTR.To_Ada (Name);
   end GetName;

   -- GetDocumentation --

   function GetDocumentation
     (This : ITypeInfo_Type)
     return String
   is
      use GNATCOM.ITypeLib_Interface;

      Ref_Lib    : ITypeLib_Type;
      Ref_Index  : aliased Interfaces.C.int;
      Doc        : aliased GNATCOM.Types.BSTR;
   begin
      Attach (Ref_Lib, GetContainingTypeLib
              (This,
               Ref_Index'Unchecked_Access));

      GetDocumentation (Ref_Lib,
                        Ref_Index,
                        null,
                        Doc'Unchecked_Access,
                        null,
                        null);

      return GNATCOM.BSTR.To_Ada (Doc);
   end GetDocumentation;

   -- GetTypeKind --

   function GetTypeKind
     (This : ITypeInfo_Type)
     return GNATCOM.Types.TYPEKIND
   is
      use GNATCOM.ITypeLib_Interface;

      Ref_Lib    : ITypeLib_Type;
      Ref_Index  : aliased Interfaces.C.int;
   begin
      Attach (Ref_Lib, GetContainingTypeLib
              (This,
               Ref_Index'Unchecked_Access));

      return GetTypeInfoType (Ref_Lib, Ref_Index);
   end GetTypeKind;

   -- GetFunctionName --

   function GetFunctionName
     (This  : ITypeInfo_Type;
      Desc  : GNATCOM.Types.Pointer_To_FUNCDESC)
     return String
   is
      use GNATCOM.Types;

      Name : aliased GNATCOM.Types.BSTR;
   begin
      GetDocumentation (This,
                        Desc.memid,
                        Name'Unchecked_Access,
                        null,
                        null,
                        null);

      case Desc.invkind is
         when INVOKE_PROPERTYGET =>
            return "Get_" & BSTR.To_Ada (Name);
         when INVOKE_PROPERTYPUT =>
            return "Put_" & BSTR.To_Ada (Name);
         when INVOKE_PROPERTYPUTREF =>
            return "PutRef_" & BSTR.To_Ada (Name);
         when others =>
            return BSTR.To_Ada (Name);
      end case;
   end GetFunctionName;

end GNATCOM.ITypeInfo_Interface;
