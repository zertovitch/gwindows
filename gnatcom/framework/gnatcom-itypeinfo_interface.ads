------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--         G N A T C O M . I T Y P E I N F O _ I N T E R F A C E            --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2006 David Botton                   --
--                                                                          --
-- MIT License                                                              --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining    --
-- a copy of this software and associated documentation files (the          --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          --
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
--                                                                          --
-- More information about GNATCOM and the most current version can          --
-- be located on the web at http://www.gnavi.org/gnatcom                    --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with System;
with Interfaces.C;

with GNATCOM.Iinterface;
with GNATCOM.Types;

package GNATCOM.ITypeInfo_Interface is

   type ITypeInfo_Type is new GNATCOM.Iinterface.Interface_Type
     with null record;

   procedure Initialize (This : in out ITypeInfo_Type);

   function Pointer (This : ITypeInfo_Type)
     return GNATCOM.Types.Pointer_To_ITypeInfo;

   function To_Pointer_To_ITypeInfo is new Ada.Unchecked_Conversion
     (System.Address, GNATCOM.Types.Pointer_To_ITypeInfo);

   procedure Attach
     (This    : in out ITypeInfo_Type;
      Pointer :        GNATCOM.Types.Pointer_To_ITypeInfo);

   function GetContainingTypeLib
     (This   : ITypeInfo_Type;
      pIndex : GNATCOM.Types.Pointer_To_int)
     return GNATCOM.Types.Pointer_To_ITypeLib;

   procedure GetDllEntry
     (This         : ITypeInfo_Type;
      memid        : Interfaces.C.long;
      invkind      : GNATCOM.Types.INVOKEKIND;
      pBstrDllName : GNATCOM.Types.Pointer_To_BSTR;
      pBstrName    : GNATCOM.Types.Pointer_To_BSTR;
      pwOrdinal    : GNATCOM.Types.Pointer_To_short);

   procedure GetDocumentation
     (This           : ITypeInfo_Type;
      memid          : Interfaces.C.long;
      pBstrName      : GNATCOM.Types.Pointer_To_BSTR;
      pBstrDocString : GNATCOM.Types.Pointer_To_BSTR;
      pdwHelpContext : GNATCOM.Types.Pointer_To_unsigned_long;
      pBstrHelpFile  : GNATCOM.Types.Pointer_To_BSTR);

   function GetFuncDesc
     (This  : ITypeInfo_Type;
      index : Interfaces.C.int)
     return GNATCOM.Types.Pointer_To_FUNCDESC;

   function GetImplTypeFlags
     (This  : ITypeInfo_Type;
      index : Interfaces.C.int)
     return Interfaces.C.unsigned;

   function GetMops
     (This  : ITypeInfo_Type;
      memid : Interfaces.C.long)
     return GNATCOM.Types.BSTR;

   procedure GetNames
     (This        : ITypeInfo_Type;
      memid       : Interfaces.C.long;
      rgBstrNames : GNATCOM.Types.Pointer_To_BSTR_PARAM_ARRAY;
      cMaxNames   : Interfaces.C.int;
      pcNames     : GNATCOM.Types.Pointer_To_unsigned);

   function GetRefTypeInfo
     (This     : ITypeInfo_Type;
      hreftype : Interfaces.C.unsigned_long)
     return GNATCOM.Types.Pointer_To_ITypeInfo;

   function GetRefTypeOfImplType
     (This  : ITypeInfo_Type;
      index : Interfaces.C.int)
      return Interfaces.C.unsigned_long;

   function GetTypeAttr
     (This : ITypeInfo_Type)
     return GNATCOM.Types.Pointer_To_TYPEATTR;

   function GetTypeComp
     (This : ITypeInfo_Type)
     return GNATCOM.Types.Pointer_To_ITypeComp;

   function GetVarDesc
     (This  : ITypeInfo_Type;
      index : Interfaces.C.int)
     return GNATCOM.Types.Pointer_To_VARDESC;

   procedure ReleaseTypeAttr
     (This      : ITypeInfo_Type;
      pTypeAttr : GNATCOM.Types.Pointer_To_TYPEATTR);

   procedure ReleaseFuncDesc
     (This      : ITypeInfo_Type;
      pFuncDesc : GNATCOM.Types.Pointer_To_FUNCDESC);

   procedure ReleaseVarDesc
     (This     : ITypeInfo_Type;
      pVarDesc : GNATCOM.Types.Pointer_To_VARDESC);

   function GetName
     (This : ITypeInfo_Type)
     return String;
   --  Simplifies retrieving the type name of an ITypeInfo

   function GetDocumentation
     (This : ITypeInfo_Type)
     return String;
   --  Simplifies retrieving the type Documentation of an ITypeInfo

   function GetTypeKind
     (This : ITypeInfo_Type)
     return GNATCOM.Types.TYPEKIND;
   --  Simplifies retrieving the type kind for the ITypeInfo

   function GetFunctionName
     (This : ITypeInfo_Type;
      Desc : GNATCOM.Types.Pointer_To_FUNCDESC)
     return String;
   --  Returns the function name based on its invocation type
   --  INVOKE_FUNC           => Name
   --  INVOKE_PROPERTYGET    => Get_Name
   --  INVOKE_PROPERTYPUT    => Put_Name
   --  INVOKE_PROPERTYPUTREF => PutRef_Name

end GNATCOM.ITypeInfo_Interface;
