------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--    G N A T C O M . I C R E A T E T Y P E I N F O  _ I N T E R F A C E    --
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

package GNATCOM.ICreateTypeInfo_Interface is

   type ICreateTypeInfo_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out ICreateTypeInfo_Type);

   function Pointer (This : ICreateTypeInfo_Type)
     return GNATCOM.Types.Pointer_To_ICreateTypeInfo;

   function To_Pointer_To_ICreateTypeInfo is new Ada.Unchecked_Conversion
     (System.Address, GNATCOM.Types.Pointer_To_ICreateTypeInfo);

   procedure Attach
     (This    : in out ICreateTypeInfo_Type;
      Pointer : in     GNATCOM.Types.Pointer_To_ICreateTypeInfo);

   procedure SetGuid
     (This : ICreateTypeInfo_Type;
      guid : GNATCOM.Types.Pointer_To_GUID);

   procedure SetTypeFlags
     (This       : ICreateTypeInfo_Type;
      uTypeFlags : Interfaces.C.unsigned);

   procedure SetDocString
     (This    : ICreateTypeInfo_Type;
      pStrDoc : GNATCOM.Types.LPWSTR);

   procedure SetHelpContext
     (This          : ICreateTypeInfo_Type;
      dwHelpContext : Interfaces.C.unsigned_long);

   procedure SetVersion
     (This         : ICreateTypeInfo_Type;
      wMajorVerNum : Interfaces.C.unsigned_short;
      wMinorVerNum : Interfaces.C.unsigned_short);

   procedure AddRefTypeInfo
     (This      : ICreateTypeInfo_Type;
      pTInfo    : GNATCOM.Types.Pointer_To_ITypeInfo;
      phRefType : GNATCOM.Types.Pointer_To_unsigned_long);

   procedure AddFuncDesc
     (This      : ICreateTypeInfo_Type;
      index     : Interfaces.C.unsigned;
      pFuncDesc : GNATCOM.Types.Pointer_To_FUNCDESC);

   procedure AddImplType
     (This     : ICreateTypeInfo_Type;
      index    : Interfaces.C.unsigned;
      hreftype : Interfaces.C.unsigned_long);

   procedure SetImplTypeFlags
     (This          : ICreateTypeInfo_Type;
      index         : Interfaces.C.unsigned;
      implTypeFlags : Interfaces.C.int);

   procedure SetAlignment
     (This        : ICreateTypeInfo_Type;
      cbAlignment : Interfaces.C.unsigned_short);

   procedure SetSchema
     (This       : ICreateTypeInfo_Type;
      pStrSchema : GNATCOM.Types.LPWSTR);

   procedure AddVarDesc
     (This     : ICreateTypeInfo_Type;
      index    : Interfaces.C.unsigned;
      pVarDesc : GNATCOM.Types.Pointer_To_VARDESC);

   procedure SetFuncAndParamNames
     (This      : ICreateTypeInfo_Type;
      index     : Interfaces.C.unsigned;
      rgszNames : GNATCOM.Types.Pointer_To_LPWSTR;
      cNames    : Interfaces.C.unsigned);

   procedure SetVarName
     (This   : ICreateTypeInfo_Type;
      index  : Interfaces.C.unsigned;
      szName : GNATCOM.Types.LPWSTR);

   procedure SetTypeDescAlias
     (This        : ICreateTypeInfo_Type;
      pTDescAlias : GNATCOM.Types.Pointer_To_TYPEDESC);

   procedure DefineFuncAsDllEntry
     (This       : ICreateTypeInfo_Type;
      index      : Interfaces.C.unsigned;
      szDllName  : GNATCOM.Types.LPWSTR;
      szProcName : GNATCOM.Types.LPWSTR);

   procedure SetFuncDocString
     (This        : ICreateTypeInfo_Type;
      index       : Interfaces.C.unsigned;
      szDocString : GNATCOM.Types.LPWSTR);

   procedure SetVarDocString
     (This        : ICreateTypeInfo_Type;
      index       : Interfaces.C.unsigned;
      szDocString : GNATCOM.Types.LPWSTR);

   procedure SetFuncHelpContext
     (This          : ICreateTypeInfo_Type;
      index         : Interfaces.C.unsigned;
      dwHelpContext : Interfaces.C.unsigned_long);

   procedure SetVarHelpContext
     (This          : ICreateTypeInfo_Type;
      index         : Interfaces.C.unsigned;
      dwHelpContext : Interfaces.C.unsigned_long);

   procedure SetMops
     (This     : ICreateTypeInfo_Type;
      index    : Interfaces.C.unsigned;
      bstrMops : GNATCOM.Types.BSTR;
      Free     : Boolean := True);

   procedure SetTypeIdldesc
     (This     : ICreateTypeInfo_Type;
      pIdlDesc : GNATCOM.Types.Pointer_To_IDLDESC);

   procedure LayOut
     (This : ICreateTypeInfo_Type);

end GNATCOM.ICreateTypeInfo_Interface;
