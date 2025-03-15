------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--    G N A T C O M . I C R E A T E T Y P E I N F O  _ I N T E R F A C E    --
--                                                                          --
--                                B o d y                                   --
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

with GNATCOM.Errors;

package body GNATCOM.ICreateTypeInfo_Interface is

   procedure Initialize (This : in out ICreateTypeInfo_Type) is
   begin
      Set_IID (This, GNATCOM.Types.IID_ICreateTypeInfo);
   end Initialize;

   function Pointer (This : ICreateTypeInfo_Type)
     return GNATCOM.Types.Pointer_To_ICreateTypeInfo
   is
   begin
      return To_Pointer_To_ICreateTypeInfo (Address (This));
   end Pointer;

   procedure Attach (This    : in out ICreateTypeInfo_Type;
                     Pointer : in     GNATCOM.Types.Pointer_To_ICreateTypeInfo)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure SetGuid
     (This : ICreateTypeInfo_Type;
      guid : GNATCOM.Types.Pointer_To_GUID)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetGuid
         (Pointer (This),
          guid));

   end SetGuid;

   procedure SetTypeFlags
     (This       : ICreateTypeInfo_Type;
      uTypeFlags : Interfaces.C.unsigned)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetTypeFlags
         (Pointer (This),
          uTypeFlags));

   end SetTypeFlags;

   procedure SetDocString
     (This    : ICreateTypeInfo_Type;
      pStrDoc : GNATCOM.Types.LPWSTR)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetDocString
         (Pointer (This),
          pStrDoc));

   end SetDocString;

   procedure SetHelpContext
     (This          : ICreateTypeInfo_Type;
      dwHelpContext : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetHelpContext
         (Pointer (This),
          dwHelpContext));

   end SetHelpContext;

   procedure SetVersion
     (This         : ICreateTypeInfo_Type;
      wMajorVerNum : Interfaces.C.unsigned_short;
      wMinorVerNum : Interfaces.C.unsigned_short)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetVersion
         (Pointer (This),
          wMajorVerNum,
          wMinorVerNum));

   end SetVersion;

   procedure AddRefTypeInfo
     (This      : ICreateTypeInfo_Type;
      pTInfo    : GNATCOM.Types.Pointer_To_ITypeInfo;
      phRefType : GNATCOM.Types.Pointer_To_unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AddRefTypeInfo
         (Pointer (This),
          pTInfo,
          phRefType));

   end AddRefTypeInfo;

   procedure AddFuncDesc
     (This      : ICreateTypeInfo_Type;
      index     : Interfaces.C.unsigned;
      pFuncDesc : GNATCOM.Types.Pointer_To_FUNCDESC)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AddFuncDesc
         (Pointer (This),
          index,
          pFuncDesc));

   end AddFuncDesc;

   procedure AddImplType
     (This     : ICreateTypeInfo_Type;
      index    : Interfaces.C.unsigned;
      hreftype : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AddImplType
         (Pointer (This),
          index,
          hreftype));

   end AddImplType;

   procedure SetImplTypeFlags
     (This          : ICreateTypeInfo_Type;
      index         : Interfaces.C.unsigned;
      implTypeFlags : Interfaces.C.int)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetImplTypeFlags
         (Pointer (This),
          index,
          implTypeFlags));

   end SetImplTypeFlags;

   procedure SetAlignment
     (This        : ICreateTypeInfo_Type;
      cbAlignment : Interfaces.C.unsigned_short)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetAlignment
         (Pointer (This),
          cbAlignment));

   end SetAlignment;

   procedure SetSchema
     (This       : ICreateTypeInfo_Type;
      pStrSchema : GNATCOM.Types.LPWSTR)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetSchema
         (Pointer (This),
          pStrSchema));

   end SetSchema;

   procedure AddVarDesc
     (This     : ICreateTypeInfo_Type;
      index    : Interfaces.C.unsigned;
      pVarDesc : GNATCOM.Types.Pointer_To_VARDESC)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AddVarDesc
         (Pointer (This),
          index,
          pVarDesc));

   end AddVarDesc;

   procedure SetFuncAndParamNames
     (This      : ICreateTypeInfo_Type;
      index     : Interfaces.C.unsigned;
      rgszNames : GNATCOM.Types.Pointer_To_LPWSTR;
      cNames    : Interfaces.C.unsigned)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetFuncAndParamNames
         (Pointer (This),
          index,
          rgszNames,
          cNames));

   end SetFuncAndParamNames;

   procedure SetVarName
     (This   : ICreateTypeInfo_Type;
      index  : Interfaces.C.unsigned;
      szName : GNATCOM.Types.LPWSTR)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetVarName
         (Pointer (This),
          index,
          szName));

   end SetVarName;

   procedure SetTypeDescAlias
     (This        : ICreateTypeInfo_Type;
      pTDescAlias : GNATCOM.Types.Pointer_To_TYPEDESC)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetTypeDescAlias
         (Pointer (This),
          pTDescAlias));

   end SetTypeDescAlias;

   procedure DefineFuncAsDllEntry
     (This       : ICreateTypeInfo_Type;
      index      : Interfaces.C.unsigned;
      szDllName  : GNATCOM.Types.LPWSTR;
      szProcName : GNATCOM.Types.LPWSTR)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.DefineFuncAsDllEntry
         (Pointer (This),
          index,
          szDllName,
          szProcName));

   end DefineFuncAsDllEntry;

   procedure SetFuncDocString
     (This        : ICreateTypeInfo_Type;
      index       : Interfaces.C.unsigned;
      szDocString : GNATCOM.Types.LPWSTR)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetFuncDocString
         (Pointer (This),
          index,
          szDocString));

   end SetFuncDocString;

   procedure SetVarDocString
     (This        : ICreateTypeInfo_Type;
      index       : Interfaces.C.unsigned;
      szDocString : GNATCOM.Types.LPWSTR)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetVarDocString
         (Pointer (This),
          index,
          szDocString));

   end SetVarDocString;

   procedure SetFuncHelpContext
     (This          : ICreateTypeInfo_Type;
      index         : Interfaces.C.unsigned;
      dwHelpContext : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetFuncHelpContext
         (Pointer (This),
          index,
          dwHelpContext));

   end SetFuncHelpContext;

   procedure SetVarHelpContext
     (This          : ICreateTypeInfo_Type;
      index         : Interfaces.C.unsigned;
      dwHelpContext : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetVarHelpContext
         (Pointer (This),
          index,
          dwHelpContext));

   end SetVarHelpContext;

   procedure SetMops
     (This     : ICreateTypeInfo_Type;
      index    : Interfaces.C.unsigned;
      bstrMops : GNATCOM.Types.BSTR;
      Free     : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetMops
         (Pointer (This),
          index,
          bstrMops));

      if Free then
               GNATCOM.Iinterface.Free (bstrMops);

      end if;

   end SetMops;

   procedure SetTypeIdldesc
     (This     : ICreateTypeInfo_Type;
      pIdlDesc : GNATCOM.Types.Pointer_To_IDLDESC)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetTypeIdldesc
         (Pointer (This),
          pIdlDesc));

   end SetTypeIdldesc;

   procedure LayOut
     (This : ICreateTypeInfo_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.LayOut
         (Pointer (This)));

   end LayOut;

end GNATCOM.ICreateTypeInfo_Interface;
