------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--     G N A T C O M . I C R E A T E T Y P E L I B _ I N T E R F A C E      --
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
with GNATCOM.BSTR;

package body GNATCOM.ICreateTypeLib_Interface is

   function CreateTypeLib
     (SK : in GNATCOM.Types.SYSKIND;
      FN : in GNATCOM.Types.BSTR;
      CT : access GNATCOM.Types.Pointer_To_ICreateTypeLib)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, CreateTypeLib, "CreateTypeLib");

   procedure Create_Type_Library
     (This      : in out ICreateTypeLib_Type;
      File_Name : in     String)
   is
      Name  : constant GNATCOM.Types.BSTR := GNATCOM.BSTR.To_BSTR (File_Name);
      pTLib : aliased GNATCOM.Types.Pointer_To_ICreateTypeLib;
   begin
      GNATCOM.Errors.Error_Check
        (CreateTypeLib (GNATCOM.Types.SYS_WIN32,
                        Name,
                        pTLib'Access));

      GNATCOM.BSTR.Free (Name);

      Attach (This, pTLib);
   end Create_Type_Library;

   procedure Initialize (This : in out ICreateTypeLib_Type) is
   begin
      Set_IID (This, GNATCOM.Types.IID_ICreateTypeLib);
   end Initialize;

   function Pointer (This : ICreateTypeLib_Type)
     return GNATCOM.Types.Pointer_To_ICreateTypeLib
   is
   begin
      return To_Pointer_To_ICreateTypeLib (Address (This));
   end Pointer;

   procedure Attach (This    : in out ICreateTypeLib_Type;
                     Pointer : in     GNATCOM.Types.Pointer_To_ICreateTypeLib)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure CreateTypeInfo
     (This     : ICreateTypeLib_Type;
      szName   : GNATCOM.Types.LPWSTR;
      tkind    : GNATCOM.Types.TYPEKIND;
      ppCTInfo : GNATCOM.Types.Pointer_To_Pointer_To_ICreateTypeInfo)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.CreateTypeInfo
         (Pointer (This),
          szName,
          tkind,
          ppCTInfo));

   end CreateTypeInfo;

   procedure SetName
     (This   : ICreateTypeLib_Type;
      szName : GNATCOM.Types.LPWSTR)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetName
         (Pointer (This),
          szName));

   end SetName;

   procedure SetVersion
     (This         : ICreateTypeLib_Type;
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

   procedure SetGuid
     (This : ICreateTypeLib_Type;
      guid : GNATCOM.Types.Pointer_To_GUID)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetGuid
         (Pointer (This),
          guid));

   end SetGuid;

   procedure SetDocString
     (This  : ICreateTypeLib_Type;
      szDoc : GNATCOM.Types.LPWSTR)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetDocString
         (Pointer (This),
          szDoc));

   end SetDocString;

   procedure SetHelpFileName
     (This           : ICreateTypeLib_Type;
      szHelpFileName : GNATCOM.Types.LPWSTR)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetHelpFileName
         (Pointer (This),
          szHelpFileName));

   end SetHelpFileName;

   procedure SetHelpContext
     (This          : ICreateTypeLib_Type;
      dwHelpContext : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetHelpContext
         (Pointer (This),
          dwHelpContext));

   end SetHelpContext;

   procedure SetLcid
     (This : ICreateTypeLib_Type;
      lcid : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetLcid
         (Pointer (This),
          lcid));

   end SetLcid;

   procedure SetLibFlags
     (This      : ICreateTypeLib_Type;
      uLibFlags : Interfaces.C.unsigned)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetLibFlags
         (Pointer (This),
          uLibFlags));

   end SetLibFlags;

   procedure SaveAllChanges
     (This : ICreateTypeLib_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SaveAllChanges
         (Pointer (This)));

   end SaveAllChanges;

end GNATCOM.ICreateTypeLib_Interface;
