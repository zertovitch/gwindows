------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--           G N A T C O M . I T Y P E L I B _ I N T E R F A C E            --
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
with GNATCOM.BSTR;

package body GNATCOM.ITypeLib_Interface is

   function LoadTypeLib
     (wszFile :        GNATCOM.Types.BSTR;
      ppTLib  : access GNATCOM.Types.Pointer_To_ITypeLib)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, LoadTypeLib, "LoadTypeLib");

   ----------
   -- Open --
   ----------

   procedure Open (This        : in out ITypeLib_Type;
                   Source_Name : in     String)
   is
      Name  : constant GNATCOM.Types.BSTR :=
        GNATCOM.BSTR.To_BSTR (Source_Name);
      pTLib : aliased GNATCOM.Types.Pointer_To_ITypeLib;
   begin
      Error_Check
        (LoadTypeLib (Name,
                      pTLib'Access));

      GNATCOM.BSTR.Free (Name);

      Attach (This, pTLib);
   end Open;

   ------------
   -- Attach --
   ------------

   procedure Attach
     (This    : in out ITypeLib_Type;
      Pointer :        GNATCOM.Types.Pointer_To_ITypeLib)
   is
   begin
      Attach (This,
              GNATCOM.Iinterface.To_Pointer_To_IUnknown (Pointer.all'Address));
   end Attach;

   --------------
   -- FindName --
   --------------

   procedure FindName
     (This      : ITypeLib_Type;
      szNameBuf : GNATCOM.Types.BSTR;
      lHashVal  : Interfaces.C.unsigned_long;
      ppTInfo   : GNATCOM.Types.Pointer_To_Pointer_To_ITypeInfo;
      rgMemId   : GNATCOM.Types.Pointer_To_long;
      pcFound   : GNATCOM.Types.Pointer_To_int;
      Clear     : Boolean                                       := True)
   is
   begin
      Error_Check
        (Pointer (This).Vtbl.FindName (Pointer (This),
                                       szNameBuf,
                                       lHashVal,
                                       ppTInfo,
                                       rgMemId,
                                       pcFound));

      if Clear then
         GNATCOM.BSTR.Free (szNameBuf);
      end if;
   end FindName;

   ----------------------
   -- GetDocumentation --
   ----------------------

   procedure GetDocumentation
     (This           : ITypeLib_Type;
      index          : Interfaces.C.int;
      pBstrName      : GNATCOM.Types.Pointer_To_BSTR;
      pBstrDocString : GNATCOM.Types.Pointer_To_BSTR;
      pdwHelpContext : GNATCOM.Types.Pointer_To_unsigned_long;
      pBstrHelpFile  : GNATCOM.Types.Pointer_To_BSTR)
   is
   begin
      Error_Check
        (Pointer (This).Vtbl.GetDocumentation (Pointer (This),
                                               index,
                                               pBstrName,
                                               pBstrDocString,
                                               pdwHelpContext,
                                               pBstrHelpFile));
   end GetDocumentation;

   ----------------
   -- GetLibAttr --
   ----------------

   function GetLibAttr
     (This : ITypeLib_Type)
     return GNATCOM.Types.Pointer_To_TLIBATTR
   is
      pTLibAttr : aliased GNATCOM.Types.Pointer_To_TLIBATTR;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetLibAttr (Pointer (This),
                                         pTLibAttr'Unchecked_Access));
      return pTLibAttr;
   end GetLibAttr;

   -----------------
   -- GetTypeComp --
   -----------------

   function GetTypeComp
     (This : ITypeLib_Type)
     return GNATCOM.Types.Pointer_To_ITypeComp
   is
      pTComp : aliased GNATCOM.Types.Pointer_To_ITypeComp;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetTypeComp (Pointer (This),
                                          pTComp'Unchecked_Access));
      return pTComp;
   end GetTypeComp;

   -----------------
   -- GetTypeInfo --
   -----------------

   function GetTypeInfo
     (This  : ITypeLib_Type;
      index : Interfaces.C.int)
     return GNATCOM.Types.Pointer_To_ITypeInfo
   is
      pTInfo : aliased GNATCOM.Types.Pointer_To_ITypeInfo;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetTypeInfo (Pointer (This),
                                          index,
                                          pTInfo'Unchecked_Access));
      return pTInfo;
   end GetTypeInfo;

   ----------------------
   -- GetTypeInfoCount --
   ----------------------

   function GetTypeInfoCount
     (This : ITypeLib_Type)
     return Interfaces.C.unsigned
   is
   begin
      return Interfaces.C.unsigned
        (Pointer (This).Vtbl.GetTypeInfoCount (Pointer (This)));
   end GetTypeInfoCount;

   -----------------------
   -- GetTypeInfoOfGuid --
   -----------------------

   function GetTypeInfoOfGuid
     (This : ITypeLib_Type;
      guid : GNATCOM.Types.Pointer_To_GUID)
     return GNATCOM.Types.Pointer_To_ITypeInfo
   is
      pTInfo : aliased GNATCOM.Types.Pointer_To_ITypeInfo;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetTypeInfoOfGuid (Pointer (This),
                                                guid,
                                                pTInfo'Unchecked_Access));
      return pTInfo;
   end GetTypeInfoOfGuid;

   ---------------------
   -- GetTypeInfoType --
   ---------------------

   function GetTypeInfoType
     (This  : ITypeLib_Type;
      index : Interfaces.C.int)
     return GNATCOM.Types.TYPEKIND
   is
      TKind : aliased GNATCOM.Types.TYPEKIND;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetTypeInfoType (Pointer (This),
                                              index,
                                              TKind'Unchecked_Access));
      return TKind;
   end GetTypeInfoType;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out ITypeLib_Type) is
   begin
      Set_IID (This, GNATCOM.Types.IID_ITypeLib);
   end Initialize;

   ------------
   -- IsName --
   ------------

   function IsName
     (This      : ITypeLib_Type;
      szNameBuf : GNATCOM.Types.BSTR;
      lHashVal  : Interfaces.C.unsigned_long;
      Clear     : Boolean                       := True)
     return GNATCOM.Types.bool
   is
      fName    : aliased GNATCOM.Types.bool;
   begin
      Error_Check
        (Pointer (This).Vtbl.IsName (Pointer (This),
                                     szNameBuf,
                                     lHashVal,
                                     fName'Unchecked_Access));

      if Clear then
         GNATCOM.BSTR.Free (szNameBuf);
      end if;

      return fName;
   end IsName;

   -------------
   -- Pointer --
   -------------

   function Pointer
     (This : ITypeLib_Type)
      return GNATCOM.Types.Pointer_To_ITypeLib
   is
   begin
      return To_Pointer_To_ITypeLib (Address (This));
   end Pointer;

   ---------------------
   -- ReleaseTLibAttr --
   ---------------------

   procedure ReleaseTLibAttr
     (This      : ITypeLib_Type;
      pTLibAttr : GNATCOM.Types.Pointer_To_TLIBATTR)
   is
   begin
      Error_Check
        (Pointer (This).Vtbl.ReleaseTLibAttr (Pointer (This),
                                              pTLibAttr));
   end ReleaseTLibAttr;

end GNATCOM.ITypeLib_Interface;
