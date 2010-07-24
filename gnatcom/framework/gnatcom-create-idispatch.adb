------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--            G N A T C O M . C R E A T E . I D I S P A T C H               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
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

with GNATCOM.Errors;

package body GNATCOM.Create.IDispatch is

   function LoadRegTypeLib
     (rguid     : access GNATCOM.Types.GUID;
      wVerMajor : in     Interfaces.C.unsigned_short;
      wVerMinor : in     Interfaces.C.unsigned_short;
      lcid      : in     Interfaces.C.unsigned_long;
      ppTLib    : access GNATCOM.Types.Pointer_To_ITypeLib)
   return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, LoadRegTypeLib, "LoadRegTypeLib");

   function DispGetIDsOfNames
     (pTInfo    : GNATCOM.Types.Pointer_To_ITypeInfo;
      rgszNames : GNATCOM.Types.Pointer_To_Pointer_To_char;
      cNames    : Interfaces.C.unsigned;
      rgDispId  : GNATCOM.Types.Pointer_To_long)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, DispGetIDsOfNames, "DispGetIDsOfNames");

   function DispInvoke
     (uthis        : GNATCOM.Types.Pointer_To_Void;
      pTInfo       : GNATCOM.Types.Pointer_To_ITypeInfo;
      dispIdMember : Interfaces.C.long;
      wFlags       : Interfaces.C.unsigned_short;
      pparams      : GNATCOM.Types.Pointer_To_DISPPARAMS;
      pVarResult   : GNATCOM.Types.Pointer_To_VARIANT;
      pExcepInfo   : GNATCOM.Types.Pointer_To_EXCEPINFO;
      puArgErr     : GNATCOM.Types.Pointer_To_unsigned)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, DispInvoke, "DispInvoke");

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out IDispatch_Type) is
      use type GNATCOM.Types.Pointer_To_ITypeInfo;

      Result : Interfaces.C.unsigned_long;
      pragma Warnings (Off, Result);
   begin
      if This.Type_Information /= null then
         Result := This.Type_Information.Vtbl.AddRef (This.Type_Information);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out IDispatch_Type) is
      use type GNATCOM.Types.Pointer_To_ITypeInfo;

      Result : Interfaces.C.unsigned_long;
      pragma Warnings (Off, Result);
   begin
      if This.Type_Information /= null then
         Result := This.Type_Information.Vtbl.Release (This.Type_Information);
         This.Type_Information := null;
      end if;
   end Finalize;

   -------------------
   -- GetIDsOfNames --
   -------------------

   function GetIDsOfNames
     (Data      : access IDispatch_Type;
      rgszNames : in     GNATCOM.Types.Pointer_To_Pointer_To_char;
      cNames    : in     Interfaces.C.unsigned;
      rgdispid  : in     GNATCOM.Types.Pointer_To_long)
      return GNATCOM.Types.HRESULT
   is
   begin
      return DispGetIDsOfNames (Data.Type_Information,
                                rgszNames,
                                cNames,
                                rgdispid);
   end GetIDsOfNames;

   -----------------
   -- GetTypeInfo --
   -----------------

   function GetTypeInfo
     (Data    : access IDispatch_Type;
      itinfo  : in     Interfaces.C.unsigned;
      pptinfo : in     GNATCOM.Types.Pointer_To_Pointer_To_Void)
      return GNATCOM.Types.HRESULT
   is
      use type Interfaces.C.unsigned_long;
      use type Interfaces.C.unsigned;
      use type GNATCOM.Types.Pointer_To_ITypeInfo;

      Result  : Interfaces.C.unsigned_long;
      pragma Warnings (Off, Result);
   begin
      if itinfo /= 0 then
         return GNATCOM.DISP_E_BADINDEX;
      end if;

      if Data.Type_Information /= null then
         Result := Data.Type_Information.Vtbl.AddRef (Data.Type_Information);
      end if;

      pptinfo.all := Data.Type_Information'Address;

      return GNATCOM.S_OK;
   end GetTypeInfo;

   ----------------------
   -- GetTypeInfoCount --
   ----------------------

   function GetTypeInfoCount (pctinfo : GNATCOM.Types.Pointer_To_unsigned)
     return GNATCOM.Types.HRESULT
   is
   begin
      pctinfo.all := 1;
      return GNATCOM.S_OK;
   end GetTypeInfoCount;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out IDispatch_Type) is
      use type GNATCOM.Types.Pointer_To_ITypeInfo;

      Lib    : aliased GNATCOM.Types.Pointer_To_ITypeLib;
      Result : Interfaces.C.unsigned_long;
      pragma Warnings (Off, Result);
   begin
      if This.Type_Information = null then
         GNATCOM.Errors.Error_Check (LoadRegTypeLib
                                     (This.LIB_IID,
                                      1, 0, 0,
                                      Lib'Access));

         GNATCOM.Errors.Error_Check (Lib.Vtbl.GetTypeInfoOfGuid
                                     (Lib,
                                      This.IID,
                                      This.Type_Information'Unchecked_Access));

         Result := Lib.Vtbl.Release (Lib);
      end if;
   end Initialize;

   ------------
   -- Invoke --
   ------------

   function Invoke
     (This         : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Data         : access IDispatch_Type;
      dispidMember : in     Interfaces.C.long;
      wFlags       : in     Interfaces.C.unsigned_short;
      pdispparams  : in     GNATCOM.Types.Pointer_To_DISPPARAMS;
      pvarResult   : in     GNATCOM.Types.Pointer_To_VARIANT;
      pexcepinfo   : in     GNATCOM.Types.Pointer_To_EXCEPINFO;
      puArgErr     : in     GNATCOM.Types.Pointer_To_unsigned)
      return GNATCOM.Types.HRESULT
   is
   begin
      return DispInvoke (This.all'Address,
                         Data.Type_Information,
                         dispidMember,
                         wFlags,
                         pdispparams,
                         pvarResult,
                         pexcepinfo,
                         puArgErr);
   end Invoke;

end GNATCOM.Create.IDispatch;
