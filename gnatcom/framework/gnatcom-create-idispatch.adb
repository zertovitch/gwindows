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

with System;
with System.Address_To_Access_Conversions;
with Ada.Containers.Ordered_Maps;
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

   function Less_Than (Left, Right : GNATCOM.Types.Pointer_To_GUID) return Boolean is
      package Conv is new System.Address_To_Access_Conversions (GNATCOM.Types.GUID);
      use System;
   begin
      return Conv.To_Address (Conv.Object_Pointer (Left)) <
        Conv.To_Address (Conv.Object_Pointer (Right));
   end Less_Than;

   use type GNATCOM.Types.Pointer_To_ITypeLib;

   package Typelib_Maps is
     new Ada.Containers.Ordered_Maps (GNATCOM.Types.Pointer_To_GUID,
                                      GNATCOM.Types.Pointer_To_ITypeLib,
                                      Less_Than);

   use type GNATCOM.Types.Pointer_To_ITypeInfo;

   package Typeinfo_Maps is
     new Ada.Containers.Ordered_Maps (GNATCOM.Types.Pointer_To_GUID,
                                      GNATCOM.Types.Pointer_To_ITypeInfo,
                                      Less_Than);

   protected Typelibs is
      procedure Load (Lib_ID    : not null GNATCOM.Types.Pointer_To_GUID;
                      IID       : not null GNATCOM.Types.Pointer_To_GUID;
                      Type_Info : out GNATCOM.Types.Pointer_To_ITypeInfo);
   private
      Libs  : Typelib_Maps.Map;
      Types : Typeinfo_Maps.Map;
   end Typelibs;

   protected body Typelibs is
      procedure Load (Lib_ID    : not null GNATCOM.Types.Pointer_To_GUID;
                      IID       : not null GNATCOM.Types.Pointer_To_GUID;
                      Type_Info : out GNATCOM.Types.Pointer_To_ITypeInfo)
      is
         CI : constant Typeinfo_Maps.Cursor := Types.Find (IID);
         C  : Typelib_Maps.Cursor;
         L  : aliased GNATCOM.Types.Pointer_To_ITypeLib;
         TI : aliased GNATCOM.Types.Pointer_To_ITypeInfo;
         N  : Interfaces.C.unsigned_long;
      begin
         if Typeinfo_Maps.Has_Element (CI) then
            TI := Typeinfo_Maps.Element (CI);
            N := TI.Vtbl.AddRef (TI);
         else
            C := Libs.Find (Lib_ID);
            if Typelib_Maps.Has_Element (C) then
               L := Typelib_Maps.Element (C);
            else
               GNATCOM.Errors.Error_Check
                 (LoadRegTypeLib (Lib_ID, 1, 0, 0, L'Access));
               Libs.Insert (Lib_ID, L);
            end if;

            GNATCOM.Errors.Error_Check
              (L.Vtbl.GetTypeInfoOfGuid (L,
               IID,
               TI'Unchecked_Access));
            Types.Insert (IID, TI);
         end if;

         Type_Info := TI;
      end Load;
   end Typelibs;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out IDispatch_Type) is

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
      use type Interfaces.C.unsigned;

      Result  : Interfaces.C.unsigned_long;
      pragma Warnings (Off, Result);
   begin
      if itinfo /= 0 then
         return GNATCOM.DISP_E_BADINDEX;
      end if;

      if Data.Type_Information /= null then
         Result := Data.Type_Information.Vtbl.AddRef (Data.Type_Information);
         pptinfo.all := Data.Type_Information.all'Address;
      else
         pptinfo.all := System.Null_Address;
      end if;

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
   begin
      if This.Type_Information = null then
         Typelibs.Load (This.LIB_IID, This.IID, This.Type_Information);
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
