------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                     G N A T C O M . V A R I A N T                        --
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

with Ada.Unchecked_Conversion;
with GNATCOM.Errors;
with GNATCOM.BSTR;

with GNATCOM.ITypeLib_Interface;
with GNATCOM.ITypeInfo_Interface;

package body GNATCOM.VARIANT is

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT);
   --  Check for VARIANT specific errors

   procedure VariantInit (pvarg : in System.Address);
   pragma Import (StdCall, VariantInit, "VariantInit");

   function VariantClear (pvarg : in System.Address)
                         return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, VariantClear, "VariantClear");

   function VariantCopy (pvargDest : System.Address;
                         pvargSrc  : System.Address)
                        return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, VariantCopy, "VariantCopy");

   function VariantChangeType (pvargDest : System.Address;
                               pvarSrc   : System.Address;
                               wFlags    : Interfaces.C.unsigned_short;
                               vt        : GNATCOM.Types.VARTYPE)
                              return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, VariantChangeType, "VariantChangeType");

   function SysAllocString (C_String : GNATCOM.Types.BSTR)
                           return GNATCOM.Types.BSTR;
   pragma Import (StdCall, SysAllocString, "SysAllocString");

   type SYSTEMTIME is
      record
         wYear         : Interfaces.C.short;
         wMonth        : Interfaces.C.short;
         wDayOfWeek    : Interfaces.C.short := 0;
         wDay          : Interfaces.C.short;
         wHour         : Interfaces.C.short;
         wMinute       : Interfaces.C.short;
         wSecond       : Interfaces.C.short;
         wMilliseconds : Interfaces.C.short := 0;
      end record;

   function VariantTimeToSystemTime
     (vtime        : in     GNATCOM.Types.DATE;
      lpSystemTime : access SYSTEMTIME)
     return Interfaces.C.int;
   pragma Import (StdCall, VariantTimeToSystemTime, "VariantTimeToSystemTime");

   function SystemTimeToVariantTime
     (systemtime : in     GNATCOM.VARIANT.SYSTEMTIME;
      pvtime     : access GNATCOM.Types.DATE)
     return Interfaces.C.int;
   pragma Import (StdCall, SystemTimeToVariantTime, "SystemTimeToVariantTime");

   -----------------
   -- Change_Type --
   -----------------

   procedure Change_Type (This : in out GNATCOM.Types.VARIANT;
                          VT   : in     GNATCOM.Types.VARTYPE)
   is
      use type Interfaces.C.unsigned_short;
   begin
      if This.vt /= VT then
         Error_Check (VariantChangeType (pvargDest => This'Address,
                                         pvarSrc  => This'Address,
                                         wFlags    => 0,
                                         vt        => VT));
      end if;
   end Change_Type;

   -------------
   -- Is_NULL --
   -------------

   function Is_NULL (This : GNATCOM.Types.VARIANT) return Boolean
   is
      use type Interfaces.C.unsigned_short;
   begin
      if This.vt = GNATCOM.Types.VT_NULL then
         return True;
      end if;

      return False;
   end Is_NULL;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out GNATCOM.Types.VARIANT) is
   begin
      Error_Check (VariantClear (This'Address));
   end Clear;

   ----------
   -- Free --
   ----------

   procedure Free (This : in GNATCOM.Types.VARIANT) is
      Temp : GNATCOM.Types.VARIANT := This;
   begin
      Error_Check (VariantClear (Temp'Address));
   end Free;

   ----------
   -- Copy --
   ----------

   function Copy (From : GNATCOM.Types.VARIANT)
     return GNATCOM.Types.VARIANT
   is
      Temp_Var    : GNATCOM.Types.VARIANT := From;
      New_Variant : GNATCOM.Types.VARIANT;
   begin
      Initialize (New_Variant);
      Error_Check (VariantCopy (pvargDest => New_Variant'Address,
                                pvargSrc  => Temp_Var'Address));
      return New_Variant;
   end Copy;

   -----------------
   -- Error_Check --
   -----------------

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT) is
   begin
      GNATCOM.Errors.Set_Last_HRESULT (Result);

      if GNATCOM.Errors.FAILED (Result) then
         case Result is
            when DISP_E_ARRAYISLOCKED =>
               raise ARRAY_LOCKED_ERROR;
            when DISP_E_BADVARTYPE =>
               raise INVALID_TYPE_ERROR;
            when DISP_E_OVERFLOW =>
               raise OVERFLOW_ERROR;
            when DISP_E_TYPEMISMATCH =>
               raise TYPE_MISMATCH_ERROR;
            when others =>
               GNATCOM.Errors.Error_Check (Result);
         end case;
      end if;
   end Error_Check;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out GNATCOM.Types.VARIANT) is
   begin
      VariantInit (This'Address);
   end Initialize;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (From  : GNATCOM.Types.VARIANT;
      Clear : Boolean := True)
      return String
   is
      use type GNATCOM.Types.VARTYPE;

      Temp_Var : GNATCOM.Types.VARIANT := From;
      Do_Clear : Boolean := Clear;
   begin
      if Temp_Var.vt = GNATCOM.Types.VT_NULL then
         return "";
      end if;

      if Temp_Var.vt /= GNATCOM.Types.VT_BSTR then
         Do_Clear := True;
      end if;

      Change_Type (Temp_Var, GNATCOM.Types.VT_BSTR);

      return GNATCOM.BSTR.To_Ada (Temp_Var.u.bstrVal, Do_Clear);
   end To_Ada;

   -----------------
   -- To_Ada_Wide --
   -----------------

   function To_Ada_Wide
     (From  : GNATCOM.Types.VARIANT;
      Clear : Boolean := True)
      return Wide_String
   is
      use type GNATCOM.Types.VARTYPE;

      Temp_Var : GNATCOM.Types.VARIANT := From;
      Do_Clear : Boolean := Clear;
   begin
      if Temp_Var.vt = GNATCOM.Types.VT_NULL then
         return "";
      end if;

      if Temp_Var.vt /= GNATCOM.Types.VT_BSTR then
         Do_Clear := True;
      end if;

      Change_Type (Temp_Var, GNATCOM.Types.VT_BSTR);

      return GNATCOM.BSTR.To_Ada_Wide (Temp_Var.u.bstrVal, Do_Clear);
   end To_Ada_Wide;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (From  : GNATCOM.Types.VARIANT)
      return Integer
   is
      Temp_Var : GNATCOM.Types.VARIANT := Copy (From);
      Result   : Integer;
   begin
      Change_Type (Temp_Var, GNATCOM.Types.VT_I4);
      Result := Integer (Temp_Var.u.lVal);
      VARIANT.Clear (Temp_Var);
      return Result;
   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (From  : GNATCOM.Types.VARIANT)
      return Float
   is
      Temp_Var : GNATCOM.Types.VARIANT := Copy (From);
      Result   : Float;
   begin
      Change_Type (Temp_Var, GNATCOM.Types.VT_R4);
      Result := Float (Temp_Var.u.fltVal);
      VARIANT.Clear (Temp_Var);
      return Result;
   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (From  : GNATCOM.Types.VARIANT)
     return Ada.Calendar.Time
   is
      use Ada.Calendar;
      use type Interfaces.C.int;
      use type Interfaces.C.short;

      Temp_Var : GNATCOM.Types.VARIANT := Copy (From);
      C_Time   : aliased SYSTEMTIME;
      Ada_Time : Ada.Calendar.Time;
   begin
      Change_Type (Temp_Var, GNATCOM.Types.VT_DATE);

      if VariantTimeToSystemTime (Temp_Var.u.date, C_Time'Access) /= 0 then
         Ada_Time := Time_Of (Year_Number (C_Time.wYear),
                              Month_Number (C_Time.wMonth),
                              Day_Number (C_Time.wDay),
                              Day_Duration
                              ((Day_Duration (C_Time.wHour) * 60 * 60) +
                               (Day_Duration (C_Time.wMinute) * 60) +
                               (Day_Duration (C_Time.wSecond))));

         VARIANT.Clear (Temp_Var);
         return Ada_Time;
      else
         raise INVALID_TYPE_ERROR;
      end if;

   end To_Ada;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (From  : GNATCOM.Types.VARIANT)
      return Boolean
   is
      use type GNATCOM.Types.VARIANT_BOOL;

      Temp_Var : GNATCOM.Types.VARIANT := Copy (From);
      Result   : Boolean;
   begin
      Change_Type (Temp_Var, GNATCOM.Types.VT_BOOL);

      if Temp_Var.u.boolVal = GNATCOM.Types.VARIANT_BOOL_FALSE then
         Result := False;
      else
         Result := True;
      end if;
      VARIANT.Clear (Temp_Var);
      return Result;
   end To_Ada;

   -------------
   -- To_BSTR --
   -------------

   function To_BSTR
     (From : GNATCOM.Types.VARIANT;
      Copy : Boolean := False)
      return GNATCOM.Types.BSTR
   is
      use type GNATCOM.Types.VARTYPE;

      Temp_Var : GNATCOM.Types.VARIANT := From;
      Do_Copy  : Boolean := Copy;
   begin
      if From.vt /= GNATCOM.Types.VT_BSTR then
         Do_Copy := False;
      end if;

      Change_Type (Temp_Var, GNATCOM.Types.VT_BSTR);

      if Do_Copy then
         return SysAllocString (Temp_Var.u.bstrVal);
      else
         return Temp_Var.u.bstrVal;
      end if;

   end To_BSTR;

   ----------
   -- To_C --
   ----------

   function To_C
     (From  : GNATCOM.Types.VARIANT;
      Clear : Boolean               := True)
      return Interfaces.C.char_array
   is
      use type GNATCOM.Types.VARTYPE;

      Temp_Var : GNATCOM.Types.VARIANT := From;
      Do_Clear : Boolean := Clear;
   begin
      if Temp_Var.vt /= GNATCOM.Types.VT_BSTR then
         Do_Clear := True;
      end if;

      Change_Type (Temp_Var, GNATCOM.Types.VT_BSTR);

      return GNATCOM.BSTR.To_C (Temp_Var.u.bstrVal, Do_Clear);
   end To_C;

   ---------------
   -- To_C_Wide --
   ---------------

   function To_C_Wide
     (From  : GNATCOM.Types.VARIANT;
      Clear : Boolean               := True)
      return Interfaces.C.wchar_array
   is
      use type GNATCOM.Types.VARTYPE;

      Temp_Var : GNATCOM.Types.VARIANT := From;
      Do_Clear : Boolean := Clear;
   begin
      if Temp_Var.vt /= GNATCOM.Types.VT_BSTR then
         Do_Clear := True;
      end if;

      Change_Type (Temp_Var, GNATCOM.Types.VT_BSTR);

      return GNATCOM.BSTR.To_C_Wide (Temp_Var.u.bstrVal, Do_Clear);
   end To_C_Wide;

   -----------------------------
   -- To_Pointer_To_IDispatch --
   -----------------------------

   function To_Pointer_To_IDispatch
     (From  : GNATCOM.Types.VARIANT;
      Clear : Boolean                   := True)
      return GNATCOM.Types.Pointer_To_IDispatch
   is
      Temp_Var  : GNATCOM.Types.VARIANT := From;
      P_Interface : GNATCOM.Types.Pointer_To_IDispatch;
      Ref       : Interfaces.C.unsigned_long;
      pragma Warnings (Off, Ref);
   begin
      Change_Type (Temp_Var, GNATCOM.Types.VT_DISPATCH);
      P_Interface := Temp_Var.u.pdispVal;
      Ref := P_Interface.Vtbl.AddRef (P_Interface);

      if Clear then
         VARIANT.Clear (Temp_Var);
      end if;

      return P_Interface;
   end To_Pointer_To_IDispatch;

   ----------------------------
   -- To_Pointer_To_IUnknown --
   ----------------------------

   function To_Pointer_To_IUnknown
     (From  : GNATCOM.Types.VARIANT;
      Clear : Boolean                   := True)
      return GNATCOM.Types.Pointer_To_IUnknown
   is
      Temp_Var  : GNATCOM.Types.VARIANT := From;
      P_Interface : GNATCOM.Types.Pointer_To_IUnknown;
      Ref       : Interfaces.C.unsigned_long;
      pragma Warnings (Off, Ref);
   begin
      Change_Type (Temp_Var, GNATCOM.Types.VT_DISPATCH);
      P_Interface := Temp_Var.u.punkVal;
      Ref := P_Interface.Vtbl.AddRef (P_Interface);

      if Clear then
         VARIANT.Clear (Temp_Var);
      end if;

      return P_Interface;
   end To_Pointer_To_IUnknown;

   -----------------------------
   -- To_Pointer_To_SAFEARRAY --
   -----------------------------

   function To_Pointer_To_SAFEARRAY (From  : GNATCOM.Types.VARIANT)
     return GNATCOM.Types.Pointer_To_SAFEARRAY
   is
      use type GNATCOM.Types.VARTYPE;
   begin
      if (From.vt and GNATCOM.Types.VT_ARRAY) = 0 then
         raise INVALID_TYPE_ERROR;
      end if;

      return From.u.parray;
   end To_Pointer_To_SAFEARRAY;

   ----------------
   -- To_VARIANT --
   ----------------

   function To_VARIANT
     (From : String)
      return GNATCOM.Types.VARIANT
   is
      New_Variant : GNATCOM.Types.VARIANT;
   begin
      Initialize (New_Variant);
      New_Variant.vt := GNATCOM.Types.VT_BSTR;
      New_Variant.u.bstrVal := GNATCOM.BSTR.To_BSTR (From);
      return New_Variant;
   end To_VARIANT;

   ----------------
   -- To_VARIANT --
   ----------------

   function To_VARIANT
     (From : Integer;
      VT   : GNATCOM.Types.VARTYPE := GNATCOM.Types.VT_I4)
      return GNATCOM.Types.VARIANT
   is
      New_Variant : GNATCOM.Types.VARIANT;
   begin
      Initialize (New_Variant);
      New_Variant.vt := VT;
      New_Variant.u.lVal := Interfaces.C.long (From);
      return New_Variant;
   end To_VARIANT;

   ----------------
   -- To_VARIANT --
   ----------------

   function To_VARIANT
     (From : Float)
      return GNATCOM.Types.VARIANT
   is
      New_Variant : GNATCOM.Types.VARIANT;
   begin
      Initialize (New_Variant);
      New_Variant.vt := GNATCOM.Types.VT_R4;
      New_Variant.u.fltVal := Interfaces.C.C_float (From);
      return New_Variant;
   end To_VARIANT;

   ----------------
   -- To_VARIANT --
   ----------------

   function To_VARIANT
     (From : Boolean)
      return GNATCOM.Types.VARIANT
   is
      New_Variant : GNATCOM.Types.VARIANT;
   begin
      Initialize (New_Variant);
      New_Variant.vt := GNATCOM.Types.VT_BOOL;
      if From then
         New_Variant.u.boolVal := GNATCOM.Types.VARIANT_BOOL_TRUE;
      else
         New_Variant.u.boolVal := GNATCOM.Types.VARIANT_BOOL_FALSE;
      end if;
      return New_Variant;
   end To_VARIANT;

   ----------------
   -- To_VARIANT --
   ----------------

   function To_VARIANT
     (From : Ada.Calendar.Time)
      return GNATCOM.Types.VARIANT
   is
      use Ada.Calendar;
      use type Ada.Calendar.Day_Duration;
      use type Interfaces.C.int;
      use type Interfaces.C.short;

      New_Variant : GNATCOM.Types.VARIANT;
      C_Time      : SYSTEMTIME;
      Seconds     : Ada.Calendar.Day_Duration;
      Temp        : Ada.Calendar.Day_Duration;
      V_Time      : aliased GNATCOM.Types.DATE;
   begin
      C_Time.wYear   := Interfaces.C.short (Year (From));
      C_Time.wMonth  := Interfaces.C.short (Month (From));
      C_Time.wDay    := Interfaces.C.short (Day (From));

      Seconds := Ada.Calendar.Seconds (From);

      Temp := Seconds / (60*60);
      C_Time.wHour   :=
        Interfaces.C.short (Float'Floor (Float (Temp)));

      Temp := (Seconds - (Ada.Calendar.Day_Duration
                          (C_Time.wHour) * (60*60))) / 60;
      C_Time.wMinute :=
        Interfaces.C.short (Float'Floor (Float (Temp)));

      Temp := Seconds -
        ((Ada.Calendar.Day_Duration (C_Time.wHour) * (60 * 60)) +
         (Ada.Calendar.Day_Duration (C_Time.wMinute) * 60));
      C_Time.wSecond :=
        Interfaces.C.short (Float'Floor (Float (Temp)));

      if SystemTimeToVariantTime (C_Time, V_Time'Access) /= 0 then
         Initialize (New_Variant);
         New_Variant.vt := GNATCOM.Types.VT_DATE;
         New_Variant.u.date := V_Time;
         return New_Variant;
      else
         raise INVALID_TYPE_ERROR;
      end if;

   end To_VARIANT;

   ----------------
   -- To_VARIANT --
   ----------------

   function To_VARIANT
     (From : GNATCOM.Types.BSTR;
      Copy : Boolean := False)
      return GNATCOM.Types.VARIANT
   is
      New_Variant : GNATCOM.Types.VARIANT;
   begin
      Initialize (New_Variant);
      New_Variant.vt := GNATCOM.Types.VT_BSTR;
      if Copy then
         New_Variant.u.bstrVal := SysAllocString (From);
      else
         New_Variant.u.bstrVal := From;
      end if;
      return New_Variant;
   end To_VARIANT;

   ----------------
   -- To_VARIANT --
   ----------------

   function To_VARIANT
     (From   : GNATCOM.Types.Pointer_To_IUnknown;
      AddRef : Boolean := True)
      return GNATCOM.Types.VARIANT
   is
      Ref         : Interfaces.C.unsigned_long;
      pragma Warnings (Off, Ref);

      New_Variant : GNATCOM.Types.VARIANT;
   begin
      Initialize (New_Variant);
      New_Variant.vt := GNATCOM.Types.VT_UNKNOWN;
      if AddRef then
         Ref := From.Vtbl.AddRef (From);
      end if;
      New_Variant.u.punkVal := From;
      return New_Variant;
   end To_VARIANT;

   ----------------
   -- To_VARIANT --
   ----------------

   function To_VARIANT
     (From   : GNATCOM.Types.Pointer_To_IDispatch;
      AddRef : Boolean := True)
      return GNATCOM.Types.VARIANT
   is
      Ref         : Interfaces.C.unsigned_long;
      pragma Warnings (Off, Ref);

      New_Variant : GNATCOM.Types.VARIANT;
   begin
      Initialize (New_Variant);
      New_Variant.vt := GNATCOM.Types.VT_DISPATCH;
      if AddRef then
         Ref := From.Vtbl.AddRef (From);
      end if;
      New_Variant.u.pdispVal := From;
      return New_Variant;
   end To_VARIANT;

   ----------------
   -- To_VARIANT --
   ----------------

   function To_VARIANT (From : GNATCOM.Types.Pointer_To_SAFEARRAY;
                        VT   : GNATCOM.Types.VARTYPE)
     return GNATCOM.Types.VARIANT
   is
      use type GNATCOM.Types.VARTYPE;

      New_Variant : GNATCOM.Types.VARIANT;
   begin
      Initialize (New_Variant);
      New_Variant.vt := GNATCOM.Types.VT_ARRAY or VT;
      New_Variant.u.parray := From;
      return New_Variant;
   end To_VARIANT;

   ----------------------
   -- To_VARIANT_BYREF --
   ----------------------

   function To_VARIANT_BYREF (From : System.Address;
                              VT   : GNATCOM.Types.VARTYPE)
     return GNATCOM.Types.VARIANT
   is
      use type GNATCOM.Types.VARTYPE;

      RefVar : GNATCOM.Types.VARIANT;
   begin
      Initialize (RefVar);
      RefVar.vt := GNATCOM.Types.VT_BYREF + VT;
      RefVar.u.byref := From;

      return RefVar;
   end To_VARIANT_BYREF;

   ----------------------
   -- To_VARIANT_BYREF --
   ----------------------

   function To_VARIANT_BYREF (From : access GNATCOM.Types.BSTR)
     return GNATCOM.Types.VARIANT
   is
   begin
      return To_VARIANT_BYREF (From.all'Address, GNATCOM.Types.VT_BSTR);
   end To_VARIANT_BYREF;

   ----------------------
   -- To_VARIANT_BYREF --
   ----------------------

   function To_VARIANT_BYREF (From : access GNATCOM.Types.VARIANT)
     return GNATCOM.Types.VARIANT
   is
   begin
      return To_VARIANT_BYREF (From.all'Address, GNATCOM.Types.VT_VARIANT);
   end To_VARIANT_BYREF;

   -----------------------
   -- To_VARIANT_From_C --
   -----------------------

   function To_VARIANT_From_C
     (From : Interfaces.C.char_array)
      return GNATCOM.Types.VARIANT
   is
      New_Variant : GNATCOM.Types.VARIANT;
   begin
      Initialize (New_Variant);
      New_Variant.vt := GNATCOM.Types.VT_BSTR;
      New_Variant.u.bstrVal := GNATCOM.BSTR.To_BSTR_From_C (From);
      return New_Variant;
   end To_VARIANT_From_C;

   --------------------------
   -- To_VARIANT_From_Wide --
   --------------------------

   function To_VARIANT_From_Wide
     (From : Wide_String)
     return GNATCOM.Types.VARIANT
   is
      New_Variant : GNATCOM.Types.VARIANT;
   begin
      Initialize (New_Variant);
      New_Variant.vt := GNATCOM.Types.VT_BSTR;
      New_Variant.u.bstrVal := GNATCOM.BSTR.To_BSTR_From_Wide (From);
      return New_Variant;
   end To_VARIANT_From_Wide;

   ----------------------------
   -- To_VARIANT_From_Wide_C --
   ----------------------------

   function To_VARIANT_From_Wide_C
     (From : Interfaces.C.wchar_array)
      return GNATCOM.Types.VARIANT
   is
      New_Variant : GNATCOM.Types.VARIANT;
   begin
      Initialize (New_Variant);
      New_Variant.vt := GNATCOM.Types.VT_BSTR;
      New_Variant.u.bstrVal := GNATCOM.BSTR.To_BSTR_From_Wide_C (From);
      return New_Variant;
   end To_VARIANT_From_Wide_C;

   -------------
   -- Get_UDT --
   -------------

   function Get_UDT (From  : GNATCOM.Types.VARIANT) return Pointer_To_Element
   is
      use type Interfaces.C.unsigned_short;

      type BRECORD is
         record
            pvRecord  : Pointer_To_Element;
            pIRecInfo : GNATCOM.Types.Pointer_To_Void;
         end record;

      function To_BRECORD is
         new Ada.Unchecked_Conversion (Interfaces.C.double,
                                       BRECORD);

      Result : constant BRECORD := To_BRECORD (From.u.dblVal);
   begin
      if From.vt /= GNATCOM.Types.VT_RECORD then
         raise INVALID_TYPE_ERROR;
      end if;

      return Result.pvRecord;
   end Get_UDT;

   -------------
   -- Put_UDT --
   -------------

   function Put_UDT
     (UDT       : access Element;
      Lib_ID    : in     GNATCOM.Types.GUID;
      Ver_Maj   : in     Natural;
      Ver_Min   : in     Natural;
      Type_GUID : in     GNATCOM.Types.GUID)
     return GNATCOM.Types.VARIANT
   is
      use GNATCOM.ITypeLib_Interface;
      use GNATCOM.ITypeInfo_Interface;

      Lib       : GNATCOM.ITypeLib_Interface.ITypeLib_Type;
      Type_Lib  : aliased GNATCOM.Types.Pointer_To_ITypeLib;
      Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      GUID      : aliased GNATCOM.Types.GUID := Lib_ID;
      TGUID     : aliased GNATCOM.Types.GUID := Type_GUID;

      function LoadRegTypeLib
        (guid           : access GNATCOM.Types.GUID;
         wMaj           : Natural;
         wMin           : Natural;
         lcid           : Interfaces.C.long;
         pLib           : access GNATCOM.Types.Pointer_To_ITypeLib)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, LoadRegTypeLib, "LoadRegTypeLib");

      function Put_UDT_BI is new Put_UDT_By_Type_Info (Element);
   begin
      GNATCOM.Errors.Error_Check
        (LoadRegTypeLib
         (GUID'Access, Ver_Maj, Ver_Min, 0, Type_Lib'Access));

      Attach (Lib, Type_Lib);

      Attach (Type_Info, GetTypeInfoOfGuid (Lib, TGUID'Unchecked_Access));

      return Put_UDT_BI (UDT, Pointer (Type_Info));
   end Put_UDT;

   ----------------------
   -- Put_UDT_By_Index --
   ----------------------

   function Put_UDT_By_Index
     (UDT     : access Element;
      Lib_ID  : in     GNATCOM.Types.GUID;
      Ver_Maj : in     Natural;
      Ver_Min : in     Natural;
      Index   : in     Natural)
     return GNATCOM.Types.VARIANT
   is
      use GNATCOM.ITypeLib_Interface;
      use GNATCOM.ITypeInfo_Interface;

      Lib       : GNATCOM.ITypeLib_Interface.ITypeLib_Type;
      Type_Lib  : aliased GNATCOM.Types.Pointer_To_ITypeLib;
      Type_Info : GNATCOM.ITypeInfo_Interface.ITypeInfo_Type;
      GUID      : aliased GNATCOM.Types.GUID := Lib_ID;

      function LoadRegTypeLib
        (guid           : access GNATCOM.Types.GUID;
         wMaj           : Natural;
         wMin           : Natural;
         lcid           : Interfaces.C.long;
         pLib           : access GNATCOM.Types.Pointer_To_ITypeLib)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, LoadRegTypeLib, "LoadRegTypeLib");

      function Put_UDT_BI is new Put_UDT_By_Type_Info (Element);
   begin
      GNATCOM.Errors.Error_Check
        (LoadRegTypeLib
         (GUID'Access, Ver_Maj, Ver_Min, 0, Type_Lib'Access));

      Attach (Lib, Type_Lib);

      Attach (Type_Info, GetTypeInfo (Lib, Interfaces.C.int (Index)));

      return Put_UDT_BI (UDT, Pointer (Type_Info));
   end Put_UDT_By_Index;

   --------------------------
   -- Put_UDT_By_Type_Info --
   --------------------------

   function Put_UDT_By_Type_Info
     (UDT       : access Element;
      Type_Info : in     GNATCOM.Types.Pointer_To_ITypeInfo)
     return GNATCOM.Types.VARIANT
   is
      function GetRecordInfoFromTypeInfo
        (Info          : in     GNATCOM.Types.Pointer_To_ITypeInfo;
         ppRecord_Info : access GNATCOM.Types.Pointer_To_Void)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, GetRecordInfoFromTypeInfo,
                       "GetRecordInfoFromTypeInfo");

      Record_Info : aliased GNATCOM.Types.Pointer_To_Void;

      type Pointer_To_Element is access all Element;

      type BRECORD is
         record
            pvRecord  : Pointer_To_Element;
            pIRecInfo : GNATCOM.Types.Pointer_To_Void;
         end record;

      function To_double is
         new Ada.Unchecked_Conversion (BRECORD,
                                       Interfaces.C.double);
      New_Variant : GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (GetRecordInfoFromTypeInfo (Type_Info, Record_Info'Access));

      Initialize (New_Variant);
      New_Variant.vt := GNATCOM.Types.VT_RECORD;
      New_Variant.u.dblVal := To_double (BRECORD'(Pointer_To_Element (UDT),
                                                  Record_Info));

      return New_Variant;
   end Put_UDT_By_Type_Info;

end GNATCOM.VARIANT;
