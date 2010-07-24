------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                G N A T C O M . D I S P I N T E R F A C E                 --
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

with Ada.Exceptions;

with GNATCOM.BSTR;
with GNATCOM.VARIANT;
with GNATCOM.Errors;

package body GNATCOM.Dispinterface is

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT);
   --  Check for IDispatch specific errors

   DISPATCH_METHOD          : constant := 1;
   DISPATCH_PROPERTYGET     : constant := 2;
   DISPATCH_PROPERTYPUT     : constant := 4;
   DISPATCH_PROPERTYPUTREF  : constant := 8;
   --  DISPID_UNKNOWN           : constant := -1;
   --  DISPID_VALUE             : constant := 0;
   DISPID_PROPERTYPUT       : constant := -3;
   --  DISPID_NEWENUM           : constant := -4;
   --  DISPID_EVALUATE          : constant := -5;
   --  DISPID_CONSTRUCTOR       : constant := -6;
   --  DISPID_DESTRUCTOR        : constant := -7;
   DISP_E_PARAMNOTFOUND     : constant := 16#80020004#;
   DISP_E_TYPEMISMATCH      : constant := 16#80020005#;
   DISP_E_EXCEPTION         : constant := 16#80020009#;

   ------------
   -- Attach --
   ------------

   procedure Attach
     (This : in out Dispinterface_Type;
      From : in     GNATCOM.Types.Pointer_To_IDispatch)
   is
   begin
      Attach (This,
              GNATCOM.Iinterface.To_Pointer_To_IUnknown (From.all'Address));
   end Attach;

   ------------
   -- Attach --
   ------------

   procedure Attach
     (This : in out Dispinterface_Type;
      From : in     GNATCOM.Types.VARIANT)
   is
   begin
      Attach (This, GNATCOM.VARIANT.To_Pointer_To_IDispatch (From));
   end Attach;

   -----------------------------------
   -- To_VARIANT_From_Dispinterface --
   -----------------------------------

   function To_VARIANT_From_Dispinterface (From : Dispinterface_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      AddRef (From);
      return GNATCOM.VARIANT.To_VARIANT
        (To_Pointer_To_IDispatch (Address (From)));
   end To_VARIANT_From_Dispinterface;

   ---------
   -- Get --
   ---------

   function Get
     (This   : Dispinterface_Type;
      Name   : String;
      LCID   : Interfaces.C.long  := 0)
      return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Get_DISPID (This, Name), LCID);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (This   : Dispinterface_Type;
      DISPID : Interfaces.C.long;
      LCID   : Interfaces.C.long  := 0)
      return GNATCOM.Types.VARIANT
   is
      Parameters : Parameter_Array (1 .. 0);
   begin
      return Invoke (This,
                     DISPID,
                     DISPATCH_PROPERTYGET,
                     Parameters,
                     False,
                     LCID);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (This        : Dispinterface_Type;
      Name        : String;
      Index_Value : GNATCOM.Types.VARIANT;
      Free        : Boolean               := True;
      LCID        : Interfaces.C.long     := 0)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Get_DISPID (This, Name), Index_Value, Free, LCID);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (This        : Dispinterface_Type;
      DISPID      : Interfaces.C.long;
      Index_Value : GNATCOM.Types.VARIANT;
      Free        : Boolean               := True;
      LCID        : Interfaces.C.long     := 0)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This,
                     DISPID,
                     DISPATCH_PROPERTYGET,
                     (1 => Index_Value),
                     Free,
                     LCID);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (This         : Dispinterface_Type;
      Name         : String;
      Index_Values : Parameter_Array;
      Free         : Boolean            := True;
      LCID         : Interfaces.C.long  := 0)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Get_DISPID (This, Name), Index_Values, Free, LCID);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (This         : Dispinterface_Type;
      DISPID       : Interfaces.C.long;
      Index_Values : Parameter_Array;
      Free         : Boolean            := True;
      LCID         : Interfaces.C.long  := 0)
      return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This,
                     DISPID,
                     DISPATCH_PROPERTYGET,
                     Index_Values,
                     Free,
                     LCID);
   end Get;

   ----------------
   -- Get_DISPID --
   ----------------

   function Get_DISPID
     (This    : Dispinterface_Type;
      Of_Name : String)
      return Interfaces.C.long
   is
      PName     : aliased GNATCOM.Types.BSTR := GNATCOM.BSTR.To_BSTR (Of_Name);
      ID        : aliased Interfaces.C.long := 0;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetIDsOfNames (Pointer (This),
                                            GNATCOM.Types.GUID_NULL'Access,
                                            PName'Unchecked_Access,
                                            1,
                                            0,
                                            ID'Unchecked_Access));
      GNATCOM.BSTR.Free (PName);
      return ID;
   end Get_DISPID;

   -------------------
   -- Get_Type_Info --
   -------------------

   function Get_Type_Info
     (This : Dispinterface_Type;
      LCID : Interfaces.C.long  := 0)
      return GNATCOM.Types.Pointer_To_ITypeInfo
   is
      Info : aliased GNATCOM.Types.Pointer_To_ITypeInfo;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetTypeInfo (Pointer (This),
                                          0,
                                          LCID,
                                          Info'Unchecked_Access));
      return Info;
   end Get_Type_Info;

   -------------------
   -- Has_Type_Info --
   -------------------

   function Has_Type_Info (This : Dispinterface_Type) return Boolean is
      use type Interfaces.C.int;

      Count : aliased Interfaces.C.int;
   begin
      Error_Check
        (Pointer (This).Vtbl.GetTypeInfoCount (Pointer (This),
                                               Count'Unchecked_Access));
      if Count > 0 then
         return True;
      else
         return False;
      end if;
   end Has_Type_Info;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Dispinterface_Type) is
   begin
      Set_IID (This, GNATCOM.Types.IID_IDispatch);
   end Initialize;

   ------------
   -- Invoke --
   ------------

   function Invoke
     (This       : Dispinterface_Type;
      Name       : String;
      Parameters : Parameter_Array;
      Free       : Boolean            := True;
      LCID       : Interfaces.C.long  := 0)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, Get_DISPID (This, Name), Parameters, Free, LCID);
   end Invoke;

   ------------
   -- Invoke --
   ------------

   function Invoke
     (This       : Dispinterface_Type;
      DISPID     : Interfaces.C.long;
      Parameters : Parameter_Array;
      Free       : Boolean            := True;
      LCID       : Interfaces.C.long  := 0)
      return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This,
                     DISPID,
                     DISPATCH_METHOD,
                     Parameters,
                     Free,
                     LCID);
   end Invoke;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (This       : in Dispinterface_Type;
      Name       : in String;
      Parameters : in Parameter_Array;
      Free       : in Boolean            := True;
      LCID       : in Interfaces.C.long  := 0)
   is
   begin
      Invoke (This, Get_DISPID (This, Name), Parameters, Free, LCID);
   end Invoke;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (This       : in Dispinterface_Type;
      DISPID     : in Interfaces.C.long;
      Parameters : in Parameter_Array;
      Free       : in Boolean            := True;
      LCID       : in Interfaces.C.long  := 0)
   is
      Result : GNATCOM.Types.VARIANT;
      pragma Warnings (Off, Result);
   begin
      Result := Invoke (This,
                        DISPID,
                        DISPATCH_METHOD,
                        Parameters,
                        Free,
                        LCID);
   end Invoke;

   ------------
   -- Invoke --
   ------------

   function Invoke
     (This   : Dispinterface_Type;
      Name   : String;
      LCID   : Interfaces.C.long  := 0)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, Get_DISPID (This, Name), LCID);
   end Invoke;

   ------------
   -- Invoke --
   ------------

   function Invoke
     (This   : Dispinterface_Type;
      DISPID : Interfaces.C.long;
      LCID   : Interfaces.C.long  := 0)
     return GNATCOM.Types.VARIANT
   is
      Parameters : Parameter_Array (1 .. 0);
   begin
      return Invoke (This,
                     DISPID,
                     DISPATCH_METHOD,
                     Parameters,
                     False,
                     LCID);
   end Invoke;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (This   : in Dispinterface_Type;
      Name   : in String;
      LCID   : in Interfaces.C.long  := 0)
   is
   begin
      Invoke (This, Get_DISPID (This, Name), LCID);
   end Invoke;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (This   : in Dispinterface_Type;
      DISPID : in Interfaces.C.long;
      LCID   : in Interfaces.C.long  := 0)
   is
      Result : GNATCOM.Types.VARIANT;
      pragma Warnings (Off, Result);
   begin
      Result := Invoke (This, DISPID, LCID);
   end Invoke;

   ------------
   -- Invoke --
   ------------

   function Invoke
     (This       : Dispinterface_Type;
      Name       : String;
      wFlags     : Interfaces.C.short;
      Parameters : Parameter_Array;
      Free       : Boolean            := True;
      LCID       : Interfaces.C.long  := 0)
      return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This, Get_DISPID (This, Name), wFlags, Parameters, Free, LCID);
   end Invoke;

   ------------
   -- Invoke --
   ------------

   function Invoke
     (This       : Dispinterface_Type;
      DISPID     : Interfaces.C.long;
      wFlags     : Interfaces.C.short;
      Parameters : Parameter_Array;
      Free       : Boolean            := True;
      LCID       : Interfaces.C.long  := 0)
      return GNATCOM.Types.VARIANT
   is
      use type Interfaces.C.short;
      use type System.Address;

      function To_Pointer_To_VARIANT_PARAM_ARRAY is
         new Ada.Unchecked_Conversion
        (System.Address,
         GNATCOM.Types.Pointer_To_VARIANT_PARAM_ARRAY);

      function To_Pointer_To_DISPID_PARAM_ARRAY is
         new Ada.Unchecked_Conversion
        (System.Address,
         GNATCOM.Types.Pointer_To_DISPID_PARAM_ARRAY);

      HR             : GNATCOM.Types.HRESULT;
      No_Arguments   : aliased GNATCOM.Types.DISPPARAMS :=
        (null, null, 0, 0);
      Params         : aliased GNATCOM.Types.DISPPARAMS;
      Pdispparams    : GNATCOM.Types.Pointer_To_DISPPARAMS;
      Exception_Info : aliased GNATCOM.Types.EXCEPINFO;
      Argument_Error : aliased Interfaces.C.int;
      Result         : aliased GNATCOM.Types.VARIANT :=
        GNATCOM.Types.VARIANT_MISSING;
      Put_DISPID     : aliased Interfaces.C.long := DISPID_PROPERTYPUT;
      PDispatch      : constant GNATCOM.Types.Pointer_To_IDispatch :=
        Pointer (This);
   begin
      if Parameters'Length = 0 then
         Pdispparams := No_Arguments'Unchecked_Access;
      else
         Params.rgvarg := To_Pointer_To_VARIANT_PARAM_ARRAY
           (Parameters'Address);
         Params.cArgs := Parameters'Length;
         if wFlags = DISPATCH_PROPERTYPUT then
            Params.rgdispidNamedArgs := To_Pointer_To_DISPID_PARAM_ARRAY
              (Put_DISPID'Address);
            Params.cNamedArgs := 1;
         else
            Params.rgdispidNamedArgs := null;
            Params.cNamedArgs := 0;
         end if;

         Pdispparams := Params'Unchecked_Access;
      end if;

      HR := PDispatch.Vtbl.Invoke (PDispatch,
                                   DISPID,
                                   GNATCOM.Types.GUID_NULL'Access,
                                   LCID,
                                   wFlags,
                                   Pdispparams,
                                   Result'Unchecked_Access,
                                   Exception_Info'Unchecked_Access,
                                   Argument_Error'Unchecked_Access);

      if Free then
         for N in Parameters'Range loop
            declare
               Temp : GNATCOM.Types.VARIANT := Parameters (N);
            begin
               GNATCOM.VARIANT.Clear (Temp);
            end;
         end loop;
      end if;

      if GNATCOM.Errors.FAILED (HR) then
         if Exception_Info.pfnDeferredFillIn /= System.Null_Address then
            declare
               type DefferedFillIn_Type is
                 access procedure (pei : GNATCOM.Types.Pointer_To_EXCEPINFO);

               function To_Procedure is
                  new Ada.Unchecked_Conversion (System.Address,
                                                DefferedFillIn_Type);

               DefferedFillin : constant DefferedFillIn_Type :=
                 To_Procedure (Exception_Info.pfnDeferredFillIn'Address);
            begin
               DefferedFillin (Exception_Info'Unchecked_Access);
            end;
         end if;
      end if;

      case HR is
         when DISP_E_PARAMNOTFOUND =>
            Ada.Exceptions.Raise_Exception
              (PARAMETER_ERROR'Identity,
               "Parameter number" &
               Interfaces.C.int'Image (Argument_Error) &
               " not found");
         when DISP_E_TYPEMISMATCH =>
            Ada.Exceptions.Raise_Exception
              (TYPE_MISMATCH_ERROR'Identity,
               "Type mismatch in" &
               "parameter number" &
               Interfaces.C.int'Image (Argument_Error));
         when DISP_E_EXCEPTION =>
            Ada.Exceptions.Raise_Exception
              (INVOKE_ERROR'Identity,
               "Exception " &
               GNATCOM.BSTR.To_Ada (Exception_Info.bstrSource) &
               " - " &
               GNATCOM.BSTR.To_Ada (Exception_Info.bstrDescription));
         when others =>
            Error_Check (HR);
      end case;

      return Result;
   end Invoke;

   -------------
   -- Pointer --
   -------------

   function Pointer
     (This : Dispinterface_Type)
      return GNATCOM.Types.Pointer_To_IDispatch
   is
   begin
      return To_Pointer_To_IDispatch (Address (This));
   end Pointer;

   ---------
   -- Put --
   ---------

   procedure Put
     (This   : in Dispinterface_Type;
      Name   : in String;
      Value  : in GNATCOM.Types.VARIANT;
      Free   : in Boolean               := True;
      LCID   : in Interfaces.C.long     := 0)
   is
   begin
      Put (This, Get_DISPID (This, Name), Value, Free, LCID);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (This   : in Dispinterface_Type;
      DISPID : in Interfaces.C.long;
      Value  : in GNATCOM.Types.VARIANT;
      Free   : in Boolean               := True;
      LCID   : in Interfaces.C.long     := 0)
   is
      Result : GNATCOM.Types.VARIANT;
      pragma Warnings (Off, Result);
   begin
      Result := Invoke (This,
                        DISPID,
                        DISPATCH_PROPERTYPUT,
                        (1 => Value),
                        Free,
                        LCID);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (This        : in Dispinterface_Type;
      Name        : in String;
      Value       : in GNATCOM.Types.VARIANT;
      Index_Value : in GNATCOM.Types.VARIANT;
      Free        : in Boolean               := True;
      LCID        : in Interfaces.C.long     := 0)
   is
   begin
      Put (This, Get_DISPID (This, Name), Value, Index_Value, Free, LCID);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (This        : in Dispinterface_Type;
      DISPID      : in Interfaces.C.long;
      Value       : in GNATCOM.Types.VARIANT;
      Index_Value : in GNATCOM.Types.VARIANT;
      Free        : in Boolean               := True;
      LCID        : in Interfaces.C.long     := 0)
   is
      Result : GNATCOM.Types.VARIANT;
      pragma Warnings (Off, Result);
   begin
      Result := Invoke (This,
                        DISPID,
                        DISPATCH_PROPERTYPUT,
                        (Value, Index_Value),
                        Free,
                        LCID);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (This         : in Dispinterface_Type;
      Name         : in String;
      Value        : in GNATCOM.Types.VARIANT;
      Index_Values : in Parameter_Array;
      Free         : in Boolean               := True;
      LCID         : in Interfaces.C.long     := 0)
   is
   begin
      Put (This, Get_DISPID (This, Name), Value, Index_Values, Free, LCID);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (This         : in Dispinterface_Type;
      DISPID       : in Interfaces.C.long;
      Value        : in GNATCOM.Types.VARIANT;
      Index_Values : in Parameter_Array;
      Free         : in Boolean               := True;
      LCID         : in Interfaces.C.long     := 0)
   is
      Result     : GNATCOM.Types.VARIANT;
      pragma Warnings (Off, Result);

      Parameters : Parameter_Array (1 .. Index_Values'Size + 1);
   begin
      Parameters (1) := Value;
      Parameters (2 .. Parameters'Last) := Index_Values;

      Result := Invoke (This,
                        DISPID,
                        DISPATCH_PROPERTYPUT,
                        Parameters,
                        Free,
                        LCID);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (This       : in Dispinterface_Type;
      Name       : in String;
      Parameters : in Parameter_Array;
      Free       : in Boolean            := True;
      LCID       : in Interfaces.C.long  := 0)
   is
   begin
      Put (This, Get_DISPID (This, Name), Parameters, Free, LCID);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (This       : in Dispinterface_Type;
      DISPID     : in Interfaces.C.long;
      Parameters : in Parameter_Array;
      Free       : in Boolean            := True;
      LCID       : in Interfaces.C.long  := 0)
   is
      Result : GNATCOM.Types.VARIANT;
      pragma Warnings (Off, Result);
   begin
      Result := Invoke (This,
                        DISPID,
                        DISPATCH_PROPERTYPUT,
                        Parameters,
                        Free,
                        LCID);
   end Put;

   ------------
   -- PutRef --
   ------------

   procedure PutRef
     (This   : in Dispinterface_Type;
      Name   : in String;
      Value  : in GNATCOM.Types.VARIANT;
      LCID   : in Interfaces.C.long     := 0)
   is
   begin
      PutRef (This, Get_DISPID (This, Name), Value, LCID);
   end PutRef;

   ------------
   -- PutRef --
   ------------

   procedure PutRef
     (This   : in Dispinterface_Type;
      DISPID : in Interfaces.C.long;
      Value  : in GNATCOM.Types.VARIANT;
      LCID   : in Interfaces.C.long     := 0)
   is
      Result : GNATCOM.Types.VARIANT;
      pragma Warnings (Off, Result);
   begin
      Result := Invoke (This,
                        DISPID,
                        DISPATCH_PROPERTYPUTREF,
                        (1 => Value),
                        False,
                        LCID);
   end PutRef;

   ------------
   -- PutRef --
   ------------

   procedure PutRef
     (This       : in Dispinterface_Type;
      Name       : in String;
      Parameters : in Parameter_Array;
      LCID       : in Interfaces.C.long  := 0)
   is
   begin
      PutRef (This, Get_DISPID (This, Name), Parameters, LCID);
   end PutRef;

   ------------
   -- PutRef --
   ------------

   procedure PutRef
     (This       : in Dispinterface_Type;
      DISPID     : in Interfaces.C.long;
      Parameters : in Parameter_Array;
      LCID       : in Interfaces.C.long  := 0)
   is
      Result : GNATCOM.Types.VARIANT;
      pragma Warnings (Off, Result);
   begin
      Result := Invoke (This,
                        DISPID,
                        DISPATCH_PROPERTYPUTREF,
                        Parameters,
                        False,
                        LCID);
   end PutRef;

   -----------------
   -- Error_Check --
   -----------------

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT) is
   begin
      GNATCOM.Errors.Set_Last_HRESULT (Result);

      if GNATCOM.Errors.FAILED (Result) then
         declare
            Message : constant String := GNATCOM.Errors.To_String (Result);
         begin
            case Result is
               when DISP_E_UNKNOWNNAME =>
                  Ada.Exceptions.Raise_Exception
                    (UNKNOWN_NAME_ERROR'Identity,
                     Message);
               when DISP_E_UNKNOWNLCID =>
                  Ada.Exceptions.Raise_Exception
                    (UNKNOWN_LCID_ERROR'Identity,
                     Message);
               when DISP_E_BADINDEX =>
                  Ada.Exceptions.Raise_Exception
                    (ELEMENT_NOT_FOUND_ERROR'Identity,
                     Message);
               when TYPE_E_ELEMENTNOTFOUND =>
                  Ada.Exceptions.Raise_Exception
                    (ELEMENT_NOT_FOUND_ERROR'Identity,
                     Message);
               when DISP_E_PARAMNOTOPTIONAL =>
                  Ada.Exceptions.Raise_Exception
                    (PARAMETER_ERROR'Identity,
                     Message);
               when DISP_E_BADPARAMCOUNT =>
                  Ada.Exceptions.Raise_Exception
                    (PARAMETER_ERROR'Identity,
                     Message);
               when DISP_E_BADVARTYPE =>
                  Ada.Exceptions.Raise_Exception
                    (TYPE_MISMATCH_ERROR'Identity,
                     Message);
               when DISP_E_MEMBERNOTFOUND =>
                  Ada.Exceptions.Raise_Exception
                    (UNKNOWN_NAME_ERROR'Identity,
                     Message);
               when DISP_E_NONAMEDARGS =>
                  Ada.Exceptions.Raise_Exception
                    (TYPE_MISMATCH_ERROR'Identity,
                     Message);
               when DISP_E_OVERFLOW =>
                  Ada.Exceptions.Raise_Exception
                    (TYPE_MISMATCH_ERROR'Identity,
                     Message);
               when others =>
                  GNATCOM.Errors.Error_Check (Result);
            end case;
         end;
      end if;
   end Error_Check;

end GNATCOM.Dispinterface;
