------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                     G N A T C O M . V A R I A N T                        --
--                                                                          --
--                                S p e c                                   --
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

--  VARIANTs are a COM type representing any one of the OLE Automation
--  Types

with Ada.Calendar;
with Interfaces.C;
with System;

with GNATCOM.Types;

package GNATCOM.VARIANT is

   procedure Initialize (This : in out GNATCOM.Types.VARIANT);
   --  All new VARIANTs must be initialized first before first use.

   procedure Clear (This : in out GNATCOM.Types.VARIANT);
   --  Clears the contents of a VARIANT and deallocates and objects it may
   --  contain. Clear must be called on all VARIANTs containing BSTRs,
   --  COM interfaces, or SAFEARRAYs in order to release the used resources.

   procedure Free (This : in GNATCOM.Types.VARIANT);
   --  Frees any BSTR, COM Interfaces, or SAFEARRAYs contained in the
   --  variant, but doesn't clear out the values in the VARIANT

   function Copy (From : GNATCOM.Types.VARIANT)
     return GNATCOM.Types.VARIANT;
   --  Creates a copy of a VARIANT

   procedure Change_Type (This : in out GNATCOM.Types.VARIANT;
                          VT   : in     GNATCOM.Types.VARTYPE);
   --  Converts the type and contents of the VARIANT to the type VT

   function Is_NULL (This : GNATCOM.Types.VARIANT) return Boolean;
   --  Returns true if the variant contains NULL

   function To_VARIANT (From : String)
     return GNATCOM.Types.VARIANT;

   function To_VARIANT_From_Wide (From : Wide_String)
     return GNATCOM.Types.VARIANT;
   --  Allocates a BSTR in a new VARIANT from an Ada String

   function To_VARIANT_From_C (From : Interfaces.C.char_array)
     return GNATCOM.Types.VARIANT;

   function To_VARIANT_From_Wide_C (From : Interfaces.C.wchar_array)
     return GNATCOM.Types.VARIANT;
   --  Allocates a BSTR in a new VARIANT from a C String

   function To_VARIANT (From : Integer;
                        VT   : GNATCOM.Types.VARTYPE :=
                          GNATCOM.Types.VT_I4)
     return GNATCOM.Types.VARIANT;
   --  Creates a VARIANT from an Integer

   function To_VARIANT (From : Float) return GNATCOM.Types.VARIANT;
   --  Creates a VARIANT from a Float

   function To_VARIANT (From : Boolean) return GNATCOM.Types.VARIANT;
   --  Creates a VARIANT from a Boolean

   function To_VARIANT (From : Ada.Calendar.Time)
     return GNATCOM.Types.VARIANT;
   --  Creates a VARIANT from an Ada Time type

   function To_VARIANT (From : GNATCOM.Types.BSTR;
                        Copy : Boolean := False)
     return GNATCOM.Types.VARIANT;
   --  Creates a new VARIANT from a BSTR by either or attaching the BSTR
   --  to the VARIANT or attaching a copy of the BSTR

   function To_VARIANT (From   : GNATCOM.Types.Pointer_To_IUnknown;
                        AddRef : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function To_VARIANT (From   : GNATCOM.Types.Pointer_To_IDispatch;
                        AddRef : Boolean := True)
     return GNATCOM.Types.VARIANT;
   --  Creates a VARIANT from COM Interfaces

   function To_VARIANT (From : GNATCOM.Types.Pointer_To_SAFEARRAY;
                        VT   : GNATCOM.Types.VARTYPE)
     return GNATCOM.Types.VARIANT;
   --  Creates a VARIANT from a SAFEARRAY

   function To_Ada (From  : GNATCOM.Types.VARIANT;
                    Clear : Boolean                   := True)
     return String;

   function To_Ada_Wide (From  : GNATCOM.Types.VARIANT;
                         Clear : Boolean                   := True)
     return Wide_String;
   --  Returns an Ada string from a VARIANT and clears the BSTR in the
   --  VARIANT if Clear = TRUE
   --  If the contents of the Variant is a VT_NULL a "" is returned

   function To_Ada (From  : GNATCOM.Types.VARIANT)
     return Integer;

   function To_Ada (From  : GNATCOM.Types.VARIANT)
     return Float;

   function To_Ada (From  : GNATCOM.Types.VARIANT)
     return Ada.Calendar.Time;

   function To_Ada (From  : GNATCOM.Types.VARIANT)
     return Boolean;
   --  Returns Ada types from VARIANTs
   --  TYPE_MISMATCH_ERROR is raised if COM can not convert the contents
   --  of the Variant to the requested type.
   --  For example, if a variant contains -1,0,"True","False" it can be
   --  converted to a boolean, but any other value will raise the
   --  exception

   function To_C (From  : GNATCOM.Types.VARIANT;
                  Clear : Boolean                   := True)
     return Interfaces.C.char_array;

   function To_C_Wide (From  : GNATCOM.Types.VARIANT;
                       Clear : Boolean                   := True)
     return Interfaces.C.wchar_array;
   --  Returns a C string from a VARIANT and clears the BSTR in
   --  the VARIANT if Clear = True

   function To_BSTR (From : GNATCOM.Types.VARIANT;
                     Copy : Boolean                   := False)
     return GNATCOM.Types.BSTR;
   --  Extracts a BSTR from a VARIANT

   function To_Pointer_To_IUnknown (From  : GNATCOM.Types.VARIANT;
                                    Clear : Boolean               := True)
     return GNATCOM.Types.Pointer_To_IUnknown;

   function To_Pointer_To_IDispatch (From  : GNATCOM.Types.VARIANT;
                                     Clear : Boolean               := True)
     return GNATCOM.Types.Pointer_To_IDispatch;
   --  Extract COM interface pointers from a VARIANT

   function To_Pointer_To_SAFEARRAY (From  : GNATCOM.Types.VARIANT)
     return GNATCOM.Types.Pointer_To_SAFEARRAY;
   --  Extract a SAFEARRAY from a VARIANT

   function To_VARIANT_BYREF (From : System.Address;
                              VT   : GNATCOM.Types.VARTYPE)
     return GNATCOM.Types.VARIANT;

   function To_VARIANT_BYREF (From : access GNATCOM.Types.BSTR)
     return GNATCOM.Types.VARIANT;

   function To_VARIANT_BYREF (From : access GNATCOM.Types.VARIANT)
     return GNATCOM.Types.VARIANT;
   --  Creates a VARIANT containing a reference to an object

   generic
      type Element is private;
      type Pointer_To_Element is access all Element;
   function Get_UDT (From  : GNATCOM.Types.VARIANT) return Pointer_To_Element;
   --  Returns a UDT contained in a Variant

   generic
      type Element is private;
   function Put_UDT
     (UDT       : access Element;
      Lib_ID    : in     GNATCOM.Types.GUID;
      Ver_Maj   : in     Natural;
      Ver_Min   : in     Natural;
      Type_GUID : in     GNATCOM.Types.GUID)
     return GNATCOM.Types.VARIANT;
   --  Places a UDT in to a Variant

   generic
      type Element is private;
   function Put_UDT_By_Index
     (UDT     : access Element;
      Lib_ID  : in     GNATCOM.Types.GUID;
      Ver_Maj : in     Natural;
      Ver_Min : in     Natural;
      Index   : in     Natural)
     return GNATCOM.Types.VARIANT;
   --  Places a UDT in to a Variant

   generic
      type Element is private;
   function Put_UDT_By_Type_Info
     (UDT       : access Element;
      Type_Info : in     GNATCOM.Types.Pointer_To_ITypeInfo)
     return GNATCOM.Types.VARIANT;
   --  Places a UDT in to a Variant

   ARRAY_LOCKED_ERROR : exception;
   --  An operation is being attempt on a VARIANT containing a SAFEARRAY
   --  that has been locked

   INVALID_TYPE_ERROR : exception;
   --  An operation is being attempted on an invalid VARIANT, perhaps
   --  one that has not been initialized

   OVERFLOW_ERROR : exception;
   --  A type conversion created an oveflow for the new type

   TYPE_MISMATCH_ERROR : exception;
   --  VARIANT contents can not be converted to the requested type

end GNATCOM.VARIANT;
