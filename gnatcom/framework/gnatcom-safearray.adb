------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                    G N A T C O M . S A F E A R R A Y                     --
--                                                                          --
--                                B o d y                                   --
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
-- for  more details.  You should have  recived  a copy of the GNU General --
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
with Interfaces.C;
with System;

with GNATCOM.VARIANT;
with GNATCOM.BSTR;
with GNATCOM.Errors;

with GNATCOM.ITypeInfo_Interface;
with GNATCOM.ITypeLib_Interface;

package body GNATCOM.SafeArray is

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT);
   --  Check for SAFEARRAY specific errors

   function SafeArrayCreateVector
     (vt        : Interfaces.C.unsigned_short;
      lLBound   : Interfaces.C.long;
      cElements : Interfaces.C.unsigned)
     return GNATCOM.Types.Pointer_To_SAFEARRAY;
   pragma Import (StdCall, SafeArrayCreateVector, "SafeArrayCreateVector");

   function SafeArrayCopy
     (psa     : access GNATCOM.Types.SAFEARRAY;
      ppsaOut : access GNATCOM.Types.Pointer_To_SAFEARRAY)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, SafeArrayCopy, "SafeArrayCopy");

   function SafeArrayGetDim
     (psa : access GNATCOM.Types.SAFEARRAY)
     return Interfaces.C.unsigned;
   pragma Import (StdCall, SafeArrayGetDim, "SafeArrayGetDim");

   function SafeArrayGetLBound
     (psa      : access GNATCOM.Types.SAFEARRAY;
      nDim     : in     Interfaces.C.unsigned;
      plLbound : access Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, SafeArrayGetLBound, "SafeArrayGetLBound");

   function SafeArrayGetUBound
     (psa      : access GNATCOM.Types.SAFEARRAY;
      nDim     : in     Interfaces.C.unsigned;
      plUBound : access Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, SafeArrayGetUBound, "SafeArrayGetUBound");

   function SafeArrayDestroy
     (psa      : access GNATCOM.Types.SAFEARRAY)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, SafeArrayDestroy, "SafeArrayDestroy");

   function SafeArrayAllocData
     (psa      : access GNATCOM.Types.SAFEARRAY)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, SafeArrayAllocData, "SafeArrayAllocData");

   function SafeArrayAllocDescriptor
     (cDims   : in     Interfaces.C.unsigned;
      ppsaOut : access GNATCOM.Types.Pointer_To_SAFEARRAY)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, SafeArrayAllocDescriptor,
                    "SafeArrayAllocDescriptor");

   procedure SafeArrayDestroyDescriptor
     (psa      : access GNATCOM.Types.SAFEARRAY);
   pragma Import (StdCall, SafeArrayDestroyDescriptor,
                    "SafeArrayDestroyDescriptor");

   -- Copy --

   function Copy
     (From : access GNATCOM.Types.SAFEARRAY)
      return GNATCOM.Types.Pointer_To_SAFEARRAY
   is
      Temp : aliased GNATCOM.Types.Pointer_To_SAFEARRAY;
   begin
      Error_Check
        (SafeArrayCopy (From, Temp'Access));
      return Temp;
   end Copy;

   -- Create --

   function Create
     (VT          : GNATCOM.Types.VARTYPE;
      Lower_Bound : Integer;
      Elements    : Natural)
      return GNATCOM.Types.Pointer_To_SAFEARRAY
   is
      use type GNATCOM.Types.Pointer_To_SAFEARRAY;

      Temp : GNATCOM.Types.Pointer_To_SAFEARRAY;
   begin
      Temp := SafeArrayCreateVector (VT,
                                     Interfaces.C.long (Lower_Bound),
                                     Interfaces.C.unsigned (Elements));
      if Temp = null then
         raise ARRAY_CREATION_ERROR;
      end if;

      return Temp;
   end Create;

   -- Create --

   function Create
     (VT     : GNATCOM.Types.VARTYPE;
      Bounds : SafeArray_Bounds)
      return GNATCOM.Types.Pointer_To_SAFEARRAY
   is
      use type GNATCOM.Types.Pointer_To_SAFEARRAY;

      type Bound_Array is
        array (Bounds'Range) of GNATCOM.Types.SAFEARRAYBOUND;

      function SafeArrayCreate
        (vt        : in     Interfaces.C.unsigned_short;
         cDims     : in     Interfaces.C.unsigned;
         rgsabound : access Bound_Array)
        return GNATCOM.Types.Pointer_To_SAFEARRAY;
      pragma Import (StdCall, SafeArrayCreate, "SafeArrayCreate");

      Temp    : GNATCOM.Types.Pointer_To_SAFEARRAY;
      SABound : aliased Bound_Array;
   begin
      if Bounds'Length = 0 then
         raise ARRAY_CREATION_ERROR;
      end if;

      for N in Bounds'Range loop
         SABound (N).cElements :=
           Interfaces.C.unsigned_long (Bounds (N).Elements);
         SABound (N).lLbound :=
           Interfaces.C.long (Bounds (N).Lower_Bound);
      end loop;

      Temp := SafeArrayCreate (VT,
                               Bounds'Length,
                               SABound'Access);

      if Temp = null then
         raise ARRAY_CREATION_ERROR;
      end if;

      return Temp;
   end Create;

   -- Create --

   function Create
     (Lib_ID    : GNATCOM.Types.GUID;
      Ver_Maj   : Natural;
      Ver_Min   : Natural;
      Type_GUID : GNATCOM.Types.GUID;
      Bounds    : SafeArray_Bounds)
     return GNATCOM.Types.Pointer_To_SAFEARRAY
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

   begin
      GNATCOM.Errors.Error_Check
        (LoadRegTypeLib
         (GUID'Access, Ver_Maj, Ver_Min, 0, Type_Lib'Access));

      Attach (Lib, Type_Lib);

      Attach (Type_Info, GetTypeInfoOfGuid (Lib, TGUID'Unchecked_Access));

      return Create (Pointer (Type_Info), Bounds);
   end Create;

   -- Create --

   function Create
     (Lib_ID      : GNATCOM.Types.GUID;
      Ver_Maj     : Natural;
      Ver_Min     : Natural;
      Type_GUID   : GNATCOM.Types.GUID;
      Lower_Bound : Integer;
      Elements    : Natural)
     return GNATCOM.Types.Pointer_To_SAFEARRAY
   is
   begin
      return Create (Lib_ID, Ver_Maj, Ver_Min, Type_GUID, (1 => (Lower_Bound,
                                                                 Elements)));
   end Create;

   -- Create --

   function Create
     (Lib_ID      : GNATCOM.Types.GUID;
      Ver_Maj     : Natural;
      Ver_Min     : Natural;
      Index       : Natural;
      Lower_Bound : Integer;
      Elements    : Natural)
     return GNATCOM.Types.Pointer_To_SAFEARRAY
   is
   begin
      return Create (Lib_ID, Ver_Maj, Ver_Min, Index, (1 => (Lower_Bound,
                                                             Elements)));
   end Create;

   -- Create --

   function Create
     (Lib_ID  : GNATCOM.Types.GUID;
      Ver_Maj : Natural;
      Ver_Min : Natural;
      Index   : Natural;
      Bounds  : SafeArray_Bounds)
     return GNATCOM.Types.Pointer_To_SAFEARRAY
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

   begin
      GNATCOM.Errors.Error_Check
        (LoadRegTypeLib
         (GUID'Access, Ver_Maj, Ver_Min, 0, Type_Lib'Access));

      Attach (Lib, Type_Lib);

      Attach (Type_Info, GetTypeInfo (Lib, Interfaces.C.int (Index)));

      return Create (Pointer (Type_Info), Bounds);
   end Create;

   -- Create --

   function Create
     (Type_Info   : GNATCOM.Types.Pointer_To_ITypeInfo;
      Lower_Bound : Integer;
      Elements    : Natural)
     return GNATCOM.Types.Pointer_To_SAFEARRAY
   is
   begin
      return Create (Type_Info, (1 => (Lower_Bound, Elements)));
   end Create;

   -- Create --

   function Create
     (Type_Info : GNATCOM.Types.Pointer_To_ITypeInfo;
      Bounds    : SafeArray_Bounds)
     return GNATCOM.Types.Pointer_To_SAFEARRAY
   is
      function GetRecordInfoFromTypeInfo
        (Info          : in     GNATCOM.Types.Pointer_To_ITypeInfo;
         ppRecord_Info : access GNATCOM.Types.Pointer_To_Void)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, GetRecordInfoFromTypeInfo,
                       "GetRecordInfoFromTypeInfo");

      Record_Info : aliased GNATCOM.Types.Pointer_To_Void;

      use type GNATCOM.Types.Pointer_To_SAFEARRAY;

      type Bound_Array is
        array (Bounds'Range) of GNATCOM.Types.SAFEARRAYBOUND;

      function SafeArrayCreateEx
        (vt        : in     Interfaces.C.unsigned_short;
         cDims     : in     Interfaces.C.unsigned;
         rgsabound : access Bound_Array;
         pRecInfo  : in     GNATCOM.Types.Pointer_To_Void)
        return GNATCOM.Types.Pointer_To_SAFEARRAY;
      pragma Import (StdCall, SafeArrayCreateEx, "SafeArrayCreateEx");

      Temp    : GNATCOM.Types.Pointer_To_SAFEARRAY;
      SABound : aliased Bound_Array;
   begin
      GNATCOM.Errors.Error_Check
        (GetRecordInfoFromTypeInfo (Type_Info, Record_Info'Access));

      if Bounds'Length = 0 then
         raise ARRAY_CREATION_ERROR;
      end if;

      for N in Bounds'Range loop
         SABound (N).cElements :=
           Interfaces.C.unsigned_long (Bounds (N).Elements);
         SABound (N).lLbound :=
           Interfaces.C.long (Bounds (N).Lower_Bound);
      end loop;

      Temp := SafeArrayCreateEx (GNATCOM.Types.VT_RECORD,
                                 Bounds'Length,
                                 SABound'Access,
                                 Record_Info);

      if Temp = null then
         raise ARRAY_CREATION_ERROR;
      end if;

      return Temp;
   end Create;

   -- Create_Custom --

   function Create_Custom (Size_Of_Element : Positive;
                           Lower_Bound     : Integer;
                           Elements        : Natural)
                          return GNATCOM.Types.Pointer_To_SAFEARRAY
   is
   begin
      return Create_Custom (Size_Of_Element, (1 => (Lower_Bound, Elements)));
   end Create_Custom;

   -- Create_Custom --

   function Create_Custom (Size_Of_Element : Positive;
                           Bounds          : SafeArray_Bounds)
                          return GNATCOM.Types.Pointer_To_SAFEARRAY
   is
      use type GNATCOM.Types.HRESULT;
      use GNATCOM.Types;

      Result  : aliased GNATCOM.Types.Pointer_To_SAFEARRAY;

      type SA_Bounds_Type is
        array (Bounds'Range) of GNATCOM.Types.SAFEARRAYBOUND;

      type Pointer_To_SA_Bounds is access all SA_Bounds_Type;

      function To_Pointer_To_SA_Bounds is
         new Ada.Unchecked_Conversion (System.Address,
                                       Pointer_To_SA_Bounds);

      SABound : Pointer_To_SA_Bounds;
   begin
      if Bounds'Length = 0 then
         raise ARRAY_CREATION_ERROR;
      end if;

      if
        SafeArrayAllocDescriptor (Bounds'Length, Result'Access) /=
        GNATCOM.S_OK
      then
         raise ARRAY_CREATION_ERROR;
      end if;

      Result.cbElements := Interfaces.C.unsigned_long (Size_Of_Element);

      SABound := To_Pointer_To_SA_Bounds (Result.rgsabound'Address);

      for N in SABound.all'Range loop
         SABound (N).cElements :=
           Interfaces.C.unsigned_long (Bounds (N).Elements);
         SABound (N).lLbound :=
           Interfaces.C.long (Bounds (N).Lower_Bound);
      end loop;

      if SafeArrayAllocData (Result) /= GNATCOM.S_OK then
         SafeArrayDestroyDescriptor (Result);
         raise ARRAY_CREATION_ERROR;
      end if;

      return Result;
   end Create_Custom;

   -- Free --

   procedure Free (This : access GNATCOM.Types.SAFEARRAY) is
      use type Interfaces.C.unsigned_short;
      use type Interfaces.C.unsigned;

      FADF_VARIANT : constant := 16#0800#;
   begin
      if
        (Interfaces.C.unsigned (This.fFeatures) and FADF_VARIANT)
        =
        FADF_VARIANT
      then
         declare
            TempVar : GNATCOM.Types.VARIANT;
         begin
            --  By placing the SAFEARRAY first in to a VARIANT the
            --  contents of the SAFEARRAY of VARIANTS will be cleared

            GNATCOM.VARIANT.Initialize (TempVar);
            TempVar.vt := GNATCOM.Types.VT_ARRAY +
              GNATCOM.Types.VT_VARIANT;
            TempVar.u.parray := GNATCOM.Types.Pointer_To_SAFEARRAY (This);
            GNATCOM.VARIANT.Free (TempVar);
         end;
      else
         Error_Check
           (SafeArrayDestroy (This));
      end if;

--      Error_Check (SafeArrayDestroyData (This));
--      Error_Check (SafeArrayDestroy (This));
   exception
      when GNATCOM.VARIANT.ARRAY_LOCKED_ERROR =>
         raise ARRAY_LOCKED_ERROR;
   end Free;

   -- Get_Dimensions --

   function Get_Dimensions
     (Of_Array : access GNATCOM.Types.SAFEARRAY)
      return Positive
   is
   begin
      return Positive (SafeArrayGetDim (Of_Array));
   end Get_Dimensions;

   -- Get_Element --

   function Get_Element
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Integer)
      return Element
   is
      type Index_Array is array (1 .. 1) of Interfaces.C.long;

      function SafeArrayGetElement
        (psa       : access GNATCOM.Types.SAFEARRAY;
         rgIndices : access Index_Array;
         pv        : access Element)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, SafeArrayGetElement, "SafeArrayGetElement");

      SAIndex   : aliased Index_Array;
      SAElement : aliased Element;
   begin
      SAIndex (1) := Interfaces.C.long (Index);
      Error_Check
        (SafeArrayGetElement (Of_Array,
                              SAIndex'Access,
                              SAElement'Access));
      return SAElement;
   end Get_Element;

   -- Get_Lower_Bound --

   function Get_Lower_Bound
     (Of_Array  : access GNATCOM.Types.SAFEARRAY;
      Dimension : in     Positive                := 1)
      return Integer
   is
      LBound : aliased Interfaces.C.long;
   begin
      Error_Check
        (SafeArrayGetLBound (Of_Array,
                             Interfaces.C.unsigned (Dimension),
                             LBound'Access));

      return Integer (LBound);
   end Get_Lower_Bound;

   -- Get_Upper_Bound --

   function Get_Upper_Bound
     (Of_Array  : access GNATCOM.Types.SAFEARRAY;
      Dimension : in     Positive                := 1)
      return Integer
   is
      UBound : aliased Interfaces.C.long;
   begin
      Error_Check
        (SafeArrayGetUBound (Of_Array,
                             Interfaces.C.unsigned (Dimension),
                             UBound'Access));

      return Integer (UBound);
   end Get_Upper_Bound;

   -- Get_Value --

   function Get_Value
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Index_Array)
      return Element
   is
      type Index_Array is array (Index'Range) of Interfaces.C.long;

      function SafeArrayGetElement
        (psa       : access GNATCOM.Types.SAFEARRAY;
         rgIndices : access Index_Array;
         pv        : access Element)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, SafeArrayGetElement, "SafeArrayGetElement");

      SAIndex   : aliased Index_Array;
      SAElement : aliased Element;
   begin
      for N in Index'Range loop
         SAIndex (N) := Interfaces.C.long (Index (N));
      end loop;

      Error_Check
        (SafeArrayGetElement (Of_Array,
                              SAIndex'Access,
                              SAElement'Access));
      return SAElement;
   end Get_Value;

   -- Put_Element --

   procedure Put_Element
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Integer;
      Value    : in     Element)
   is
      type Index_Array is array (1 .. 1) of Interfaces.C.long;

      function SafeArrayPutElement
        (psa       : access GNATCOM.Types.SAFEARRAY;
         rgIndices : access Index_Array;
         pv        : access Element)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, SafeArrayPutElement, "SafeArrayPutElement");

      SAIndex   : aliased Index_Array;
      SAElement : aliased Element := Value;
   begin
      SAIndex (1) := Interfaces.C.long (Index);
      Error_Check
        (SafeArrayPutElement (Of_Array,
                              SAIndex'Access,
                              SAElement'Access));
   end Put_Element;

   -- Put_Value --

   procedure Put_Value
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Index_Array;
      Value    : in     Element)
   is
      type Index_Array is array (Index'Range) of Interfaces.C.long;

      function SafeArrayPutElement
        (psa       : access GNATCOM.Types.SAFEARRAY;
         rgIndices : access Index_Array;
         pv        : access Element)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, SafeArrayPutElement, "SafeArrayPutElement");

      SAIndex   : aliased Index_Array;
      SAElement : aliased Element := Value;
   begin
      for N in Index'Range loop
         SAIndex (N) := Interfaces.C.long (Index (N));
      end loop;

      Error_Check
        (SafeArrayPutElement (Of_Array,
                              SAIndex'Access,
                              SAElement'Access));
   end Put_Value;

   -- Put_IUnknown --

   procedure Put_IUnknown
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Integer;
      Value    : in     GNATCOM.Types.Pointer_To_IUnknown)
   is
      type Index_Array is array (1 .. 1) of Interfaces.C.long;

      function SafeArrayPutElement
        (psa       : access GNATCOM.Types.SAFEARRAY;
         rgIndices : access Index_Array;
         pv        : in     GNATCOM.Types.Pointer_To_IUnknown)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, SafeArrayPutElement, "SafeArrayPutElement");

      SAIndex   : aliased Index_Array;
   begin
      SAIndex (1) := Interfaces.C.long (Index);

      Error_Check
        (SafeArrayPutElement (Of_Array,
                              SAIndex'Access,
                              Value));
   end Put_IUnknown;

   -- Put_IUnknown --

   procedure Put_IUnknown
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Index_Array;
      Value    : in     GNATCOM.Types.Pointer_To_IUnknown)
   is
      type Index_Array is array (Index'Range) of Interfaces.C.long;

      function SafeArrayPutElement
        (psa       : access GNATCOM.Types.SAFEARRAY;
         rgIndices : access Index_Array;
         pv        : GNATCOM.Types.Pointer_To_IUnknown)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, SafeArrayPutElement, "SafeArrayPutElement");

      SAIndex   : aliased Index_Array;
   begin
      for N in Index'Range loop
         SAIndex (N) := Interfaces.C.long (Index (N));
      end loop;

      Error_Check
        (SafeArrayPutElement (Of_Array,
                              SAIndex'Access,
                              Value));
   end Put_IUnknown;

   -- Put_IDispatch --

   procedure Put_IDispatch
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Integer;
      Value    : in     GNATCOM.Types.Pointer_To_IDispatch)
   is
      type Index_Array is array (1 .. 1) of Interfaces.C.long;

      function SafeArrayPutElement
        (psa       : access GNATCOM.Types.SAFEARRAY;
         rgIndices : access Index_Array;
         pv        : in     GNATCOM.Types.Pointer_To_IDispatch)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, SafeArrayPutElement, "SafeArrayPutElement");

      SAIndex   : aliased Index_Array;
   begin
      SAIndex (1) := Interfaces.C.long (Index);

      Error_Check
        (SafeArrayPutElement (Of_Array,
                              SAIndex'Access,
                              Value));
   end Put_IDispatch;

   -- Put_IDispatch --

   procedure Put_IDispatch
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Index_Array;
      Value    : in     GNATCOM.Types.Pointer_To_IDispatch)
   is
      type Index_Array is array (Index'Range) of Interfaces.C.long;

      function SafeArrayPutElement
        (psa       : access GNATCOM.Types.SAFEARRAY;
         rgIndices : access Index_Array;
         pv        : GNATCOM.Types.Pointer_To_IDispatch)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, SafeArrayPutElement, "SafeArrayPutElement");

      SAIndex   : aliased Index_Array;
   begin
      for N in Index'Range loop
         SAIndex (N) := Interfaces.C.long (Index (N));
      end loop;

      Error_Check
        (SafeArrayPutElement (Of_Array,
                              SAIndex'Access,
                              Value));
   end Put_IDispatch;

   -- Put_BSTR --

   procedure Put_BSTR
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Integer;
      Value    : in     GNATCOM.Types.BSTR;
      Clear    : in     Boolean                 := True)
   is
      type Index_Array is array (1 .. 1) of Interfaces.C.long;

      function SafeArrayPutElement
        (psa       : access GNATCOM.Types.SAFEARRAY;
         rgIndices : access Index_Array;
         pv        : in     GNATCOM.Types.BSTR)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, SafeArrayPutElement, "SafeArrayPutElement");

      SAIndex   : aliased Index_Array;
   begin
      SAIndex (1) := Interfaces.C.long (Index);

      Error_Check
        (SafeArrayPutElement (Of_Array,
                              SAIndex'Access,
                              Value));

      if Clear then
         GNATCOM.BSTR.Free (Value);
      end if;
   end Put_BSTR;

   -- Put_BSTR --

   procedure Put_BSTR
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Index_Array;
      Value    : in     GNATCOM.Types.BSTR;
      Clear    : in     Boolean                 := True)
   is
      type Index_Array is array (Index'Range) of Interfaces.C.long;

      function SafeArrayPutElement
        (psa       : access GNATCOM.Types.SAFEARRAY;
         rgIndices : access Index_Array;
         pv        : in     GNATCOM.Types.BSTR)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, SafeArrayPutElement, "SafeArrayPutElement");

      SAIndex   : aliased Index_Array;
   begin
      for N in Index'Range loop
         SAIndex (N) := Interfaces.C.long (Index (N));
      end loop;

      Error_Check
        (SafeArrayPutElement (Of_Array,
                              SAIndex'Access,
                              Value));

      if Clear then
         GNATCOM.BSTR.Free (Value);
      end if;
   end Put_BSTR;

   -- Put_VARIANT --

   procedure Put_VARIANT
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Integer;
      Value    : in     GNATCOM.Types.VARIANT;
      Clear    : in     Boolean                 := True)
   is
      type Index_Array is array (1 .. 1) of Interfaces.C.long;

      function SafeArrayPutElement
        (psa       : access GNATCOM.Types.SAFEARRAY;
         rgIndices : access Index_Array;
         pv        : access GNATCOM.Types.VARIANT)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, SafeArrayPutElement, "SafeArrayPutElement");

      SAIndex   : aliased Index_Array;
      SAElement : aliased GNATCOM.Types.VARIANT := Value;
   begin
      SAIndex (1) := Interfaces.C.long (Index);

      Error_Check
        (SafeArrayPutElement (Of_Array,
                              SAIndex'Access,
                              SAElement'Access));

      if Clear then
         GNATCOM.VARIANT.Free (Value);
      end if;
   end Put_VARIANT;

   -- Put_VARIANT --

   procedure Put_VARIANT
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Index_Array;
      Value    : in     GNATCOM.Types.VARIANT;
      Clear    : in     Boolean                 := True)
   is
      type Index_Array is array (Index'Range) of Interfaces.C.long;

      function SafeArrayPutElement
        (psa       : access GNATCOM.Types.SAFEARRAY;
         rgIndices : access Index_Array;
         pv        : access GNATCOM.Types.VARIANT)
        return GNATCOM.Types.HRESULT;
      pragma Import (StdCall, SafeArrayPutElement, "SafeArrayPutElement");

      SAIndex   : aliased Index_Array;
      SAElement : aliased GNATCOM.Types.VARIANT := Value;
   begin
      for N in Index'Range loop
         SAIndex (N) := Interfaces.C.long (Index (N));
      end loop;

      Error_Check
        (SafeArrayPutElement (Of_Array,
                              SAIndex'Access,
                              SAElement'Access));

      if Clear then
         GNATCOM.VARIANT.Free (Value);
      end if;
   end Put_VARIANT;

   -- Error_Check --

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT) is
   begin
      GNATCOM.Errors.Set_Last_HRESULT (Result);

      if GNATCOM.Errors.FAILED (Result) then
         case Result is
            when DISP_E_ARRAYISLOCKED =>
               raise ARRAY_LOCKED_ERROR;
            when DISP_E_BADINDEX =>
               raise INVALID_INDEX_ERROR;
            when others =>
               GNATCOM.Errors.Error_Check (Result);
         end case;
      end if;
   end Error_Check;

end GNATCOM.SafeArray;
