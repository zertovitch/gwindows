------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                    G N A T C O M . S A F E A R R A Y                     --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
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

--  Binding to SAFEARRAYs, OLE Automation Arrays

with GNATCOM.Types;

package GNATCOM.SafeArray is

   type Bound is record
      Lower_Bound : Integer;
      Elements    : Natural;
   end record;

   type SafeArray_Bounds is array (Positive range <>) of Bound;

   type Index_Array is array (Positive range <>) of Integer;
   --  Indexes are passed in from right to left, ie. FORTRAN style

   function Create (VT          : GNATCOM.Types.VARTYPE;
                    Lower_Bound : Integer;
                    Elements    : Natural)
                   return GNATCOM.Types.Pointer_To_SAFEARRAY;
   --  Creates a one dimensional SAFEARRAY of range Lower_Bound ..
   --  Lower_Bound + Elements
   --  Note:
   --    VT Can not have VT_ARRAY or VT_BYREF flags set. It also may not be
   --  VT_NULL or VT_EMPTY

   function Create (VT     : GNATCOM.Types.VARTYPE;
                    Bounds : SafeArray_Bounds)
                   return GNATCOM.Types.Pointer_To_SAFEARRAY;
   --  Creates single or multi dimensional SAFEARRAYs using bounds
   --  Note:
   --    VT Can not have VT_ARRAY or VT_BYREF flags set. It also may not be
   --  VT_NULL or VT_EMPTY

   function Create
     (Lib_ID      : GNATCOM.Types.GUID;
      Ver_Maj     : Natural;
      Ver_Min     : Natural;
      Type_GUID   : GNATCOM.Types.GUID;
      Lower_Bound : Integer;
      Elements    : Natural)
     return GNATCOM.Types.Pointer_To_SAFEARRAY;
   function Create
     (Lib_ID  : GNATCOM.Types.GUID;
      Ver_Maj : Natural;
      Ver_Min : Natural;
      Type_GUID   : GNATCOM.Types.GUID;
      Bounds  : SafeArray_Bounds)
     return GNATCOM.Types.Pointer_To_SAFEARRAY;
   --  Creates a SAFEARRAY of VT_RECORDs of custom type with GUID in
   --  Type Library with Lib_ID and version Ver_Maj.Ver_Min

   function Create
     (Lib_ID      : GNATCOM.Types.GUID;
      Ver_Maj     : Natural;
      Ver_Min     : Natural;
      Index       : Natural;
      Lower_Bound : Integer;
      Elements    : Natural)
     return GNATCOM.Types.Pointer_To_SAFEARRAY;
   function Create
     (Lib_ID  : GNATCOM.Types.GUID;
      Ver_Maj : Natural;
      Ver_Min : Natural;
      Index   : Natural;
      Bounds  : SafeArray_Bounds)
     return GNATCOM.Types.Pointer_To_SAFEARRAY;
   --  Creates a SAFEARRAY of VT_RECORDs of custom type at Index in
   --  Type Library with Lib_ID and version Ver_Maj.Ver_Min

   function Create
     (Type_Info   : GNATCOM.Types.Pointer_To_ITypeInfo;
      Lower_Bound : Integer;
      Elements    : Natural)
     return GNATCOM.Types.Pointer_To_SAFEARRAY;
   function Create
     (Type_Info : GNATCOM.Types.Pointer_To_ITypeInfo;
      Bounds    : SafeArray_Bounds)
     return GNATCOM.Types.Pointer_To_SAFEARRAY;
   --  Creates a SAFEARRAY of VT_RECORDs of custom
   --  type described by Type_Info

   function Create_Custom (Size_Of_Element : Positive;
                           Lower_Bound     : Integer;
                           Elements        : Natural)
                          return GNATCOM.Types.Pointer_To_SAFEARRAY;
   --  Creates a one dimensional SAFEARRAY of range Lower_Bound ..
   --  Lower_Bound + Elements of a custom type with Size_Of_Element bytes

   function Create_Custom (Size_Of_Element : Positive;
                           Bounds          : SafeArray_Bounds)
                          return GNATCOM.Types.Pointer_To_SAFEARRAY;
   --  Creates single or multi dimensional SAFEARRAYs using bounds
   --  of a custom type with Size_Of_Element bytes

   function Copy (From : access GNATCOM.Types.SAFEARRAY)
                 return GNATCOM.Types.Pointer_To_SAFEARRAY;
   --  Duplicates a SAFEARRAY
   --  Will make new duplicates of strings and Variants and will call AddRef
   --  on COM Interfaces passed to the new copy

   procedure Free (This : access GNATCOM.Types.SAFEARRAY);
   --  Clears and Deallocates the SAFEARRAY from memory
   --  Also clears internal Variants and Strings and calls Release on
   --  COM Interfaces

   function Get_Dimensions (Of_Array : access GNATCOM.Types.SAFEARRAY)
                           return Positive;
   --  Returns number of dimensions of a SAFEARRAY

   function Get_Lower_Bound (Of_Array  : access GNATCOM.Types.SAFEARRAY;
                             Dimension : in     Positive                := 1)
                            return Integer;
   --  Returns lower bound of array dimension

   function Get_Upper_Bound (Of_Array  : access GNATCOM.Types.SAFEARRAY;
                             Dimension : in     Positive                := 1)
                            return Integer;
   --  Returns upper bound of array dimension

   generic
      type Element is private;
   function Get_Element (Of_Array : access GNATCOM.Types.SAFEARRAY;
                         Index    : in     Integer)
                        return Element;
   --  Returns a copy of the element at index in the SAFEARRAY

   generic
      type Element is private;
   function Get_Value (Of_Array : access GNATCOM.Types.SAFEARRAY;
                       Index    : in     Index_Array)
                      return Element;
   --  Returns a copy of the element at index in the multi dimensional
   --  SAFEARRAY

   generic
      type Element is private;
   procedure Put_Element (Of_Array : access GNATCOM.Types.SAFEARRAY;
                          Index    : in     Integer;
                          Value    : in     Element);
   --  Puts element in to array (Not for use with VT_DISPATCH, VT_UNKNOWN,
   --  or VT_BSTR type arrays)

   generic
      type Element is private;
   procedure Put_Value (Of_Array : access GNATCOM.Types.SAFEARRAY;
                        Index    : in     Index_Array;
                        Value    : in     Element);
   --  Puts element in to a multi dimensional  array (Not for use with
   --  VT_DISPATCH, VT_UNKNOWN or VT_BSTR type arrays)

   procedure Put_IUnknown
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Integer;
      Value    : in     GNATCOM.Types.Pointer_To_IUnknown);

   procedure Put_IUnknown
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Index_Array;
      Value    : in     GNATCOM.Types.Pointer_To_IUnknown);
   --  Required to place IUnknown pointers in to an array

   procedure Put_IDispatch
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Integer;
      Value    : in     GNATCOM.Types.Pointer_To_IDispatch);

   procedure Put_IDispatch
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Index_Array;
      Value    : in     GNATCOM.Types.Pointer_To_IDispatch);
   --  Required to place IDispatch pointers in to an array

   procedure Put_BSTR
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Integer;
      Value    : in     GNATCOM.Types.BSTR;
      Clear    : in     Boolean                 := True);

   procedure Put_BSTR
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Index_Array;
      Value    : in     GNATCOM.Types.BSTR;
      Clear    : in     Boolean                 := True);
   --  Required to place BSTRs in an array

   procedure Put_VARIANT
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Integer;
      Value    : in     GNATCOM.Types.VARIANT;
      Clear    : in     Boolean                 := True);

   procedure Put_VARIANT
     (Of_Array : access GNATCOM.Types.SAFEARRAY;
      Index    : in     Index_Array;
      Value    : in     GNATCOM.Types.VARIANT;
      Clear    : in     Boolean                 := True);
   --  May be used to place VARIANTs in an array

   ARRAY_LOCKED_ERROR : exception;
   --  An operation is being attempted on a SAFEARRAY that has been locked

   ARRAY_CREATION_ERROR : exception;
   --  OS was unable to create array

   INVALID_INDEX_ERROR : exception;
   --  The requested index value is invalid for this array

end GNATCOM.SafeArray;
