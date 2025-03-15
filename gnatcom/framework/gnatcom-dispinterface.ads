------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                G N A T C O M . D I S P I N T E R F A C E                 --
--                                                                          --
--                                 S p e c                                  --
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

--  The Dispinterface_Type encapsulates the functionality of IDispatch
--  creating a thick binding to COM Dispatch Interfaces (dispinterfaces)

--  One of the COM initialization methods must be called first before
--  using any COM Interface wrapper

with Ada.Unchecked_Conversion;
with System;
with Interfaces.C;

with GNATCOM.Types;
with GNATCOM.Iinterface;

package GNATCOM.Dispinterface is

   type Dispinterface_Type is new GNATCOM.Iinterface.Interface_Type
     with null record;

   procedure Attach
     (This : in out Dispinterface_Type;
      From : in     GNATCOM.Types.Pointer_To_IDispatch);
   --  Attaches a disinterface to a dispnterface_Type. AddRef is not called.

   procedure Attach
     (This : in out Dispinterface_Type;
      From : in     GNATCOM.Types.VARIANT);
   --  Attaches a Dispatch interface contained in a VARINT to a
   --  Dispinterface_Type. AddRef is not called.

   function Pointer (This : Dispinterface_Type)
     return GNATCOM.Types.Pointer_To_IDispatch;
   --  Returns the internal interface pointer

   procedure Initialize (This : in out Dispinterface_Type);
   --  Called by runtime of controlled objects
   --  Initializes IID to IDispatch

   function To_Pointer_To_IDispatch is
     new Ada.Unchecked_Conversion
     (System.Address, GNATCOM.Types.Pointer_To_IDispatch);

   function To_VARIANT_From_Dispinterface (From : Dispinterface_Type)
     return GNATCOM.Types.VARIANT;
   --  Returns a variant containing and IDispatch Pointer to This
   --  Calls Addref. VARIANT should be destroyed when no longer needed.

   function Has_Type_Info (This : Dispinterface_Type) return Boolean;
   --  Returns true if there is a ITypeInfo object available for this
   --  dispinterface

   function Get_Type_Info
     (This : Dispinterface_Type;
      LCID : Interfaces.C.long  := 0)
      return GNATCOM.Types.Pointer_To_ITypeInfo;
   --  Retrieves the type information for an object, which can then be used
   --  to get the type information for an interface.

   function Get_DISPID
     (This    : Dispinterface_Type;
      Of_Name : String)
     return Interfaces.C.long;
   --  Retrieve the DISPID Of_Name in object

   type Parameter_Array is array (Positive range <>) of GNATCOM.Types.VARIANT;
   --  Used to create an array of parameters in right to left order.
   --  IDispatch::Invoke uses VB/FORTRAN right to left

   procedure Put
     (This   : in Dispinterface_Type;
      Name   : in String;
      Value  : in GNATCOM.Types.VARIANT;
      Free   : in Boolean               := True;
      LCID   : in Interfaces.C.long     := 0);
   procedure Put
     (This   : in Dispinterface_Type;
      DISPID : in Interfaces.C.long;
      Value  : in GNATCOM.Types.VARIANT;
      Free   : in Boolean               := True;
      LCID   : in Interfaces.C.long     := 0);
   --  Invoke a member as a put
   --  If Free then Free Variants after put

   procedure Put
     (This        : in Dispinterface_Type;
      Name        : in String;
      Value       : in GNATCOM.Types.VARIANT;
      Index_Value : in GNATCOM.Types.VARIANT;
      Free        : in Boolean               := True;
      LCID        : in Interfaces.C.long     := 0);
   procedure Put
     (This        : in Dispinterface_Type;
      DISPID      : in Interfaces.C.long;
      Value       : in GNATCOM.Types.VARIANT;
      Index_Value : in GNATCOM.Types.VARIANT;
      Free        : in Boolean               := True;
      LCID        : in Interfaces.C.long     := 0);
   --  Invoke a member as a put on a collection indexed by Index_Value
   --  If Free then Free Variants after put

   procedure Put
     (This         : in Dispinterface_Type;
      Name         : in String;
      Value        : in GNATCOM.Types.VARIANT;
      Index_Values : in Parameter_Array;
      Free         : in Boolean               := True;
      LCID         : in Interfaces.C.long     := 0);
   procedure Put
     (This         : in Dispinterface_Type;
      DISPID       : in Interfaces.C.long;
      Value        : in GNATCOM.Types.VARIANT;
      Index_Values : in Parameter_Array;
      Free         : in Boolean               := True;
      LCID         : in Interfaces.C.long     := 0);
   --  Invoke a member as a get on a multi-dimensional collection indexed by
   --  Index_Value. Index_Values are passed in column-major order, ie, right
   --  to left order (1,2,3) = IndexValues(1) = 3, IndexValues(2) = 2 ...
   --  If Free then Free Variants after put

   procedure Put
     (This       : in Dispinterface_Type;
      Name       : in String;
      Parameters : in Parameter_Array;
      Free       : in Boolean            := True;
      LCID       : in Interfaces.C.long  := 0);
   procedure Put
     (This       : in Dispinterface_Type;
      DISPID     : in Interfaces.C.long;
      Parameters : in Parameter_Array;
      Free       : in Boolean            := True;
      LCID       : in Interfaces.C.long  := 0);
   --  Invoke a member as a put with variable number of parameters
   --  If Free then Free Variants after put

   procedure PutRef
     (This   : in Dispinterface_Type;
      Name   : in String;
      Value  : in GNATCOM.Types.VARIANT;
      LCID   : in Interfaces.C.long     := 0);
   procedure PutRef
     (This   : in Dispinterface_Type;
      DISPID : in Interfaces.C.long;
      Value  : in GNATCOM.Types.VARIANT;
      LCID   : in Interfaces.C.long     := 0);
   --  Invoke a member as a putref

   procedure PutRef
     (This       : in Dispinterface_Type;
      Name       : in String;
      Parameters : in Parameter_Array;
      LCID       : in Interfaces.C.long  := 0);
   procedure PutRef
     (This       : in Dispinterface_Type;
      DISPID     : in Interfaces.C.long;
      Parameters : in Parameter_Array;
      LCID       : in Interfaces.C.long  := 0);
   --  Invoke a member as a putref with multiple paramters

   function Get
     (This   : Dispinterface_Type;
      Name   : String;
      LCID   : Interfaces.C.long  := 0)
      return GNATCOM.Types.VARIANT;
   function Get
     (This   : Dispinterface_Type;
      DISPID : Interfaces.C.long;
      LCID   : Interfaces.C.long  := 0)
      return GNATCOM.Types.VARIANT;
   --  Invoke a member as a get

   function Get
     (This        : Dispinterface_Type;
      Name        : String;
      Index_Value : GNATCOM.Types.VARIANT;
      Free        : Boolean               := True;
      LCID        : Interfaces.C.long     := 0)
      return GNATCOM.Types.VARIANT;
   function Get
     (This        : Dispinterface_Type;
      DISPID      : Interfaces.C.long;
      Index_Value : GNATCOM.Types.VARIANT;
      Free        : Boolean               := True;
      LCID        : Interfaces.C.long     := 0)
      return GNATCOM.Types.VARIANT;
   --  Invoke a member as a get on a collection indexed by Index_Value

   function Get
     (This         : Dispinterface_Type;
      Name         : String;
      Index_Values : Parameter_Array;
      Free         : Boolean            := True;
      LCID         : Interfaces.C.long  := 0)
      return GNATCOM.Types.VARIANT;
   function Get
     (This         : Dispinterface_Type;
      DISPID       : Interfaces.C.long;
      Index_Values : Parameter_Array;
      Free         : Boolean            := True;
      LCID         : Interfaces.C.long  := 0)
      return GNATCOM.Types.VARIANT;
   --  Invoke a member as a get on a multi-dimensional collection indexed by
   --  Index_Value. Index_Values are passed in column-major order, ie, right
   --  to left order (1,2,3) = IndexValues(1) = 3, IndexValues(2) = 2 ...

   function Invoke
     (This       : Dispinterface_Type;
      Name       : String;
      Parameters : Parameter_Array;
      Free       : Boolean            := True;
      LCID       : Interfaces.C.long  := 0)
      return  GNATCOM.Types.VARIANT;
   function Invoke
     (This       : Dispinterface_Type;
      DISPID     : Interfaces.C.long;
      Parameters : Parameter_Array;
      Free       : Boolean            := True;
      LCID       : Interfaces.C.long  := 0)
      return  GNATCOM.Types.VARIANT;
   --  Invoke a member as a method
   --  If Free then Free Variants after put

   procedure Invoke
     (This       : in Dispinterface_Type;
      Name       : in String;
      Parameters : in Parameter_Array;
      Free       : in Boolean            := True;
      LCID       : in Interfaces.C.long  := 0);
   procedure Invoke
     (This       : in Dispinterface_Type;
      DISPID     : in Interfaces.C.long;
      Parameters : in Parameter_Array;
      Free       : in Boolean            := True;
      LCID       : in Interfaces.C.long  := 0);
   --  Invoke a member as a method
   --  If Free then Free Variants after put

   function Invoke
     (This   : Dispinterface_Type;
      Name   : String;
      LCID   : Interfaces.C.long  := 0)
      return  GNATCOM.Types.VARIANT;
   function Invoke
     (This   : Dispinterface_Type;
      DISPID : Interfaces.C.long;
      LCID   : Interfaces.C.long  := 0)
      return  GNATCOM.Types.VARIANT;
   --  Invoke a member as a method

   procedure Invoke
     (This   : in Dispinterface_Type;
      Name   : in String;
      LCID   : in Interfaces.C.long  := 0);
   procedure Invoke
     (This   : in Dispinterface_Type;
      DISPID : in Interfaces.C.long;
      LCID   : in Interfaces.C.long  := 0);
   --  Invoke a member as a method

   function Invoke
     (This       : Dispinterface_Type;
      Name       : String;
      wFlags     : Interfaces.C.short;
      Parameters : Parameter_Array;
      Free       : Boolean            := True;
      LCID       : Interfaces.C.long  := 0)
      return  GNATCOM.Types.VARIANT;
   function Invoke
     (This       : Dispinterface_Type;
      DISPID     : Interfaces.C.long;
      wFlags     : Interfaces.C.short;
      Parameters : Parameter_Array;
      Free       : Boolean            := True;
      LCID       : Interfaces.C.long  := 0)
      return  GNATCOM.Types.VARIANT;
   --  Generic invoke of a member of the dispinterface
   --  If Free then Free Variants after put

   INVOKE_ERROR : exception;
   --  Raised when the IDispatch Invoke method returns with an error

   PARAMETER_ERROR : exception;
   --  Raised when the wrong number of paramaters are passed in

   TYPE_MISMATCH_ERROR : exception;
   --  Raised when there is a type mismatch in the arguments passed in

   UNKNOWN_NAME_ERROR : exception;
   --  Raised when there is no matching get/put/invoke in the object

   UNKNOWN_LCID_ERROR : exception;
   --  Raised when an unknown local ID is passed in

   ELEMENT_NOT_FOUND_ERROR : exception;
   --  Raised when a request for an TypeInfo can not be fulfilled

end GNATCOM.Dispinterface;
