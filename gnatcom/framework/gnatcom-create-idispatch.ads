------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--            G N A T C O M . C R E A T E . I D I S P A T C H               --
--                                                                          --
--                                 S p e c                                  --
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

--  Reusable Implementation of IDispatch for dual interfaces

with GNATCOM.Create.COM_Interface;
with GNATCOM.Types;

package GNATCOM.Create.IDispatch is

   type IDispatch_Type
     (IID       : GNATCOM.Types.Pointer_To_GUID;
      LIB_IID   : GNATCOM.Types.Pointer_To_GUID;
      wVerMajor : Interfaces.C.unsigned_short;
      wVerMinor : Interfaces.C.unsigned_short)
   is
     new Ada.Finalization.Controlled with
      record
         Type_Information : aliased GNATCOM.Types.Pointer_To_ITypeInfo := null;
      end record;
   type Pointer_To_IDispatch_Type is access all IDispatch_Type;
   --  A member of this type should be added to the COM objects data.
   --  It holds a pointer to the type information for the dual interface.
   --  The type information is used to dispatch calls to the IDispatch
   --  calls to the regular interface implementations.

   procedure Initialize (This : in out IDispatch_Type);
   procedure Finalize (This : in out IDispatch_Type);
   procedure Adjust (This : in out IDispatch_Type);

   --  These functions should be called in the functions of the dual
   --  interface of the same name.
   --
   --  There can be multiple implementations of dual interfaces using
   --  this method, just create mutliple members of IDispatch_Type for
   --  each interface and insure to pass the correct IDispatch_Type
   --  that corresponds to that interface.
   --  Scripting languages like JScript and VBScript will only have access
   --  to the dual interface marked as [default] in the IDL and only that
   --  interface should be in the IID map for the object for IDispatch.

   function GetTypeInfoCount (pctinfo : GNATCOM.Types.Pointer_To_unsigned)
                             return GNATCOM.Types.HRESULT;

   function GetTypeInfo
     (Data    : access IDispatch_Type;
      itinfo  : in     Interfaces.C.unsigned;
      pptinfo : in     GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;

   function GetIDsOfNames
     (Data      : access IDispatch_Type;
      rgszNames : in     GNATCOM.Types.Pointer_To_Pointer_To_char;
      cNames    : in     Interfaces.C.unsigned;
      rgdispid  : in     GNATCOM.Types.Pointer_To_long)
     return GNATCOM.Types.HRESULT;

   function Invoke
     (This         : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Data         : access IDispatch_Type;
      dispidMember : in     Interfaces.C.long;
      wFlags       : in     Interfaces.C.unsigned_short;
      pdispparams  : in     GNATCOM.Types.Pointer_To_DISPPARAMS;
      pvarResult   : in     GNATCOM.Types.Pointer_To_VARIANT;
      pexcepinfo   : in     GNATCOM.Types.Pointer_To_EXCEPINFO;
      puArgErr     : in     GNATCOM.Types.Pointer_To_unsigned)
     return GNATCOM.Types.HRESULT;
   --  pragma Convention (StdCall, Invoke);
   --  ^ This commented-out pragma is of no known use in any
   --    component or sample of GNATCOM and GWindows.
   --    Probably it was originally left by mistake when
   --    DispInvoke's profile (see Invoke's body) was copied to Invoke's
   --    specification.
   --    It causes the following error on GNAT GPL 2013:
   --       dispatching subprogram at line 89 cannot use Stdcall convention

end GNATCOM.Create.IDispatch;
