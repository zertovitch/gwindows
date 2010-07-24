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
   pragma Convention (StdCall, Invoke);

end GNATCOM.Create.IDispatch;
