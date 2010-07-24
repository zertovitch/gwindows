------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--         G N A T C O M . E V E N T S . E V E N T _ O B J E C T            --
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

with GNATCOM.Types;
with GNATCOM.Create.COM_Interface;
with Interfaces.C;

package GNATCOM.Events.Event_Object is

   type Event_Type is tagged null record;
   type Event_Pointer is access all Event_Type'Class;
   --  Allows association of an Ada object with a created
   --  Event_Object. The associated object is passed in
   --  to the call back Invoke_Function

   type Invoke_Function is access
     procedure (dispidMember : in Interfaces.C.long;
                wFlags       : in Interfaces.C.unsigned_short;
                pdispparams  : in GNATCOM.Types.Pointer_To_DISPPARAMS;
                Event_Object : in Event_Pointer);
   --  Definition of the call back function

   function Create (Invoke       : Invoke_Function;
                    Event_IID    : GNATCOM.Types.GUID;
                    Event_Object : Event_Pointer := null)
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type;
   --  Creates a COM object that implements IDispatch and calls the
   --  call back function to handle IDispatch invokes
end GNATCOM.Events.Event_Object;
