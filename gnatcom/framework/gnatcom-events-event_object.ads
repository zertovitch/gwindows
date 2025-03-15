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
