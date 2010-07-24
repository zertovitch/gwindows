------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                      G N A T C O M . E V E N T S                         --
--                                                                          --
--                                S p e c                                   --
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
with Interfaces.C;
with System;

with GNATCOM.Iinterface;
with GNATCOM.Types;
with GNATCOM.Create.COM_Interface;

package GNATCOM.Events is

   type IConnectionPoint_Type is new GNATCOM.Iinterface.Interface_Type with
      record
         Cookie : aliased Interfaces.C.unsigned_long := 0;
      end record;
   --  Interface to an event connection point. This is usually retrieved
   --  using an IConnectionPointContainer. For convenience a function
   --  is provided that automaticly creates an IConnectionPointContainer
   --  and gets a connection point for a particular event interface, i.e.
   --  Set_Events.

   procedure Advise
     (This            : in out IConnectionPoint_Type;
      Event_Interface :
        access  GNATCOM.Create.COM_Interface.COM_Interface_Type);
   procedure Advise
     (This            : in out IConnectionPoint_Type;
      Event_Interface : in     System.Address);
   --  Set an event handler for this Connection Point

   procedure Unadvise (This : in out IConnectionPoint_Type);
   --  Release the event handler for this Connection Point

   procedure Set_Events
     (This            : in out IConnectionPoint_Type;
      For_Object      : in     GNATCOM.Iinterface.Interface_Type'Class;
      Event_IID       : in     GNATCOM.Types.GUID;
      Event_Interface :
        access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Free            : Boolean := True);
   --  Sets up events for an object that supports Connection Points.
   --  It requests an IConnectionPointContainer then does a FindConnectionPoint
   --  for the Event_IID following that it does and advise with the
   --  Event_Interface
   --  If Free = True then Release is called on the Event_Interface after
   --  establishing the connection.

   procedure Set_Events
     (This            : in out IConnectionPoint_Type;
      For_Object      : in     GNATCOM.Iinterface.Interface_Type'Class;
      Event_IID       : in     GNATCOM.Types.GUID;
      Event_Interface : in     System.Address);
   --  Sets up events for an object that supports Connection Points.
   --  It requests an IConnectionPointContainer then does a FindConnectionPoint
   --  for the Event_IID following that it does and advise with the
   --  Event_Interface

   type IConnectionPointContainer_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   function FindConnectionPoint
     (This      : IConnectionPointContainer_Type;
      Event_IID : GNATCOM.Types.GUID)
     return GNATCOM.Types.Pointer_To_IConnectionPoint;
   --  Returns a IConnectionPoint that creates connections for
   --  Event_IID

   ADVISE_LIMIT_ERROR : exception;
   --  Raised when the COM object can no longer accept additional advise
   --  connections to receive events

   INCORRECT_INTERFACE_ERROR : exception;
   --  The interface passed in does not suport an interface that can be used
   --  for events to this object

   NO_PREVIOUS_ADVISE_ERROR : exception;
   --  An Unadvise was called, but an advise was not called previously

   --  Low level interfaces

   procedure Initialize (This : in out IConnectionPoint_Type);
   --  Set the IID to IConnectionPoint

   function Pointer (This : in IConnectionPoint_Type)
     return GNATCOM.Types.Pointer_To_IConnectionPoint;
   --  Returns a pointer to internal IConnectionPoint interface

   function To_Pointer_To_IConnectionPoint is
      new Ada.Unchecked_Conversion (System.Address,
                                    GNATCOM.Types.Pointer_To_IConnectionPoint);

   procedure Initialize (This : in out IConnectionPointContainer_Type);
   --  Set the IID to IConnectionPointContainer

   function Pointer
     (This : in IConnectionPointContainer_Type)
     return GNATCOM.Types.Pointer_To_IConnectionPointContainer;
   --  Returns a pointer to the internal IConnectionPointerContainer

   function To_Pointer_To_IConnectionPointContainer is
      new Ada.Unchecked_Conversion
     (System.Address,
      GNATCOM.Types.Pointer_To_IConnectionPointContainer);

end GNATCOM.Events;
