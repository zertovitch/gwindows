------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                      G N A T C O M . E V E N T S                         --
--                                                                          --
--                                B o d y                                   --
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

with GNATCOM.Errors;

package body GNATCOM.Events is

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT);

   -- Advise --

   procedure Advise
     (This            : in out IConnectionPoint_Type;
      Event_Interface :
        access  GNATCOM.Create.COM_Interface.COM_Interface_Type)
   is
   begin
      Advise (This, Event_Interface.all'Address);
   end Advise;

   -- Advise --

   procedure Advise
     (This            : in out IConnectionPoint_Type;
      Event_Interface : in     System.Address)
   is
      Sink : GNATCOM.Iinterface.Interface_Type;
   begin
      GNATCOM.Iinterface.Attach (Sink, Event_Interface);
      GNATCOM.Iinterface.AddRef (Sink);

      Error_Check
        (Pointer (This).Vtbl.Advise (Pointer (This),
                                     GNATCOM.Iinterface.Pointer (Sink),
                                     This.Cookie'Unchecked_Access));
   end Advise;

   -- FindConnectionPoint --

   function FindConnectionPoint
     (This      : IConnectionPointContainer_Type;
      Event_IID : GNATCOM.Types.GUID)
      return GNATCOM.Types.Pointer_To_IConnectionPoint
   is
      Connection : aliased GNATCOM.Types.Pointer_To_IConnectionPoint;
      New_GUID   : aliased GNATCOM.Types.GUID := Event_IID;
   begin
      Error_Check
        (Pointer (This).Vtbl.FindConnectionPoint
         (Pointer (This),
          New_GUID'Unchecked_Access,
          Connection'Unchecked_Access));
      return Connection;
   end FindConnectionPoint;

   -- Initialize --

   procedure Initialize (This : in out IConnectionPointContainer_Type) is
   begin
      Set_IID (This, GNATCOM.Types.IID_IConnectionPointContainer);
   end Initialize;

   -- Initialize --

   procedure Initialize (This : in out IConnectionPoint_Type) is
   begin
      Set_IID (This, GNATCOM.Types.IID_IConnectionPoint);
   end Initialize;

   -- Pointer --

   function Pointer
     (This : in IConnectionPoint_Type)
      return GNATCOM.Types.Pointer_To_IConnectionPoint
   is
   begin
      return To_Pointer_To_IConnectionPoint (Address (This));
   end Pointer;

   -- Pointer --

   function Pointer
     (This : in IConnectionPointContainer_Type)
      return GNATCOM.Types.Pointer_To_IConnectionPointContainer
   is
   begin
      return To_Pointer_To_IConnectionPointContainer (Address (This));
   end Pointer;

   -- Set_Events --

   procedure Set_Events
     (This            : in out IConnectionPoint_Type;
      For_Object      : in     GNATCOM.Iinterface.Interface_Type'Class;
      Event_IID       : in     GNATCOM.Types.GUID;
      Event_Interface :
        access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Free            : Boolean := True)
   is
      Container : IConnectionPointContainer_Type;
   begin
      Query (Container, For_Object);
      Attach (This, FindConnectionPoint
              (Container,
               Event_IID => Set_Events.Event_IID).all'Address);
      Advise (This, Event_Interface);

      if Free then
         GNATCOM.Create.COM_Interface.Release (Event_Interface);
      end if;
   end Set_Events;

   -- Set_Events --

   procedure Set_Events
     (This            : in out IConnectionPoint_Type;
      For_Object      : in     GNATCOM.Iinterface.Interface_Type'Class;
      Event_IID       : in     GNATCOM.Types.GUID;
      Event_Interface : in     System.Address)
   is
      Container : IConnectionPointContainer_Type;
   begin
      Query (Container, For_Object);
      Attach (This, FindConnectionPoint
              (Container,
               Event_IID => Set_Events.Event_IID).all'Address);
      Advise (This, Event_Interface);
   end Set_Events;

   -- Unadvise --

   procedure Unadvise (This : in out IConnectionPoint_Type) is
   begin
      Error_Check
        (Pointer (This).Vtbl.Unadvise (Pointer (This),
                                       This.Cookie));
   end Unadvise;

   -- Error_Check --

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT) is
   begin
      if GNATCOM.Errors.FAILED (Result) then
         case Result is
            when CONNECT_E_ADVISELIMIT =>
               raise ADVISE_LIMIT_ERROR;
            when CONNECT_E_CANNOTCONNECT =>
               raise INCORRECT_INTERFACE_ERROR;
            when CONNECT_E_NOCONNETION =>
               raise NO_PREVIOUS_ADVISE_ERROR;
            when others =>
               GNATCOM.Errors.Error_Check (Result);
         end case;
      end if;
   end Error_Check;

end GNATCOM.Events;
