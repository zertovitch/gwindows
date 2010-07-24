------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--         G N A T C O M . C R E A T E . L O C A L _ S E R V E R            --
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

--  Provides implementation of the Local Server (the EXE) that will host
--  the COM objects

with Ada.Strings.Unbounded;

with GNATCOM.Create.Factory;
with GNATCOM.Types;

package GNATCOM.Create.Local_Server is

   procedure Init_Object (LIBID : in GNATCOM.Types.GUID);
   --  Initialize Object containter paramters

   type Factory_Record is
      record
         CLSID       : aliased GNATCOM.Types.GUID;
         Create      : GNATCOM.Create.Factory.Creation_Function;
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Version     : Ada.Strings.Unbounded.Unbounded_String;
         Description : Ada.Strings.Unbounded.Unbounded_String;
         pFactory    : GNATCOM.Create.Factory.Pointer_To_IClassFactory := null;
         dwRegister  : aliased Interfaces.C.unsigned_long := 0;
      end record;
   --  Map for CLSIDs to COM object creation infromation and functions

   type Factory_Record_Array is array (Natural range <>) of Factory_Record;
   type Factory_Record_Array_Pointer is access all Factory_Record_Array;

   Factory_Map : Factory_Record_Array_Pointer;
   --  Pointer to map of COM objects in container

   ALREADY_REGISTERED_ERROR : exception;
   --  Object has already been registered and can not be registered again

end GNATCOM.Create.Local_Server;
