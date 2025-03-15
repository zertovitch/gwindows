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

--  Provides implementation of the Local Server (the EXE) that will host
--  the COM objects

with Ada.Strings.Unbounded;

with GNATCOM.Create.Factory;
with GNATCOM.Types;

package GNATCOM.Create.Local_Server is

   procedure Init_Object (LIBID : in GNATCOM.Types.GUID; Run : Run_Mode);
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
         Implemented_Categories : GNATCOM.Types.GUID_Array_Pointer := null;
         Service_Name : Ada.Strings.Unbounded.Unbounded_String :=
           Ada.Strings.Unbounded.Null_Unbounded_String;
         APPID       : aliased GNATCOM.Types.GUID := GNATCOM.Types.GUID_NULL;
         dwRegisterROT : aliased GNATCOM.Types.DWORD := 0;
      end record;
   --  Map for CLSIDs to COM object creation infromation and functions

   type Factory_Record_Array is array (Natural range <>) of Factory_Record;
   type Factory_Record_Array_Pointer is access all Factory_Record_Array;

   Factory_Map : Factory_Record_Array_Pointer;
   --  Pointer to map of COM objects in container

   ALREADY_REGISTERED_ERROR : exception;
   --  Object has already been registered and can not be registered again

end GNATCOM.Create.Local_Server;
