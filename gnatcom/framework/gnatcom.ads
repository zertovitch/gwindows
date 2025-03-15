------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                              G N A T C O M                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2018 David Botton                   --
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

--  GNATCOM is a collection of packages for use in interfacing Ada with Type
--  Libraries, COM+/COM/DCOM Objects, ActiveX, Automation and OLE

with Ada.Finalization;
with Interfaces;

package GNATCOM is
   pragma Linker_Options ("-lole32");
   pragma Linker_Options ("-loleaut32");
   pragma Linker_Options ("-luser32");

   --  Commonly used COM constants

   S_OK                      : constant := 0;
   S_FALSE                   : constant := 1;

   E_NOINTERFACE             : constant := 16#80004002#;
   E_FAIL                    : constant := 16#80004005#;
   E_NOTIMPL                 : constant := 16#80004001#;
   E_OUTOFMEMORY             : constant := 16#8007000e#;
   E_INVALIDARG              : constant := 16#80070057#;
   E_POINTER                 : constant := 16#80004003#;
   E_ABORT                   : constant := 16#80004004#;
   E_ACCESSDENIED            : constant := 16#80070005#;
   E_UNEXPECTED              : constant := 16#8000ffff#;
   E_BOUNDS                  : constant := 16#8000000B#;

   DISP_E_TYPEMISMATCH       : constant := 16#80020005#;
   DISP_E_BADVARTYPE         : constant := 16#80020008#;
   DISP_E_OVERFLOW           : constant := 16#8002000a#;
   DISP_E_BADINDEX           : constant := 16#8002000b#;
   DISP_E_ARRAYISLOCKED      : constant := 16#8002000d#;
   DISP_E_UNKNOWNNAME        : constant := 16#80020006#;
   DISP_E_UNKNOWNLCID        : constant := 16#8002000c#;
   DISP_E_PARAMNOTOPTIONAL   : constant := 16#8002000f#;
   DISP_E_MEMBERNOTFOUND     : constant := 16#80020003#;
   DISP_E_PARAMNOTFOUND      : constant := 16#80020004#;
   DISP_E_NONAMEDARGS        : constant := 16#80020007#;
   DISP_E_BADPARAMCOUNT      : constant := 16#8002000e#;

   TYPE_E_ELEMENTNOTFOUND    : constant := 16#8002802b#;
   TYPE_E_IOERROR            : constant := 16#80028ca2#;
   TYPE_E_INVALIDSTATE       : constant := 16#80028029#;
   TYPE_E_INVDATAREAD        : constant := 16#80028018#;
   TYPE_E_UNSUPFORMAT        : constant := 16#80028019#;
   TYPE_E_UNKNOWNLCID        : constant := 16#8002802e#;
   TYPE_E_CANTLOADLIBRARY    : constant := 16#80029c4a#;
   TYPE_E_REGISTRYACCESS     : constant := 16#8002801c#;

   REGDB_E_CLASSNOTREG       : constant := 16#80040154#;

   CLASS_E_CLASSNOTAVAILABLE : constant := 16#80040111#;
   CLASS_E_CLASSNOTLICENSED  : constant := 16#80040112#;

   CO_E_CLASSSTRING          : constant := 16#800401f3#;
   CO_E_APPNOTFOUND          : constant := 16#800401f5#;
   CO_E_DLLNOTFOUND          : constant := 16#800401f8#;
   CO_E_ERRORINDLL           : constant := 16#800401f9#;
   CO_E_APPDIDNTREG          : constant := 16#800401fe#;
   CO_E_OBJNOTCONNECTED      : constant := 16#800401FD#;
   CO_E_OBJISREG             : constant := 16#800401fc#;

   DB_E_ERRORSINCOMMAND      : constant := 16#80040E14#;

   CONNECT_E_NOCONNETION     : constant := 16#80040200#;
   CONNECT_E_ADVISELIMIT     : constant := 16#80040201#;
   CONNECT_E_CANNOTCONNECT   : constant := 16#80040202#;
   CONNECT_E_OVERRIDDEN      : constant := 16#80040203#;

private
   Initialize_Count : aliased Interfaces.Unsigned_32 := 0;

   type COM_Uninitialize_Type is
     new Ada.Finalization.Controlled with null record;

   overriding
   procedure Finalize (This : in out COM_Uninitialize_Type);

   Main_Program_Uninitialize : COM_Uninitialize_Type;
   --  In order to insure the CoUnitialize is called after any controlled
   --  objects with pointers to interfaces have already finalized, GNATCOM
   --  creates COM_Unitialize_Type in this main package that will be
   --  elaborated before any other. If the Initialize_Count is greater
   --  then zero then CoUnititialize is called.
   --
   --  A GNATCOM.Initialize.Unitialize_COM matching the first COM
   --  initialization in the main thread should not be called

end GNATCOM;
