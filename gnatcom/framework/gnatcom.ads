------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                              G N A T C O M                               --
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

--  GNATCOM is a collection of packages for use in interfacing Ada with Type
--  Libraries, COM+/COM/DCOM Objects, ActiveX, Automation and OLE

with Ada.Finalization;

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

   CONNECT_E_NOCONNETION     : constant := 16#80040200#;
   CONNECT_E_ADVISELIMIT     : constant := 16#80040201#;
   CONNECT_E_CANNOTCONNECT   : constant := 16#80040202#;
   CONNECT_E_OVERRIDDEN      : constant := 16#80040203#;

private
   Initialize_Count : aliased Integer := 0;

   type COM_Uninitialize_Type is
     new Ada.Finalization.Controlled with null record;

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
