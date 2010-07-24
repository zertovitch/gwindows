------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--     G N A T C O M . I C R E A T E T Y P E L I B _ I N T E R F A C E      --
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
with System;
with Interfaces.C;

with GNATCOM.Iinterface;
with GNATCOM.Types;

package GNATCOM.ICreateTypeLib_Interface is

   type ICreateTypeLib_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out ICreateTypeLib_Type);

   function Pointer (This : ICreateTypeLib_Type)
     return GNATCOM.Types.Pointer_To_ICreateTypeLib;

   function To_Pointer_To_ICreateTypeLib is new Ada.Unchecked_Conversion
     (System.Address, GNATCOM.Types.Pointer_To_ICreateTypeLib);

   procedure Attach (This    : in out ICreateTypeLib_Type;
                     Pointer : in     GNATCOM.Types.Pointer_To_ICreateTypeLib);

   procedure Create_Type_Library
     (This      : in out ICreateTypeLib_Type;
      File_Name : in     String);
   --  This function creates an implementation for ICreateTypeLib that will
   --  write out a Type Library to File_Name

   procedure CreateTypeInfo
     (This     : ICreateTypeLib_Type;
      szName   : GNATCOM.Types.LPWSTR;
      tkind    : GNATCOM.Types.TYPEKIND;
      ppCTInfo : GNATCOM.Types.Pointer_To_Pointer_To_ICreateTypeInfo);

   procedure SetName
     (This   : ICreateTypeLib_Type;
      szName : GNATCOM.Types.LPWSTR);

   procedure SetVersion
     (This         : ICreateTypeLib_Type;
      wMajorVerNum : Interfaces.C.unsigned_short;
      wMinorVerNum : Interfaces.C.unsigned_short);

   procedure SetGuid
     (This : ICreateTypeLib_Type;
      guid : GNATCOM.Types.Pointer_To_GUID);

   procedure SetDocString
     (This  : ICreateTypeLib_Type;
      szDoc : GNATCOM.Types.LPWSTR);

   procedure SetHelpFileName
     (This           : ICreateTypeLib_Type;
      szHelpFileName : GNATCOM.Types.LPWSTR);

   procedure SetHelpContext
     (This          : ICreateTypeLib_Type;
      dwHelpContext : Interfaces.C.unsigned_long);

   procedure SetLcid
     (This : ICreateTypeLib_Type;
      lcid : Interfaces.C.unsigned_long);

   procedure SetLibFlags
     (This      : ICreateTypeLib_Type;
      uLibFlags : Interfaces.C.unsigned);

   procedure SaveAllChanges
     (This : ICreateTypeLib_Type);

end GNATCOM.ICreateTypeLib_Interface;
